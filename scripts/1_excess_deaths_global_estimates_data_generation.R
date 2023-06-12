# Step 1: import libraries ---------------------------------------

# Import libraries
library(tidyverse)
library(data.table)
library(lubridate)
library(countrycode)
library(readr)
library(anytime)
library(googlesheets4)
gs4_deauth() # This puts googlesheets4 into a de-authorized mode.

options(scipen=999)

# Step 2: import excess deaths data and convert into a daily time series ---------------------------------------

# Import excess deaths
excess_deaths_git <- "https://raw.githubusercontent.com/TheEconomist/covid-19-excess-deaths-tracker/master/output-data/excess-deaths/"
excess_deaths_source <- bind_rows(list(Weekly = read_csv(paste0(excess_deaths_git, "all_weekly_excess_deaths.csv")) %>% select(-region_code),
                                       Monthly = read_csv(paste0(excess_deaths_git, "all_monthly_excess_deaths.csv")) %>% select(-region_code),
                                       Quarterly = read_csv(paste0(excess_deaths_git, "all_quarterly_excess_deaths.csv")) %>% select(-region_code)),
                                  .id = "timeframe") %>% 
  filter(country == region & end_date <= today()) %>%
  select(timeframe, country, start_date, end_date, days, population, 
         total_deaths, covid_deaths, expected_deaths, excess_deaths, non_covid_deaths, covid_deaths_per_100k, excess_deaths_per_100k) %>% 
  group_by(country, start_date, end_date) %>% slice(1:1) %>% ungroup() %>% 
  pivot_longer(cols = c("start_date", "end_date"), names_to = "date_type", values_to = "date")

daily_excess_deaths <- expand.grid(date = seq(min(excess_deaths_source$date), as.Date(today()), by = "days"),
                                   country = unique(excess_deaths_source$country)) %>% 
  left_join(excess_deaths_source, by = c("country", "date")) %>% 
  mutate(iso3c = countrycode(country, "country.name", "iso3c")) %>% 
  mutate(iso3c = ifelse(country == 'Kosovo', 'KSV', iso3c)) %>%
  arrange(iso3c, date) %>% 
  group_by(iso3c) %>% tidyr::fill(timeframe:date_type) %>% 
  # The below line removes those entries that are after a "end date" but before a "start date" - original code only does this for trailing dates
  mutate_at(vars(days:excess_deaths_per_100k), list(~ifelse(date_type == "end_date" & lag(date_type) == "end_date", NA, .))) %>% ungroup() %>% 
  mutate_at(vars(total_deaths:excess_deaths_per_100k), list(~ . / days)) %>% 
  filter(date >= as.Date("2020-01-01") & !is.na(iso3c)) %>% 
  mutate_at(c("total_deaths", "expected_deaths", "excess_deaths"), list(per_100k = ~ . * 1e5 / population)) %>% 
  select(iso3c, date, total_deaths, total_deaths_per_100k, expected_deaths, expected_deaths_per_100k, excess_deaths, excess_deaths_per_100k) %>% 
  rename_at(vars(contains("_")), list(~paste0("daily_", .))) %>% 
  drop_na(-date, -iso3c) %>% 
  filter(!iso3c %in% c("ARM","AZE","ETH","MMR")) # Removes countries which have entered (possibly civil) wars

# Removing very recent data for countries with clear reporting lag issues (Malaysia) after discussions with our source for this data, as well as data that is less than two weeks old (also to avoid reporting lag issues):
daily_excess_deaths <- daily_excess_deaths[!(daily_excess_deaths$iso3c == "MYS" & daily_excess_deaths$date >= Sys.Date()-30*5) & daily_excess_deaths$date < Sys.Date()-2*7, ]

# Removing data from Ukraine and Russia war:
daily_excess_deaths <- daily_excess_deaths[!(daily_excess_deaths$iso3c == "RUS" & daily_excess_deaths$date >= as.Date("2022-02-24")), ]
daily_excess_deaths <- daily_excess_deaths[!(daily_excess_deaths$iso3c == "UKR" & daily_excess_deaths$date >= as.Date("2022-02-24")), ]

# Removing data from Syria and Turkey during the earthquake:
daily_excess_deaths <- daily_excess_deaths[!(daily_excess_deaths$iso3c == "TUR" & daily_excess_deaths$date >= as.Date("2023-02-01") & daily_excess_deaths$date >= as.Date("2023-04-01")), ]
daily_excess_deaths <- daily_excess_deaths[!(daily_excess_deaths$iso3c == "SYR" & daily_excess_deaths$date >= as.Date("2023-02-01") & daily_excess_deaths$date >= as.Date("2023-04-01")), ]


# Check to ensure and remove any duplicated country-dates
daily_excess_deaths <- daily_excess_deaths[!duplicated(paste0(daily_excess_deaths$iso3c, "_", daily_excess_deaths$date)), ]

# Step 3: import OWID data on testing and cases, creating a daily time series ---------------------------------------

# Import daily data for countries from Our World In Data
country_daily_data <- fread("https://raw.githubusercontent.com/owid/covid-19-data/master/public/data/owid-covid-data.csv") %>%
  mutate(date = as.Date(date),
         country = location,
         iso3c = iso_code,
         region = countrycode(iso3c, origin="iso3c",destination="un.region.name"),
         subregion = countrycode(iso3c, origin="iso3c",destination="un.regionsub.name"),
         daily_covid_deaths = new_deaths_smoothed,
         daily_covid_deaths_per_100k = (daily_covid_deaths / population) * 100000,
         daily_covid_cases = new_cases_smoothed,
         daily_covid_cases_per_100k = (daily_covid_cases / population) * 100000,
         daily_tests = new_tests_smoothed,
         daily_tests_per_100k = (daily_tests / population) * 100000,
         daily_positive_rate = positive_rate * 100,
         daily_vaccinations = new_vaccinations_smoothed,
         daily_vaccinations_per_100k = (daily_vaccinations / population) * 100000,
         vaccinated_pct = people_vaccinated_per_hundred,
         fully_vaccinated_pct = people_fully_vaccinated_per_hundred,
         daily_covid_cases_raw = new_cases,
         daily_covid_deaths_raw = new_deaths) %>%
  mutate(iso3c = ifelse(country == 'Kosovo', 'KSV', iso3c)) %>%
  mutate(region = ifelse(country == 'Kosovo', 'Europe', region)) %>%
  mutate(subregion = ifelse(country == 'Kosovo', 'Southern Europe', subregion)) %>%
  filter(date >= as.Date("2020-01-01"),
         !str_detect(iso3c,"OWID"),
         !str_detect(iso3c,"BLM")) %>%
  group_by(iso3c) %>%
  fill(daily_tests_per_100k,daily_positive_rate)  %>%
  dplyr::select(date,country,iso3c,region,subregion,population,
                hospital_beds_per_thousand,
                population_density,
                median_age,aged_65_older,aged_70_older,life_expectancy,
                daily_covid_deaths,daily_covid_deaths_per_100k,
                daily_covid_cases,daily_covid_cases_per_100k,
                daily_tests,daily_tests_per_100k,daily_positive_rate,
                daily_vaccinations,
                daily_vaccinations_per_100k,
                vaccinated_pct,
                fully_vaccinated_pct,
                daily_covid_cases_raw,
                daily_covid_deaths_raw)

# After the switch from JHU to WHO data, OWID sometimes misses country-days. We ensure all country-days are present here:
unique_dates <- unique(country_daily_data$date)
cat('\nAdding missing OWID country-dates: ')
country_daily_data <- country_daily_data[order(country_daily_data$date), ]
for(i in setdiff(unique(country_daily_data$iso3c), c('ESH', "TWN", 'MAC'))){
  for(j in (Sys.Date()-10):Sys.Date()){
    if(!any(country_daily_data$date == j & country_daily_data$iso3c == i)){
      temp <- country_daily_data[country_daily_data$date == max(country_daily_data$date[country_daily_data$date < j]) & country_daily_data$iso3c == i, ]
      if(nrow(temp) == 0){
        message(paste0('Data missing for more than 10 days for ', i))
      }
      temp[, c("daily_covid_deaths", "daily_covid_deaths_per_100k", "daily_covid_cases", "daily_covid_cases_per_100k",
               "daily_tests", "daily_tests_per_100k", "daily_positive_rate", "daily_vaccinations",
               "daily_vaccinations_per_100k")] <- NA
      temp$date <- as.Date(j, origin = '1970-01-01')
      
      country_daily_data <- rbind(country_daily_data, temp)
      cat(paste0(j, '-', i, '..'))
    }
  }
}
cat('\nAdding missing OWID country-dates - completed.')

# OWID data sometimes lacks the 7-day rolling average, which we here calculate and add manually when missing:
country_daily_data <- data.frame(country_daily_data[order(country_daily_data$date), ])
seven_day_average <- function(x){
  temp <- x
  for(i in 1:length(x)){
    x[i] <- mean(temp[max(c(1, i-6)):min(c(length(x), i))], na.rm = T)
  }
  x
}
# For cases (absolute, per 100k)
country_daily_data$daily_covid_cases_recalculated <- ave(country_daily_data$daily_covid_cases_raw, country_daily_data$iso3c, FUN = seven_day_average)
country_daily_data$daily_covid_cases_per_100k_recalculated <- (country_daily_data$daily_covid_cases_recalculated / country_daily_data$population) * 100000

# For deaths (absolute, per 100k)
country_daily_data$daily_covid_deaths_recalculated <- ave(country_daily_data$daily_covid_deaths_raw, country_daily_data$iso3c, FUN = seven_day_average)
country_daily_data$daily_covid_deaths_per_100k_recalculated <- (country_daily_data$daily_covid_deaths_recalculated / country_daily_data$population) * 100000

# Adding them in:
vars <- c('daily_covid_cases',
          'daily_covid_cases_per_100k',
          'daily_covid_deaths',
          'daily_covid_deaths_per_100k')

for(i in vars){
  # print(length(country_daily_data[!is.na(country_daily_data[, i]), i]) - length(country_daily_data[!is.na(country_daily_data[, paste0(i, '_recalculated')]), paste0(i, '_recalculated')])) # Uncomment this to see how many were re-calculated.
  country_daily_data[is.na(country_daily_data[, i]), i] <-   country_daily_data[is.na(country_daily_data[, i]), paste0(i, '_recalculated')]
}

# Removing extra columns:
for(i in vars){country_daily_data[, paste0(i, '_recalculated')] <- NULL}

# Fix for Taiwan regions:
country_daily_data$region[country_daily_data$iso3c == "TWN"] <- "Asia"
country_daily_data$subregion[country_daily_data$iso3c == "TWN"] <- "Eastern Asia"


# Fix for Chinese testing data, which erroneously provides tests per day per 100k for a small interval in 2020 (see source notes here https://ourworldindata.org/coronavirus-testing#china). No testing data for China is at the moment available. 
country_daily_data$daily_tests[country_daily_data$iso3c == "CHN"] <- NA
country_daily_data$daily_tests_per_100k[country_daily_data$iso3c == "CHN"] <- NA
country_daily_data$daily_positive_rate[country_daily_data$iso3c == "CHN"] <- NA


# Join excess deaths onto dataframe
country_daily_excess_deaths <- country_daily_data %>%
  ungroup() %>%
  full_join(daily_excess_deaths, by = c("iso3c", "date"))

# Add missing countries (Turkmenistan and DPRK)
na_countries <- data.frame(date = as.Date("2020-01-01"),
                           country = c("North Korea",
                                       "Turkmenistan"),
                           iso3c = c("PRK",
                                     "TKM"),
                           region = c("Asia",
                                      "Asia"),
                           subregion = c("Eastern Asia",
                                         "Central Asia"),
                           population = c(25670000, 5942000))
na_countries[, setdiff(colnames(country_daily_excess_deaths), colnames(na_countries))] <- NA
na_countries <- na_countries[, colnames(country_daily_excess_deaths)]
country_daily_excess_deaths <- rbind(country_daily_excess_deaths,
                                     na_countries)

# Ensure data has all days for all countries
country_daily_excess_deaths <- merge(country_daily_excess_deaths, expand.grid(iso3c = unique(country_daily_excess_deaths$iso3c),
                            date = seq.Date(min(country_daily_excess_deaths$date), 
                                            max(country_daily_excess_deaths$date), by = 'day'),
                            stringsAsFactors = FALSE), by = c('iso3c', 'date'), all = TRUE)

# Fill now-missing values for previously missing dates, non-date varying data:
for(i in c("country","iso3c","region","subregion","population",
           "hospital_beds_per_thousand",
           "population_density","median_age","aged_65_older",
           "aged_70_older","life_expectancy")){
  country_daily_excess_deaths[, i] <- ave(country_daily_excess_deaths[, i],
                                          country_daily_excess_deaths$iso3c, FUN = function(x){na.omit(x)[1]})
}


# Fill in leading 0s for covid data:

# Order by date
country_daily_excess_deaths <- country_daily_excess_deaths[order(country_daily_excess_deaths$date), ]

# Define function
leading_zeros <- function(x){
  if(is.na(x[1]) & sum(is.na(x)) != length(x)){
    x[1:min(which(!is.na(x))-1)] <- 0
    }
  x
}

# Cycle through relevant columns an impute leading zeroes
country_daily_excess_deaths <- country_daily_excess_deaths[order(country_daily_excess_deaths$date), ]
for(i in c("daily_covid_deaths", "daily_covid_deaths_per_100k",
           "daily_covid_cases", "daily_covid_cases_per_100k",
           "daily_total_deaths",
           "daily_total_deaths_per_100k",
           "vaccinated_pct",
           "fully_vaccinated_pct",
           "daily_vaccinations",
           "daily_vaccinations_per_100k")){
  country_daily_excess_deaths[, i] <- 
    ave(country_daily_excess_deaths[, i],
        country_daily_excess_deaths$iso3c,
        FUN = function(x) leading_zeros(x))
}

# Generate cumulative tests, cases, deaths, and vaccinations:
for(i in c("daily_tests", "daily_covid_cases", "daily_covid_deaths", "daily_vaccinations")){
  country_daily_excess_deaths[, paste0("cumulative_", i, "_per_100k")] <- ave(
    country_daily_excess_deaths[, i], country_daily_excess_deaths$iso3c, 
    FUN = function(x){
      if(sum(is.na(x)) == length(x)){x} else {
      cumsum(ifelse(is.na(x), 0, x))}}
  )*(100000/country_daily_excess_deaths$population)
}

# Generate day of week
country_daily_excess_deaths$weekday <- as.POSIXlt(country_daily_excess_deaths$date)$wday

# Step 4: import other data sources (country-level, static) ---------------------------------------

# Import V-DEM data (source: https://www.v-dem.net/en/)
vdem <- fread("source-data/vdem.csv")

# Make descriptive column names and select relevant columns
vdem <- vdem %>% 
        rename(
          iso3c = country_code,
          vdem_liberal_democracy_score = v2x_libdem,
          vdem_freedom_of_expression_score = v2x_freexp_altinf) %>% 
        select(iso3c,
               vdem_freedom_of_expression_score,
               vdem_liberal_democracy_score)

# Add to list of static datasets:
static_data <- list(vdem)

# Import Boix et al democracy binary classification (source: https://dataverse.harvard.edu/dataset.xhtml?persistentId=doi:10.7910/DVN/FJLMKT)
democracy_binary <- read_csv("source-data/democracy-v3.0.csv")

# Restrict to most recent year:
democracy_binary <- democracy_binary[democracy_binary$year == 2015, ] 

# Generate iso3c:
democracy_binary$iso3c <- countrycode(democracy_binary$ccode, "cown", "iso3c")
democracy_binary$iso3c <- ifelse(democracy_binary$country == 'Kosovo', 'KSV', democracy_binary$iso3c)
  

# Make descriptive column names and select relevant columns
democracy_binary <- democracy_binary %>% 
                    rename(
                        boix_democracy_yes_no = democracy,
                        boix_democracy_duration_years = democracy_duration) %>% 
                      select(iso3c,
                             boix_democracy_yes_no,
                             boix_democracy_duration_years)

# Add to list of static datasets:
static_data[[length(static_data) + 1]] <- democracy_binary

# Import freedom house (source: https://freedomhouse.org/report/freedom-world)
freedom_house <- read_csv("source-data/freedomhouse.csv")

# Generate iso3c and restrict to entities designated as countries (+ Hong Kong) in most recent year
freedom_house$iso3c <- countrycode(freedom_house$'Country/Territory', "country.name", "iso3c")
freedom_house$iso3c <- ifelse(freedom_house$'Country/Territory' == 'Kosovo', "KSV", freedom_house$iso3c)

freedom_house <- freedom_house[freedom_house$Edition == 2020 &
                               (freedom_house$`C/T` == "c" | freedom_house$`Country/Territory` == "Hong Kong"), ]

# Make descriptive column names and select relevant columns
freedom_house <- freedom_house %>% 
  rename(
    freedom_house_freedom_score = Total,
    freedom_house_political_rights = PR,
    freedom_house_civil_liberties = CL) %>% 
  select(iso3c,
         freedom_house_civil_liberties,
         freedom_house_political_rights,
         freedom_house_freedom_score)

# Add to list of static datasets:
static_data[[length(static_data) + 1]] <- freedom_house

# Import PolityV (source: https://www.systemicpeace.org/polityproject.html)
polity <- readxl::read_xls("source-data/p5v2018.xls")

# Generate iso3c and restrict to most recent year
polity$iso3c <- countrycode(polity$ccode, "cown", "iso3c")
polity$iso3c <- ifelse(polity$country == 'Kosovo', "KSV", polity$iso3c)
polity <- polity[polity$year == 2018, ]

# Make descriptive column names and select relevant columns
polity <- polity %>% 
  rename(
    polity_democracy_score = polity2) %>% 
  select(iso3c,
         polity_democracy_score)

# Add to list of static datasets:
static_data[[length(static_data) + 1]] <- polity

# Make island indicator variable:
islands       <- c("Antigua and Barbuda",
                   "Bahamas",
                   "Bahrain",
                   "Barbados",
                   "Brunei",
                   "Cape Verde",
                   "Comoros",
                   "Cuba",
                   "Cyprus",
                   "Dominica",
                   "Dominican Republic",
                   "East Timor",
                   "Federated States of Micronesia",
                   "Fiji",
                   "Grenada",
                   "Haiti",
                   "Iceland",
                   "Indonesia",
                   "Ireland",
                   "Jamaica",
                   "Japan",
                   "Kiribati",
                   "Madagascar",
                   "Maldives",
                   "Malta",
                   "Marshall Islands",
                   "Mauritius",
                   "Nauru",
                   "New Zealand",
                   "Northern Cyprus",
                   "Palau",
                   "Papua New Guinea",
                   "Philippines",
                   "Saint Kitts and Nevis",
                   "Saint Lucia",
                   "Saint Vincent and the Grenadines",
                   "Samoa",
                   "São Tomé and Príncipe",
                   "Seychelles",
                   "Singapore",
                   "Solomon Islands",
                   "Sri Lanka",
                   "Republic of China (Taiwan)",
                   "Tonga",
                   "Trinidad and Tobago",
                   "Tuvalu",
                   "United Kingdom",
                   "Vanuatu")

islands <- data.frame(iso3c = countrycode(islands, "country.name",
                                          "iso3c"),
                      island = TRUE)

# Ensure all countries either island or not island
islands <- merge(unique(country_daily_excess_deaths[, c("iso3c"), drop = F]), 
                 islands, by = 'iso3c', all = T)
islands$island[is.na(islands$island)] <- FALSE

static_data[[length(static_data) + 1]] <- islands

# Get WDI data:

# Update every 6 months:
if(Sys.Date() %in% c(as.Date("2021-05-31"),
                     as.Date("2022-01-01"),
                     as.Date("2022-05-31"),
                     as.Date("2023-01-01"))){
library(WDI)
wdi <- WDI(country = 'all',
           indicator = c('wdi_prop_less_2_usd_day' = 'SI.POV.DDAY',
                         'wdi_gdppc_nominal' = 'NY.GDP.PCAP.CD',
                         'wdi_gdppc_ppp' = 'NY.GDP.PCAP.PP.CD',
                         'wdi_urban_population_pct' = 'SP.URB.TOTL.IN.ZS',
                         'wdi_urban_pop_1m_cities_pct' = 'EN.URB.MCTY.TL.ZS',
                         'wdi_gini_index' = 'SI.POV.GINI',
                         'wdi_life_expectancy_at_birth' = 'SP.DYN.LE00.IN',
                         'wdi_pop_over_65' = 'SP.POP.65UP.TO.ZS',
                         'wdi_pop_under_15' = 'SP.POP.0014.TO.ZS'))


wdi$iso3c <- countrycode(wdi$iso2c, "iso2c", "iso3c")
wdi$iso3c <- ifelse(wdi$country == 'Kosovo', 'KSV', wdi$iso3c)
wdi$iso2c <- NULL
wdi$country <- NULL

# Restrict to data that is less than 10 years old
wdi <- wdi[wdi$year >= 2010, ]

# Only latest observation
wdi <- wdi[order(wdi$year), ]
wdi <- wdi[!is.na(wdi$iso3c), ]

# Reshape to long format
wdi <- pivot_longer(wdi, 
                    cols = setdiff(colnames(wdi), 
                                   c("year", "iso3c")))

# remove NA
wdi <- na.omit(wdi)

# Get max year:
wdi <- wdi[wdi$year == ave(wdi$year, 
                           paste0(wdi$iso3c, 
                                  "_", wdi$name), 
                           FUN = max), ]

# Get average year lag:
wdi$wdi_obs_lag <- ave(wdi$year, wdi$iso3c, FUN = function(x) mean(2019-x))
wdi$wdi_prop_NA <- ave(wdi$year, wdi$iso3c, FUN = function(x) 1-length(x)/length(unique(wdi$name)))
wdi$year <- NULL

# Transform back to long:
wdi <- pivot_wider(wdi, id_cols = c("iso3c", 
                                    "wdi_obs_lag",
                                    "wdi_prop_NA"),
                   names_from = "name",
                   values_from = "value")


saveRDS(wdi, "source-data/WDI_cache.RDS") 
} else {
  wdi <- readRDS("source-data/WDI_cache.RDS")
}

static_data[[length(static_data) + 1]] <- wdi

# Import lat-long coordinates (capital city, largest city):
library(maps)
data(world.cities)
world.cities$largest_city_in_country <- ave(world.cities$pop, 
                                            world.cities$country.etc,
                                            FUN = max) == world.cities$pop
world.cities <- world.cities[world.cities$largest_city_in_country | 
                               world.cities$capital == 1, ]

# Remove duplicated cities (some capitals entered twice)
world.cities <- world.cities[!duplicated(paste0(world.cities$country.etc,
                                                world.cities$name)), ]

# Make container variables
world.cities$lat_largest_city <- NA
world.cities$lng_largest_city <- NA
world.cities$lat_capital <- NA
world.cities$lng_capital <- NA

# Loop through countries and assign:
for(i in unique(world.cities$country.etc)){
  temp <- world.cities[world.cities$country.etc == i, ]
  if(nrow(temp) == 1){
    world.cities$lat_largest_city[world.cities$country.etc == i] <- 
      world.cities$lat_capital[world.cities$country.etc == i] <- 
      temp$lat
    world.cities$lng_largest_city[world.cities$country.etc == i] <- 
      world.cities$lng_capital[world.cities$country.etc == i] <- 
      temp$long
  } else {
    world.cities$lat_largest_city[world.cities$country.etc == i] <- 
      temp$lat[temp$capital != 1]
    world.cities$lat_capital[world.cities$country.etc == i] <- 
      temp$lat[temp$capital == 1]

    world.cities$lng_largest_city[world.cities$country.etc == i] <- 
      temp$long[temp$capital != 1]
    world.cities$lng_capital[world.cities$country.etc == i] <- 
      temp$long[temp$capital == 1]
  }
}

# Make descriptive column names and select relevant columns
world.cities <- world.cities %>% 
  mutate(
    iso3c = countrycode(world.cities$country.etc, "country.name",
                        "iso3c"),
    largest_city_pop = ave(world.cities$pop, world.cities$country.etc,
                           FUN = max) 
    ) %>% 
  mutate(iso3c = ifelse(country.etc == 'Kosovo', 'KSV', iso3c)) %>%
  select(iso3c,
         largest_city_pop,
         lat_largest_city,
         lng_largest_city,
         lat_capital,
         lng_capital) %>% unique()

# Convert largest city pop to % of total population
world.cities <- merge(world.cities, na.omit(unique(country_daily_excess_deaths[, c("iso3c", "population")])), by = 'iso3c', all.x = T)
world.cities$largest_city_pop_pct <- world.cities$largest_city_pop / world.cities$population
world.cities$largest_city_pop <- NULL
world.cities$population <- NULL


# Add to list of static datasets:
static_data[[length(static_data) + 1]] <- world.cities
rm(world.cities)

# Import estimated IFR if infections random and no difference in care (source: https://www.economist.com/graphic-detail/2020/11/16/why-rich-countries-are-so-vulnerable-to-covid-19 and github: https://github.com/TheEconomist/covid-19-age-adjusted-ifr):
ifr <- readRDS("source-data/ifr_cache.RDS")
ifr$iso2c[ifr$area == "Namibia"] <- "NA"
ifr$demography_adjusted_ifr <- ifr$area_ifr

ifr <- ifr %>% 
  mutate(iso3c = countrycode(iso2c, "iso2c", "iso3c")) %>%
  select(demography_adjusted_ifr, iso3c)
ksv <- ifr[ifr$iso3c == 'SRB', ]
ksv$iso3c <- 'KSV'
ifr <- na.omit(rbind(ifr, ksv))
                       
# Add to list of static datasets:
static_data[[length(static_data) + 1]] <- ifr

# Add tourism arrivals (source: https://www.unwto.org/statistic/basic-tourism-statistics):
library(readxl)
unwto <- read_xlsx("source-data/UNWTO.xlsx", skip =2)

# Fill in country name
unwto$country <- unwto$`Basic data and indicators`
for(i in 1:nrow(unwto)){
  if(is.na(unwto$country[i])){
    unwto$country[i] <- unwto$country[i-1]
  }
}

# Select column
unwto <- unwto[unwto$...6 == 'Total arrivals', ]
unwto <- unwto[!is.na(unwto$country), c("country", "...6", "Units", 2000:2019)]

# Get most recent data
unwto$tourist_arrivals_in_thousands_2019 <- NA
for(i in 1:nrow(unwto)){
  unwto$tourist_arrivals_in_thousands_2019[i] <- unlist(rev(c(NA, na.omit(as.numeric(unwto[i, as.character(2000:2019)])))))[1]
}

unwto <- unwto %>%
  mutate(
    iso3c = countrycode(country, "country.name", "iso3c")
  ) %>%
  select(
    iso3c,
    tourist_arrivals_in_thousands_2019
  )

unwto <- merge(unwto, 
               unique(country_daily_excess_deaths[, c("iso3c", "population")]), by = "iso3c")
unwto$tourist_arrivals_in_thousands_2019_per_100k <- 100000*unwto$tourist_arrivals_in_thousands_2019/unwto$population

unwto <- unwto %>%
  select(
    iso3c,
    tourist_arrivals_in_thousands_2019_per_100k
  )

# Add to list of static datasets:
static_data[[length(static_data) + 1]] <- unwto

# Import other geographical characteristics
# Source: https://dataverse.harvard.edu/file.xhtml?persistentId=doi:10.7910/DVN/SPHS5E/MXIPZK&version=1.0
geo <- read_csv("source-data/physfact_rev.csv")

# Select variables
geo <- geo %>%
  rename(iso3c = wbcode,
         centroid_latitude = cen_lat,
         centroid_longitude = cen_lon,
         mean_elevation = elev,
         mean_distance_to_coast = distc,
         percent_land_area_in_tropics = tropicar) %>%
  select(iso3c,
         centroid_latitude, centroid_longitude,
         mean_elevation, mean_distance_to_coast,
         percent_land_area_in_tropics)

# Add to list of static datasets:
static_data[[length(static_data) + 1]] <- geo

# Add data on prevalence of three other highest mortality diseases (HIV, TB, Malaria) 

# Load HIV/AIDS data (from 2015-2019):
AIDS_deaths_raw <- read_csv("source-data/AIDS_related_deaths_all_ages.csv")

# Select relevant columns, coding of NA
AIDS_deaths <- AIDS_deaths_raw[, c("Country", 2015:2019)]
AIDS_deaths[AIDS_deaths == "..."] <- NA

# Cycle through rows, record value of year of last non-NA observation:
AIDS_deaths$latest_year <- NA
AIDS_deaths$AIDS_deaths_latest <- NA

for(i in 1:nrow(AIDS_deaths)){
  
  # Find latest non-NA
  latest_non_NA <- max(which(!is.na(AIDS_deaths[i, ])))
  
  # If any, then record year (column name) and value
  if(latest_non_NA > 1){
  AIDS_deaths$latest_year[i] <- colnames(AIDS_deaths)[latest_non_NA] 
  AIDS_deaths$AIDS_deaths_latest[i] <- unlist(AIDS_deaths[i, latest_non_NA])
  }
}
AIDS_deaths <- AIDS_deaths[, c("Country", "AIDS_deaths_latest", "latest_year")]

# Remove spaces:
AIDS_deaths$AIDS_deaths_latest <- gsub(" ", "", AIDS_deaths$AIDS_deaths_latest)

# Replace values with < with half that value (e.g. <1000 = 500)
AIDS_deaths$AIDS_deaths_latest <- as.numeric(gsub("<", "", AIDS_deaths$AIDS_deaths_latest))/
  ifelse(grepl("<", AIDS_deaths$AIDS_deaths_latest), 2, 1)

# Merge in population to get per 100k:
AIDS_deaths <- AIDS_deaths %>% 
  mutate(iso3c = countrycode(Country, "country.name", "iso3c")) %>% 
  filter(!is.na(iso3c)) %>% 
  left_join(country_daily_excess_deaths %>% distinct(iso3c, population), by = "iso3c") %>% 
  mutate(AIDS_deaths_latest_per_100k = 1e5 * AIDS_deaths_latest / population,
         AIDS_deaths_latest_year = as.numeric(latest_year)) %>%
  select(iso3c, AIDS_deaths_latest_per_100k, AIDS_deaths_latest_year)

# Add to list of static datasets:
static_data[[length(static_data) + 1]] <- AIDS_deaths

# Load TB data:
TB_deaths_raw <- read_csv("source-data/TB_burden_countries_2021-06-25.csv")

# Merge in population to get per 100k:
TB_deaths <- TB_deaths_raw %>% 
  select(iso3c = iso3, year, TB_deaths_latest = e_mort_exc_tbhiv_num) %>% 
  filter(!is.na(TB_deaths_latest)) %>% 
  arrange(iso3c, desc(year)) %>% 
  group_by(iso3c) %>% slice(1:1) %>% ungroup() %>% 
  left_join(country_daily_excess_deaths %>% distinct(iso3c, population), by = "iso3c") %>% 
  transmute(iso3c, TB_deaths_latest_per_100k_year = year, # This ensures year is saved as separate column
            TB_deaths_latest_per_100k = 1e5 * TB_deaths_latest / population) 

# Since all countries in our data have TB deaths from 2019, remove year of obs columns 
# to verify: summary(TB_deaths[TB_deaths$iso3c %in% country_daily_excess_deaths$iso3c, ])
TB_deaths$TB_deaths_latest_per_100k_year <- NULL 

# Add to list of static datasets:
static_data[[length(static_data) + 1]] <- TB_deaths

# Load Malaria data for 2019:
malaria_deaths_raw <- read_xlsx("source-data/malaria_deaths_2000_2019.xlsx", skip = 5)

# Merge in population to get per 100k 
malaria_deaths <- malaria_deaths_raw %>% 
  filter(Year == 2019 & nchar(ISO3) == 3) %>% 
  select(iso3c = ISO3, malaria_deaths_latest = Point...9) %>% 
  left_join(country_daily_excess_deaths %>% distinct(iso3c, population), by = "iso3c") %>% 
  transmute(iso3c, malaria_deaths_latest_per_100k = 1e5 * malaria_deaths_latest / population) 

# Add to list of static datasets:
static_data[[length(static_data) + 1]] <- malaria_deaths


# Load total mortality burden data (number of deaths in 2019 estimates):
total_deaths_raw <- read_xlsx("source-data/total_deaths_who_2019.xlsx", sheet = "Deaths All ages", skip = 6)

# Merge in population to get per 100k
total_deaths <- total_deaths_raw %>% 
  filter(Sex == "Persons" & `GHE cause` == "All Causes") %>%  # Select all causes
  select(Afghanistan:Zimbabwe) %>%  # Select all countries
  pivot_longer(cols = everything(), names_to = "iso3c", values_to = "deaths") %>% 
  mutate(iso3c = countrycode(iso3c, "country.name", "iso3c"),
         total_deaths_latest = as.numeric(deaths) * 1e3) %>%  # Convert from thousands
  left_join(country_daily_excess_deaths %>% distinct(iso3c, population), by = "iso3c") %>% 
  transmute(iso3c, total_deaths_latest_per_100k = 1e5 * total_deaths_latest / population) # Calculate per 100k

# Add to list of static datasets:
static_data[[length(static_data) + 1]] <- total_deaths

# This can be used to inspect static data:
manually_inspect <- FALSE
if(manually_inspect){
  library(tidyverse)
  world <- map_data("world")
  world$iso3c <- countrycode(world$region, "country.name", "iso3c")
  
  test_data <- AIDS_deaths
  test_data$var <- test_data$AIDS_deaths_latest_per_100k 
  
  world$var <- NA
  for(i in unique(test_data$iso3c)){
    world$var[world$iso3c == i] <- test_data$var[test_data$iso3c == i]
  }
  
  ggplot() +
    geom_map(
      data = world, map = world,
      aes(long, lat, map_id = region, fill = var),
      color = "black", size = 0.1)
}

# Step 5: import other data sources (country-level, time-varying) ---------------------------------------
time_varying_data <- list()

# Google mobility reports (no longer updating as of 2022-10-15):
# mob <- fread("https://www.gstatic.com/covid19/mobility/Global_Mobility_Report.csv")
# saveRDS(mob, "source-data/mobility_cache.RDS") 
mob <- readRDS('source-data/mobility_cache.RDS')

# Restrict to country-level data
mob <- unique(mob[mob$sub_region_1 == "" & mob$sub_region_2 == "" & mob$metro_area == "", ])

# Generate iso3c, rename and select columns
mob <- mob %>%
  mutate(
    iso3c = countrycode(country_region_code, "iso2c", "iso3c"),
  ) %>%
  rename(
         mobility_retail_rec_pct_of_baseline = retail_and_recreation_percent_change_from_baseline,
         mobility_grocery_and_pharma_pct_of_baseline = grocery_and_pharmacy_percent_change_from_baseline,
         mobility_parks_pct_of_baseline = parks_percent_change_from_baseline,
         mobility_transit_rec_pct_of_baseline = transit_stations_percent_change_from_baseline,
         mobility_workplaces_rec_pct_of_baseline = workplaces_percent_change_from_baseline,
  ) %>%
  select(iso3c, 
         date,
         mobility_retail_rec_pct_of_baseline,
         mobility_grocery_and_pharma_pct_of_baseline,
         mobility_parks_pct_of_baseline,
         mobility_transit_rec_pct_of_baseline,
         mobility_workplaces_rec_pct_of_baseline)

# Add to time-varying data
time_varying_data[[length(time_varying_data) + 1]] <- mob

# Add polity response data (source: https://raw.githubusercontent.com/OxCGRT/covid-policy-tracker/master/data/OxCGRT_latest.csv)
ox <- data.frame(fread("https://raw.githubusercontent.com/OxCGRT/covid-policy-tracker-legacy/main/legacy_data_202207/OxCGRT_latest.csv"))

# Use only national data:
ox <- ox[ox$Jurisdiction == "NAT_TOTAL", ]

# Only use national policies (not those with geographical scope, as we do not know the extent of the geographical scope):
for(i in c("C1_School.closing", "C2_Workplace.closing", 
           "C3_Cancel.public.events", "C4_Restrictions.on.gatherings", 
           "C5_Close.public.transport", "C6_Stay.at.home.requirements", 
           "C7_Restrictions.on.internal.movement", 
           "H6_Facial.Coverings")){
  
  ox[(ox[, paste0(substr(i, 1, 2), "_Flag")] != 1 & !is.na(ox[, paste0(substr(i, 1, 2), "_Flag")])), i] <- 0
}

ox <- ox %>% 
  mutate(oxcgrt_schools_closed = `C1_School.closing`,
         oxcgrt_workplaces_closed = `C2_Workplace.closing`,
         oxcgrt_cancel_public_events = `C3_Cancel.public.events`,
         oxcgrt_gathering_restrictions = `C4_Restrictions.on.gatherings`,
         oxcgrt_public_transport_closed = `C5_Close.public.transport`,
         oxcgrt_stay_at_home_required = `C6_Stay.at.home.requirements`,
         oxcgrt_internal_movement_restrictions = `C7_Restrictions.on.internal.movement`,
         oxcgrt_international_movement_restrictions = `C8_International.travel.controls`,
         oxcgrt_face_masks_required = `H6_Facial.Coverings`,
         iso3c = countrycode(CountryName, "country.name", "iso3c"),
         date = as.Date(as.character(Date), format = "%Y%m%d")
         ) %>%
  mutate(iso3c = ifelse(CountryName == 'Kosovo', 'KSV', iso3c)) %>%
  select(oxcgrt_schools_closed,
         oxcgrt_workplaces_closed,
         oxcgrt_cancel_public_events,
         oxcgrt_gathering_restrictions,
         oxcgrt_public_transport_closed,
         oxcgrt_stay_at_home_required,
         oxcgrt_internal_movement_restrictions,
         oxcgrt_international_movement_restrictions,
         oxcgrt_face_masks_required,
         iso3c,
         date)

# Add to time-varying data
time_varying_data[[length(time_varying_data) + 1]] <- ox

# Import seroprevalence data:
# We here use a static version, which is updated every time the models are. This is because new data typically comes in for observations well in the past, rather than for new dates, and because we need to inspect which surveys to include manually.
update_model <- F
if(update_model){
  # This is wrapped in tryCatch in case download link ceases to operate. If so, reverts to backup (done daily.), and records incident in log
  gs4_deauth() # This puts googlesheets4 into a de-authorized mode.
  
tryCatch( expr = {sero_raw <- googlesheets4::read_sheet("https://docs.google.com/spreadsheets/d/17R1wayZYk6WzGqe8HioWWBnqSd_KyW-BNCYJ57lBwc4/edit?usp=sharing")
write_csv(sero_raw, "source-data/serotracker.csv")}, 
error = function(e){ sero_raw <- read_csv("source-data/serotracker.csv")
print(" -- serotracker download link is no longer operational. --")
message(e)})
} else {
  sero_raw <- read_csv("source-data/serotracker.csv")
}    

### Restrict to national or regional surveys with random or quasi-random sampling among adult population and without high risk of bias.
sero <- sero <- sero_raw %>% 
  filter(`Grade of Estimate Scope` %in% c("National", "Regional")) %>% 
  mutate(`Sampling Method` = case_when(`Prevalence Estimate Name` %in% c("200609_Bergamo_HealthAgency_GenPop", # miss-coded in original.
                                                                         "210206_China_ChineseCenterforDiseaseControlandPrevention_Overall",  # Does not match the number in the paper for the national seropositivity. 
                                                                         "210320_Wuhan_ChineseAcademyOfMedicalSciences_overall_Roche", # Excluded as it targeted areas within Wuhan specifically (and thus unrepresentative even at the regional level). 
                                                                         "210312_NorthCarolina_WakeForestBaptistHealth") ~ "Convenience", # miss-coded in original (recruited via email, etc.). 35.2% are healthcare workers, etc.. % seropositivity is also the estimate for endpoint, not total over sample period.
                                       T ~ `Sampling Method`)) %>% 
  mutate(`Grade of Estimate Scope` = case_when(`Prevalence Estimate Name` %in% c("210815_Ethiopia_FederalMinistryofHealth_Overall_Adjusted", # Urban areas only.
                                                                                 "210311_Egypt_CenterofScientificExcellenceforInfluenzaViruses_October3", # Rural areas only
                                                                                 "210525_Pakistan_RiphahInternationalUniversity" # Selected cities                       
  ) ~ "Regional", 
  T ~ `Grade of Estimate Scope`))   %>% 
  filter(`Sampling Method` %in% c("Sequential", "Simplified probability", "Stratified probability", "Stratified non-probability")) %>% 
  filter(`Sample Frame (groups of interest)` %in% c("Household and community samples", "Residual sera", "Multiple populations", "Blood donors")) %>% 
  filter(`Sample Frame (age)` %in% c("Adults (18-64 years)", "Multiple groups", "Not reported")) %>% 
  filter(!`Overall Risk of Bias (JBI)` %in% c("High")) %>%
  filter(!(`Overall Risk of Bias (JBI)` == "Unclear" &
             `Data Quality Status` == "Unverified")) %>%
  filter(!`Prevalence Estimate Name` %in% c("210302_Diredawa_TheUniversityofSheffield_Overall", #= places with overcrowding
                                            "210309_Zambia_ZambiaMinistryofHealth_overall", #= Does not include overall.
                                            "210108_CapeVerde_MestreEmSaúdeEDesenvolvimento_overall", # = recorded estimate does not match numbers in abstract.
                                            "210116_SaudiArabia_MinistryofHealth_overall", # Sample excluded those with past or current infection.
                                            "201217_Honduras_ColegioMédico_OverallGenpop", # Only took samples from unaffected regions      
                                            "210618_Brazil_UniversidadeFederaldePelotas_primary", # Only children 
                                            "210424_EmirateofAbuDhabi_AbuDhabiPublicHealthCenter_Households_TestAdj." # Excluded labor camps (which were much higher)
  )) %>%
  ### If start date is unknown but end date is not, assume all done in one day. Set date to midpoint.
  mutate(start_date = as.Date(anytime(`Sampling Start Date (ISO)`)),
         end_date = as.Date(anytime(`Sampling End Date (ISO)`))) %>% 
  mutate(start_date = case_when(is.na(start_date) ~ end_date, T ~ start_date)) %>% 
  mutate(date = start_date + floor((end_date - start_date)/2)) %>% 
  transmute(iso3c = countrycode(`Country`, "country.name", "iso3c"),
            date,
            seroprevalence_regional_or_national = as.numeric(gsub("%", "", `Serum positive prevalence (%)`)),
            seroprevalence_national = ifelse(`Grade of Estimate Scope` == "National", as.numeric(gsub("%", "", `Serum positive prevalence (%)`)), NA),
            seroprevalence_N = `Denominator Value`,
            seroprevalence_study_level = `Grade of Estimate Scope`) %>% 
  group_by(iso3c, date) %>% 
  summarise(seroprevalence_regional_or_national = weighted.mean(seroprevalence_regional_or_national, seroprevalence_N),
            seroprevalence_national = weighted.mean(seroprevalence_national, seroprevalence_N, na.rm = T),
            seroprevalence_N = sum(seroprevalence_N)) %>% 
  filter(iso3c %in% unique(country_daily_excess_deaths$iso3c)) %>% 
  full_join(country_daily_excess_deaths %>% 
              select(iso3c, date, region, cumulative_daily_covid_cases_per_100k, population),
            by = c("iso3c", "date")) %>% 
  arrange(iso3c, date) 

# Save dates of last known sero-survey by iso3c:
sero_end_sample_dates_nat <- unique(sero[!is.na(sero$seroprevalence_national), c("iso3c", "date")])
sero_end_sample_dates_reg_or_nat <- unique(sero[!is.na(sero$seroprevalence_regional_or_national), c("iso3c", "date")])

### Rolling weighted average of sero
weighted_rolling_sero_mean <- function (df) {
  df %>% 
    mutate(weighted_seroprevalence_reg_or_nat = seroprevalence_regional_or_national * log(seroprevalence_N),
           weighted_seroprevalence_nat = seroprevalence_national * log(seroprevalence_N))%>% 
    mutate(seroprevalence_reg_or_nat_30d_wma_N = zoo::rollapply(weighted_seroprevalence_reg_or_nat, width = 30, sum, na.rm = T, align = "right", fill = NA),
           seroprevalence_nat_30d_wma_N = zoo::rollapply(weighted_seroprevalence_nat, width = 30, sum, na.rm = T, align = "right", fill = NA),
           seroprevalence_reg_or_nat_30d_wma_D = zoo::rollapply(log(seroprevalence_N), width = 30, sum, na.rm = T, align = "right", fill = NA)) %>% 
    mutate(seroprevalence_reg_or_nat_30d_wma = seroprevalence_reg_or_nat_30d_wma_N / seroprevalence_reg_or_nat_30d_wma_D,
           seroprevalence_nat_30d_wma = seroprevalence_nat_30d_wma_N / seroprevalence_reg_or_nat_30d_wma_D) %>% 
    mutate_at(c("seroprevalence_reg_or_nat_30d_wma", "seroprevalence_nat_30d_wma"),
              list(~case_when(cumulative_daily_covid_cases_per_100k == 0 | is.na(cumulative_daily_covid_cases_per_100k) ~ 0, T ~ .))) %>% 
    mutate(sero_reg_or_nat_30d_wma_interpolated = zoo::na.approx(seroprevalence_reg_or_nat_30d_wma, na.rm = F),
           sero_nat_30d_wma_interpolated = zoo::na.approx(seroprevalence_nat_30d_wma, na.rm = F)) %>% 
    mutate_at(c("sero_reg_or_nat_30d_wma_interpolated", "sero_nat_30d_wma_interpolated"),
              list(~cummax(ifelse(is.na(.), 0, .))))
}

sero <- map_dfr(split(sero, f = sero$iso3c), weighted_rolling_sero_mean) %>% 
  ungroup()

# Set interpolated seroprevalence to NA when 15 days has passed since last known survey date:
for(i in unique(sero$iso3c)){
  temp_max_date <- max(sero_end_sample_dates_reg_or_nat$date[sero_end_sample_dates_reg_or_nat$iso3c == i])+15 
  sero$sero_reg_or_nat_30d_wma_interpolated[sero$iso3c == i & sero$date > temp_max_date ] <- NA

  temp_max_date <- max(sero_end_sample_dates_nat$date[sero_end_sample_dates_nat$iso3c == i])+15 
  sero$sero_nat_30d_wma_interpolated[sero$iso3c == i & sero$date > temp_max_date ] <- NA
}

# Define function for centered average:
centered_average <- function(x, n = 30, ...){
  unlist(lapply(1:length(x), FUN = function(i){
    window <- max(c(0, min(c(floor(n/2), i-floor(n/2), length(x)-i))))
    mean(x[(i-window):(i+window)], ...)
  }))
}

# Transform to 30-day average change in seroprevalence
sero <- sero[order(sero$date), ]
sero$sero_nat_delta <- ave(sero$sero_nat_30d_wma_interpolated,
                           sero$iso3c,
                           FUN = function(x){
                             x <- x - c(0, x)[1:length(x)]
                             centered_average(x, n=30,
                                              na.rm = T)
                           })
sero$sero_nat_or_reg_delta <- ave(sero$sero_reg_or_nat_30d_wma_interpolated,
                                  sero$iso3c,
                                  FUN = function(x){
                                    x <- x - c(0, x)[1:length(x)]
                                    centered_average(x, n=30, na.rm = T)
                                  })

# Select relevant columns:
sero <- sero %>% 
  select(iso3c, date,
         sero_nat_delta,
         sero_nat_or_reg_delta,
         sero_nat_30d_wma_interpolated, sero_reg_or_nat_30d_wma_interpolated)

# To account for time lags between serosurveys being completed and added to the database (i.e. analysis completed and published, mean time of 96 days in 2021 as of last update of seroprevalence data), very recent observations are removed (this is important mainly for regional averages which otherwise can be skewed by small samples of seroprevalence estimates):
sero <- sero[sero$date <= as.Date("2021-05-11"), ]

# Add to time-varying data
time_varying_data[[length(time_varying_data) + 1]] <- sero

# Load average weekly temperatures by city:
# Source: https://www.kaggle.com/hansukyang/temperature-history-of-1000-cities-1980-to-2020
# This dataset was constructed using the Copernicus Climate Service by Joseph Yang of oikalabs.com. 
use_climate_cache <- TRUE
if(!use_climate_cache){
  climate <- data.frame(t(read_csv("source-data/daily_temperature_1000_cities_1980_2020.csv")))
  colnames(climate) <- climate[1, ]
  climate <- climate[-1, ]
  climate <- climate[!is.na(climate$date), ]
  
  # Reshape to long format:
  climate <- climate %>% pivot_longer(13:ncol(climate)) %>% data.frame()
  climate$value <- as.numeric(climate$value)
  climate$population <- as.numeric(climate$population)
  climate$iso3c <- climate$iso3
  climate <- climate[!is.na(climate$iso3c), ]
  
  # Specify date as date:
  climate$date <- as.Date(climate$name)
  climate <- climate[climate$date >= as.Date("2015-01-01") & climate$date < as.Date("2020-01-01"), ]
  
  # Get week of the year:
  climate$week <- week(climate$date)
  
  climate <- climate[!is.na(climate$value) & !is.na(climate$population), ]
  
  # Get population-weighted mean by week and country:
  climate$weekly_mean_temperature_in_major_cities_2015_2019 <- 
    ave(climate[, c("value", "population")], paste0(climate$iso3c, "_", climate$week),
        FUN = function(x){
          x <- na.omit(x)
          c(weighted.mean(x=x[, 1], w=x[, 2], na.rm=T), NA)[1]
          })[[1]]
  
  # Collapse to weekly country-level dataset:
  climate <- climate[!duplicated(paste0(climate$iso3c, "_", climate$week)), 
                     c("iso3c", "week",
                       "weekly_mean_temperature_in_major_cities_2015_2019")]
  
  # Convert to 3-week average:
  climate <- climate[order(climate$date), ]
  climate$weekly_mean_temperature_in_major_cities_2015_2019 <- 
    ave(climate$weekly_mean_temperature_in_major_cities_2015_2019,
        climate$iso3c,
        FUN = function(x){
          temp <- x
          for(i in 1:length(x)){
            temp[i] <- mean(x[max(c(i-1, 1)):min(c(length(x), i+1))])
          }
          temp})
  
  saveRDS(climate, "source-data/climate_cache.RDS")}

# Load country - date, major-city population-weighted average temperature 
climate <- readRDS("source-data/climate_cache.RDS")

# Add date column:
date_week_ids <- data.frame(date = unique(country_daily_excess_deaths$date),
                            week = week(unique(country_daily_excess_deaths$date)))
climate <- merge(climate, date_week_ids)

# Add to time-varying data
time_varying_data[[length(time_varying_data) + 1]] <- unique(climate[, c("iso3c", "date", "weekly_mean_temperature_in_major_cities_2015_2019")])

# Step 6: merge everything together ---------------------------------------

# Ensure all obs have iso3c:
country_daily_excess_deaths <- country_daily_excess_deaths[!is.na(country_daily_excess_deaths$iso3c), ]

for(i in static_data){
  country_daily_excess_deaths <- merge(country_daily_excess_deaths,
                                       unique(i), by = "iso3c", all.x = T) }

for(i in time_varying_data){
  country_daily_excess_deaths <- merge(country_daily_excess_deaths,
  i, by = c("iso3c", "date"), all.x = T)
}

# Check that no countries has multiple observations for the same date:
max(table(paste0(country_daily_excess_deaths$iso3c, "_", country_daily_excess_deaths$date)))==1

# Step 7: fix regions and country names to The Economist standard  ---------------------------------------
econ_df <- read_csv("source-data/economist_country_names.csv") %>%
  rename(
    country = Name,
    economist_region = Regions,
    wb_income_group = `Income group WB`,
    imf_economy = `Economy IMF`,
    iso3c = ISOA3
    ) %>%
  select(
    country, iso3c,
    iso3c,
    wb_income_group,
    imf_economy,
    economist_region
  )

country_daily_excess_deaths$country <- NULL
country_daily_excess_deaths <- merge(country_daily_excess_deaths, 
                                     econ_df, by = "iso3c", 
                                     all.x = T)

# Adding small territories not in master list:
country_daily_excess_deaths$economist_region[country_daily_excess_deaths$iso3c == "GGY"] <- "Europe"
country_daily_excess_deaths$economist_region[country_daily_excess_deaths$iso3c == "JEY"] <- "Europe"
country_daily_excess_deaths$economist_region[country_daily_excess_deaths$iso3c == "KSV"] <- "Europe"

# Fix for Taiwan regions:
country_daily_excess_deaths$region[country_daily_excess_deaths$iso3c == "TWN"] <- "Asia"
country_daily_excess_deaths$subregion[country_daily_excess_deaths$iso3c == "TWN"] <- "Eastern Asia"

# Step 8: add in large subregions/non-repo excess deaths data, and set excess death baseline to 0 ---------------------------------------
# China (source: https://www.bmj.com/content/372/bmj.n415):dwa
# Data from online supplemental materials. I have requested further data but was not supplied it.
china <- read_csv("source-data/china_bmj_excess_deaths.csv", skip = 1)
china_wuhan <- china[, 1:5]
china_hubei_except_wuhan <- china[, c(1:2, 6:8)]
china_outside_hubei <- china[, c(1:2, 9:11)]
colnames(china_wuhan)[3] <- "Mean No. of reported Deaths, 2015-19"
colnames(china_wuhan)[5] <- "No. of reported deaths in 2020"
colnames(china_hubei_except_wuhan) <- colnames(china_wuhan)
colnames(china_outside_hubei) <- colnames(china_wuhan)

# Population associated with each reporting area also from supplemental materials:
china_wuhan$type <- "Wuhan"
china_wuhan$population <- 2300887 
china_wuhan$mean_population_2015_2019 <- (2202663 + 2280200 + 2314269 + 2293425 + 2300887)/5  
china_wuhan$total_population <- 11081000 # from http://www.hb.xinhuanet.com/2019-03/26/c_1124281764.htm

colnames(china_hubei_except_wuhan) <- colnames(china_wuhan)
china_hubei_except_wuhan$type <- "Hubei, except Wuhan"
china_hubei_except_wuhan$population <- 10156616
china_hubei_except_wuhan$mean_population_2015_2019 <- (10348961 + 10216200 + 10252213 + 10179298 + 10156616)/5  
china_hubei_except_wuhan$total_population <- 59270000 - 11081000 # from https://www.statista.com/statistics/279013/population-in-china-by-region/

colnames(china_outside_hubei) <- colnames(china_wuhan)
china_outside_hubei$type <- "China, outside Hubei"
china_outside_hubei$population <- 325888003
china_outside_hubei$mean_population_2015_2019 <- (319755726 + 321459243
 + 323652034 + 324722209
 + 325888003)/5  

china_outside_hubei$total_population <- 1400050000 - 59270000 - 11081000  # from https://www.statista.com/statistics/263765/total-population-of-china/

china <- rbind(china_wuhan,
               china_hubei_except_wuhan,
               china_outside_hubei)

# This adjusts for deaths data not being recorded when the paper was written:
china$reporting_adjusted_deaths <- china$`No. of reported deaths in 2020`/(china$`Delay adjustment ratio (%)*`/100)

# Since they do not supply the raw data, we cannot do better than:
china$expected_deaths <- china$`Mean No. of reported Deaths, 2015-19`
# And adjusting for total increase in mortality in China over time, projecting one year forward (see macrotrends.net/countries/CHN/china/death-rate):
mortality_2015_2019 <- cbind.data.frame(
  "mort" = c(6.880, 6.911, 6.941, 6.972, 7.003, 7.027, 7.05, 7.074, 7.097, 7.121, 7.261),
  "year" = c(2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017, 2018, 2019))
mean_mort <- mean(mortality_2015_2019$mort[mortality_2015_2019$year %in% 2015:2019])
projected_mort <- predict(newdata = data.frame(year = 2020,
                                     mort = NA),
                          lm(mort ~ year, data = mortality_2015_2019))

#mortality_2015_2019 <- rbind(mortality_2015_2019, c(projected_mort, 2020))
#plot(mortality_2015_2019[, c("year", "mort")])

china$expected_deaths <- china$expected_deaths*(projected_mort/mean_mort)

# With population-change multiplier:
china$expected_deaths <- china$expected_deaths*(china$population/china$mean_population_2015_2019)
# Note: supplemental figure 2 suggests the mean should work well, no strong over-time-trend in per capita death.

# To inspect our estimates for China excess deaths by area, uncomment the below chunk:
# ggplot(china, aes(x=as.numeric(Week), y=100000*reporting_adjusted_deaths/population, col = "2020"))+
#   geom_line()+
#   geom_line(aes(y=100000*expected_deaths/population, col = "Expected"))+
#   geom_hline(aes(yintercept = 0), col = "black")+ylim(c(-10, 100))+
# theme_minimal()+facet_grid(.~type)+xlab("Week in 2020")+ylab("Deaths per 100k population")

# The final week appears anamolous compared to the figures in the paper. Upon inspection, it appears the table reports a drop in expected deaths that seems anamolously large compared to the figures in the paper. We therefore conservatively drop the final week:
china <- china[china$Week != 13, ]

# Specify excess deaths
china$excess_deaths <- china$reporting_adjusted_deaths - china$expected_deaths
china$excess_deaths_per_100k <- 100000*china$excess_deaths/china$population

# We next combine the reporting regions, weighting them by the total population they represent.
china$pop_weights <- china$total_population / 1400050000

china$country_excess_deaths_per_100k <- NA
for(i in unique(china$Week)){
  china$country_excess_deaths_per_100k[china$Week == i] <- sum(china$excess_deaths_per_100k[china$Week == i]*china$pop_weights[china$Week == i])
}

# Transform this to a daily rate
china$country_excess_deaths_per_100k <- china$country_excess_deaths_per_100k / 7

# And daily dataset:
china_daily <- unique(china[china$`Starting date of the week` != "Jan-March", c("country_excess_deaths_per_100k", "Starting date of the week")])
china_daily$date <- as.Date(paste0(china_daily$`Starting date of the week`, "-2020"), 
                                   format = "%d-%b-%Y")
# Add in other 6 days:
temp <- china_daily
for(i in 1:6){
  temp$date <- temp$date + 1
  china_daily <- rbind(china_daily, temp)
}
china_daily$excess_deaths <- china_daily$country_excess_deaths_per_100k * 1400050000 / 100000

# Add into big dataset:
for(i in 1:nrow(china_daily)){
country_daily_excess_deaths$daily_excess_deaths_per_100k[country_daily_excess_deaths$iso3c == "CHN" & country_daily_excess_deaths$date == china_daily$date[i]] <- china_daily$country_excess_deaths_per_100k[i]
country_daily_excess_deaths$daily_excess_deaths[country_daily_excess_deaths$iso3c == "CHN" & country_daily_excess_deaths$date == china_daily$date[i]] <- china_daily$excess_deaths[i]
}

# Load subnational data:
add.subnational <- T
if(add.subnational){
# if desire to regenerate: source("scripts/aux_subnational_data_generation.R")
subnational <- readRDS("output-data/model-objects/auxilliary_subnational_data.RDS")

subnational$lat_largest_city <- subnational$lat_capital
subnational$lng_largest_city <- subnational$lng_capital

# Merge in missing columns from big dataset:
subnational <- merge(subnational, country_daily_excess_deaths[, c("date", "iso3c", setdiff(colnames(country_daily_excess_deaths), colnames(subnational)))], by = c("iso3c", "date"))

# Restrict applicable seroprevalence estimates to national ones:
subnational$sero_nat_or_reg_delta <- subnational$sero_nat_delta
subnational$sero_reg_or_nat_30d_wma_interpolated <- subnational$sero_nat_30d_wma_interpolated

# Make iso3c reflect subnational status
subnational$iso3c <- paste0(subnational$iso3c, "_", gsub(" ", "_", subnational$name))

# Restrict to columns in big data
subnational <- subnational[, colnames(country_daily_excess_deaths)]

subnational$is_subregion <- "YES"

# Merge this in with big data
country_daily_excess_deaths$is_subregion <- "NO"
country_daily_excess_deaths <- rbind(country_daily_excess_deaths, subnational)
}

# Set baseline excess deaths due to the pandemic to 0 for all countries except China for the first 3 weeks of January. Countries did not have mass excess deaths before this time, but some had lower than expected deaths in this time period, perhaps due to random variation. This helps the model and estimates only pick up pandemic-related excess deaths:  
country_daily_excess_deaths$daily_excess_deaths[country_daily_excess_deaths$date <= as.Date("2020-01-21") & country_daily_excess_deaths$iso3c != "CHN" & !is.na(country_daily_excess_deaths$daily_excess_deaths)] <- 0
country_daily_excess_deaths$daily_excess_deaths_per_100k[country_daily_excess_deaths$date <= as.Date("2020-01-21") & country_daily_excess_deaths$iso3c != "CHN" & !is.na(country_daily_excess_deaths$daily_excess_deaths_per_100k)] <- 0

# Step 9: calculate region averages  ---------------------------------------

# Fix for countries which have backward adjusted their deaths:
omit_iso3c <- c("PER", "ECU")
omit_cols <- c("daily_covid_deaths", "daily_covid_deaths_per_100k", "cumulative_daily_covid_deaths_per_100k")

# Ensure row order stays the same
country_daily_excess_deaths$row_order <- 1:nrow(country_daily_excess_deaths)

# Save them, set them as NA in main data:
omit_iso3c_data <- country_daily_excess_deaths[country_daily_excess_deaths$iso3c %in% omit_iso3c, omit_cols]
country_daily_excess_deaths[country_daily_excess_deaths$iso3c %in% omit_iso3c, omit_cols] <- NA

# Calculate regional averages by day (weighted by population)

# 0. Select definition of region (NB: 'region' are as defined by World Bank Development Indicators)
library(countrycode)

# Continent variable
country_daily_excess_deaths$continent <- countrycode(unlist(substr(country_daily_excess_deaths$iso3c, 1, 3)), "iso3c", "continent")
country_daily_excess_deaths$continent[country_daily_excess_deaths$iso3c == "KSV"] <- "Europe" # fix for Kosovo

# Region variable
country_daily_excess_deaths$region <- countrycode(unlist(substr(country_daily_excess_deaths$iso3c, 1, 3)), "iso3c", "region")
country_daily_excess_deaths$region[country_daily_excess_deaths$iso3c == "SHN"] <- "Europe & Central Asia" # fix for Saint Helena
country_daily_excess_deaths$region[country_daily_excess_deaths$iso3c == "KSV"] <- "Europe & Central Asia" # fix for Kosovo

# Sub-region variable (fix for Kosovo)
country_daily_excess_deaths$subregion[country_daily_excess_deaths$iso3c == "KSV"] <- "Southern Europe" # fix for Kosovo

# 1. Define region-average function:
region_average <- function(variable = country_daily_excess_deaths$daily_total_deaths_per_100k,
                           region = country_daily_excess_deaths$region,
                           time = country_daily_excess_deaths$date,
                           weights = log(country_daily_excess_deaths$population),
                           iso3c = country_daily_excess_deaths$iso3c,
                           include_current_obs = FALSE,
                           omit.subnational = T){
  
  if(omit.subnational){
    weights[nchar(iso3c) > 3] <- 0
  }

  # Make ID variable
  id <- paste0(region, "_", time)

  # Make results container
  res <- rep(NA, length(variable))
  
  # Loop over region-days
  if(include_current_obs){ # Including observation in question
    res <- ave(cbind.data.frame(variable, weights), 
               id, FUN = function(x){
                 weighted.mean(x= x[, 1],
                               w= x[, 2],
                               na.rm = T)})[, 1]
  } else {
    res <- ave(cbind.data.frame(variable, weights), 
                  id, FUN = function(x){
                      unlist(lapply(1:nrow(x), FUN = function(pp){
                             weighted.mean(x= x[-pp, 1],
                                           w= x[-pp, 2],
                                           na.rm = T)}))
                         })[, 1]
    }
  
  return(res)}


# 2. Define variables to calculate region averages for:

# Time-varying:
region_average_vars_ts <- c("sero_nat_or_reg_delta",
                         "sero_nat_delta",
                         "sero_nat_30d_wma_interpolated", 
                         "sero_reg_or_nat_30d_wma_interpolated",
                         "daily_excess_deaths_per_100k",
                         "daily_tests_per_100k",
                         "daily_covid_cases_per_100k",
                         "daily_covid_deaths_per_100k",
                         "daily_positive_rate")

# Time-constant:
region_average_vars_c <- c("demography_adjusted_ifr")

# 3. Calculate by applying function defined above by region and subregion:
for(p in region_average_vars_ts){
  country_daily_excess_deaths[,
                              paste0(p, "_region_average")] <- region_average(variable = country_daily_excess_deaths[, p],
                                                                              include_current_obs = FALSE)
  
  country_daily_excess_deaths[, 
                              paste0(p, "_sub_region_average")] <- region_average(variable = country_daily_excess_deaths[, p],
                                                                                  region = country_daily_excess_deaths$subregion,
                                                                              include_current_obs = FALSE)
  
  country_daily_excess_deaths[, 
                              paste0(p, "_econ_region_average")] <- region_average(variable = country_daily_excess_deaths[, p],
                                                                                  region = country_daily_excess_deaths$economist_region,
                                                                                  include_current_obs = FALSE)
  
  print(paste("Region average completed for:", p))
} # NB: excluding observation in question is crucial if doing this for e.g. observed excess deaths

# Doing the same for non-time-varying variables
constant_temp <- country_daily_excess_deaths[, c("iso3c", "region", "subregion", "economist_region", "population", "date", region_average_vars_c)]
constant_temp <- constant_temp[!duplicated(constant_temp$iso3c), ]
constant_temp$date <- "static"
col_start <- ncol(constant_temp) + 1

for(p in region_average_vars_c){
  constant_temp[, paste0(p, "_region_average")] <- region_average(variable = constant_temp[, p],
          region = constant_temp$region,
          time = constant_temp$date,
          iso3c = constant_temp$iso3c,
          weights = log(constant_temp$population),
          include_current_obs = FALSE)
  
  constant_temp[, paste0(p, "_sub_region_average")] <- region_average(variable = constant_temp[, p],
              region = constant_temp$subregion,
              time = constant_temp$date,
              iso3c = constant_temp$iso3c,
              weights = log(constant_temp$population), include_current_obs = FALSE)
  
  constant_temp[, paste0(p, "_econ_region_average")] <- region_average(variable = constant_temp[, p],                          region = constant_temp$economist_region,
               time = constant_temp$date,
               iso3c = constant_temp$iso3c,
               weights = log(constant_temp$population), include_current_obs = FALSE)
  
  print(paste("Region average completed for:", p))
} # NB: excluding observation in question is crucial if doing this for e.g. observed excess deaths

# Merge in these columns:
constant_temp <-  constant_temp[, c("iso3c", colnames(constant_temp)[col_start:ncol(constant_temp)])]
country_daily_excess_deaths <- merge(country_daily_excess_deaths, constant_temp, by = 'iso3c', all.x = T)

# Step 10: calculate distance averages  ---------------------------------------

# 0. Define variables to use (remove seroprevalence and daily excess deaths here, as data is too sparse (and missingness-prone) to be used for distance-weighted averages up to the present day):
region_average_vars_ts <- c("daily_tests_per_100k",
                            "daily_covid_cases_per_100k", "daily_covid_deaths_per_100k", "daily_positive_rate", "daily_excess_deaths_per_100k")


# 1. get distance dyads (source: http://www.cepii.fr/cepii/en/bdd_modele/presentation.asp?id=6):
dist_dy <- data.frame(read_xls("source-data/dist_cepii.xls"))

# # Don't use distances longer than 5000 km
# dist_dy <- dist_dy[dist_dy$dist <= 5000, ]

# Applying a few fixes to iso codes:
# DRC, Montenegro, Romania, Serbia, Timor Leste, South Sudan
dist_dy$iso_d[dist_dy$iso_d == "ROM"] <- "ROU" # Romania
dist_dy$iso_o[dist_dy$iso_o == "ROM"] <- "ROU" # 
dist_dy$iso_d[dist_dy$iso_d == "ZAR"] <- "COD" # Congo
dist_dy$iso_o[dist_dy$iso_o == "ZAR"] <- "COD" # 
dist_dy$iso_d[dist_dy$iso_d == "YUG"] <- "SRB" # Serbia
dist_dy$iso_o[dist_dy$iso_o == "YUG"] <- "SRB" # 
dist_dy$iso_d[dist_dy$iso_d == "TMP"] <- "TLS" # Timor-Leste
dist_dy$iso_o[dist_dy$iso_o == "TMP"] <- "TLS" #

# Montenegro (use SRB values)
temp <- dist_dy[dist_dy$iso_d == "SRB" | dist_dy$iso_o == "SRB", ]
temp$iso_d[temp$iso_d == "SRB"] <- "MNE"
temp$iso_o[temp$iso_o == "SRB"] <- "MNE"
dist_dy <- rbind(dist_dy, temp)

# Kosvo (use SCG values)
temp <- dist_dy[dist_dy$iso_d == "SRB" | dist_dy$iso_o == "SRB", ]
temp$iso_d[temp$iso_d == "SRB"] <- "KSV"
temp$iso_o[temp$iso_o == "SRB"] <- "KSV"
dist_dy <- rbind(dist_dy, temp)

# South Sudan (use SUD values)
temp <- dist_dy[dist_dy$iso_d == "SDN" | dist_dy$iso_o == "SDN", ]
temp$iso_d[temp$iso_d == "SDN"] <- "SSD"
temp$iso_o[temp$iso_o == "SDN"] <- "SSD"
dist_dy <- rbind(dist_dy, temp)

# Subnational units are not included in country-population-center-distance weighted calculations. If one wishes to try this, uncomment the below code:
# subnational_units <- unique(country_daily_excess_deaths$iso3c[nchar(country_daily_excess_deaths$iso3c) > 3])
# 
# for(i in subnational_units){
# temp_iso3c <- unlist(substr(i, 1, 3))
# temp <- dist_dy[dist_dy$iso_d == temp_iso3c | dist_dy$iso_o == temp_iso3c, ]
# temp$iso_d[temp$iso_d == temp_iso3c] <- i
# temp$iso_o[temp$iso_o == temp_iso3c] <- i
# dist_dy <- rbind(dist_dy, temp)
# }

dist_dy$subregion <- countrycode(dist_dy$iso_d, "iso3c", "un.regionsub.name")
dist_dy$subregion[dist_dy$iso_d == "TWN"] <- "Eastern Asia"
dist_dy$subregion[dist_dy$iso_d == "KSV"] <- "Southern Europe"
dist_dy$subregion[dist_dy$iso_d == "ANT"] <- "Latin America and the Caribbean"
dist_dy <- dist_dy[!dist_dy$iso_d == 'PAL', ] 

# 2. define function to get average of neighbouring countries (within region):
contig_ave <- function(var = region_average_vars[1],
                       t_iso3c = unique(country_daily_excess_deaths$iso3)[1],
                       dist_dy,
                       country_daily_excess_deaths,
                       omit_subnational = T){
    temp <- country_daily_excess_deaths %>% 
      filter(iso3c %in% unlist(dist_dy[dist_dy$iso_o == t_iso3c & dist_dy$contig == 1, "iso_d"]))
    if(omit_subnational == T){
      temp <- temp[nchar(temp$iso3c) == 3, ]
    }
    
    if(nrow(temp) == 0){
      return(rep(NA, length(unique(country_daily_excess_deaths$date))))
    }

    return(
      unlist(lapply(unique(temp$date), FUN = function(i){
        mean(temp[temp$date == i, p], na.rm = T)
      })))
}

# 3. define function to get population and distance-weighted average
dist_ave <- function(vars = region_average_vars_ts,
                     t_iso3c,
                     dist_dy,
                     country_daily_excess_deaths){

  temp_df <- left_join(country_daily_excess_deaths[country_daily_excess_deaths$iso3c != t_iso3c, c(vars, "iso3c", "date", "subregion", "population")], dist_dy[dist_dy$iso_o == t_iso3c, c("iso_d", "dist", "subregion")], by= c("iso3c"= "iso_d"))

  temp_df$weight <- log(temp_df$population)*1/log(temp_df$dist+1)

  
  # temp_df$weight <- 1/log(temp_df$dist+1)
  
  
    temp_df$weight[is.na(temp_df$dist)] <- 0

  # Uncomment below to restrict distance-weighted averages to sub-region:
  # temp_df$weight[temp_df$subregion.x != temp_df$subregion.y] <- 0
  
  res <- lapply(vars, function(v){
    unlist(lapply(unique(temp_df$date),
                  FUN = function(i){
                    X = temp_df[temp_df$date == i,
                                c(v, "weight")]
                    weighted.mean(x= X[, 1],
                                  w= X[, 2],
                                  na.rm = T)
                  }))
  })
  
  res <- cbind.data.frame(res)
  colnames(res) <- vars
  
  return(res)
}
# This function takes a list of variables, an iso3c code, a distance matrix, and a data frame with population, iso3c, and data on the variables in question

# 4. apply over variables and countries 
country_daily_excess_deaths <- country_daily_excess_deaths[order(country_daily_excess_deaths$date), ]

# Load a progress bar (this takes some time)
library(progress)
pb <- progress_bar$new(
  format = "  calculating [:bar] :percent eta: :eta",
  total = length(unique(country_daily_excess_deaths$iso3c)), 
  clear = FALSE, width= 60)

# Loop over countries for time-varying variables...
country_daily_excess_deaths[, paste0(region_average_vars_ts, "_dist_average")] <- NA
country_daily_excess_deaths[, paste0(region_average_vars_ts, "_contiguous_country_average")] <- NA

for(j in unique(country_daily_excess_deaths$iso3c)){
  
  # If we have distance data
  if(j %in% unique(dist_dy$iso_o)){

    # Calculate distance-weighted average
    country_daily_excess_deaths[country_daily_excess_deaths$iso3c == j, paste0(region_average_vars_ts, "_dist_average")] <- dist_ave(var = region_average_vars_ts,
               t_iso3c = j,
               dist_dy,
               country_daily_excess_deaths)

    # And mean of neighbouring countries
    for(p in region_average_vars_ts){
      country_daily_excess_deaths[country_daily_excess_deaths$iso3c == j, paste0(p, "_contiguous_country_average")] <- contig_ave(var = p,
             t_iso3c = j,
             dist_dy,
             country_daily_excess_deaths)
    }
    
    # Incrementing the progress bar counter for each country-variable
    pb$tick()
  }
}

# Loop over time-constant variables...
constant_temp <- country_daily_excess_deaths[, c("iso3c", "region", "subregion", "economist_region", "population", "date", region_average_vars_c)]
constant_temp <- constant_temp[!duplicated(constant_temp$iso3c), ]
constant_temp$date <- "static"
col_start <- ncol(constant_temp) + 1

for(p in region_average_vars_c){
  constant_temp[, paste0(p, "_dist_average")] <- NA
  constant_temp[, paste0(p, "_contiguous_country_average")] <- NA
  # An countries
  for(j in unique(constant_temp$iso3c)){
    
    # If we have distance data
    if(j %in% unique(dist_dy$iso_o)){
      
      # Calculate distance-weighted average
      constant_temp[constant_temp$iso3c == j, 
                                  paste0(p, "_dist_average")] <-
        dist_ave(var = p,
                 t_iso3c = j,
                 dist_dy,
                 constant_temp)
      
      # And mean of neighbouring countries
      constant_temp[constant_temp$iso3c == j, 
                                  paste0(p, "_contiguous_country_average")] <-
        contig_ave(var = p,
                   t_iso3c = j,
                   dist_dy,
                   constant_temp)
    }
 
  }
}

# Merge in these columns:
constant_temp <-  constant_temp[, c("iso3c", colnames(constant_temp)[col_start:ncol(constant_temp)])]
country_daily_excess_deaths <- merge(country_daily_excess_deaths, constant_temp, by = 'iso3c', all.x = T)

# Make sure subregional data for a country has country-level regional and distance-weighted averages:

# Get these columns:
average_columns <- grep("_average", colnames(country_daily_excess_deaths), value = T)

# Get countries with subnational units:
sub_iso3c <- unique(country_daily_excess_deaths$iso3c[nchar(country_daily_excess_deaths$iso3c) > 3])
sub_iso3c <- unique(unlist(substr(sub_iso3c, 1, 3)))

# This loops over columns
for(i in average_columns){
  
  # The countries which have subnational units
  for(j in sub_iso3c){
    
    # Replacing the observations with the observation for that country and date (or NA, if the country has no non-NA observations for that date) for all the subunits within that same country on the same date:
  country_daily_excess_deaths[unlist(substr(country_daily_excess_deaths$iso3c, 1, 3)) == j, i] <- ave(  country_daily_excess_deaths[unlist(substr(country_daily_excess_deaths$iso3c, 1, 3)) == j, i], country_daily_excess_deaths[unlist(substr(country_daily_excess_deaths$iso3c, 1, 3)) == j, "date"], FUN = function(x) c(na.omit(x), NA)[1])
  
  }
}
  
# Finally: Ensure distance-based seroprevalence estimates are non-decreasing

# Select columns:
sero_columns <- c("sero_nat_30d_wma_interpolated_region_average",
                  "sero_nat_30d_wma_interpolated_sub_region_average",
                  "sero_nat_30d_wma_interpolated_econ_region_average",
                  "sero_reg_or_nat_30d_wma_interpolated_region_average",
                  "sero_reg_or_nat_30d_wma_interpolated_sub_region_average",
                  "sero_reg_or_nat_30d_wma_interpolated_econ_region_average")

# Sort data by date
country_daily_excess_deaths <- country_daily_excess_deaths[order(country_daily_excess_deaths$date), ]

# Cycle through columns and then within each country, get cumulative maximum, respecting NAs:
for(i in sero_columns){
  country_daily_excess_deaths[, i] <- ave(country_daily_excess_deaths[, i],
                                          country_daily_excess_deaths$date, 
                                          FUN = function(x){
                                            temp <- x
                                            x <- cummax(ifelse(is.na(x), 0, x))
                                            x[is.na(temp)] <- NA
                                            x
                                          })
}

# Step 11: Add back in countries who have backward engineered covid deaths, add two-week lag columns for vaccinations and various other feature-engineering   ---------------------------------------

# Add omitted countries data back in (these were the ones who have backward-engineered their covid deaths):
country_daily_excess_deaths <- country_daily_excess_deaths[order(country_daily_excess_deaths$row_order), ]

country_daily_excess_deaths[country_daily_excess_deaths$iso3c %in% omit_iso3c, omit_cols] <- omit_iso3c_data

country_daily_excess_deaths$row_order <- NULL


 # Add two-week lags of time-varying features related to vaccinations
vaccination_variables <- c("daily_vaccinations_per_100k",
                         "vaccinated_pct",
                         "fully_vaccinated_pct",
                         "cumulative_daily_vaccinations_per_100k")

vaccination_variables <- vaccination_variables[vaccination_variables %in% colnames(country_daily_excess_deaths)]

# Order dataset by date:
country_daily_excess_deaths <- country_daily_excess_deaths[order(country_daily_excess_deaths$date), ] 

# Cycle through these indicators and add 2-week lag:
for(i in vaccination_variables){
  country_daily_excess_deaths[, paste0(i, "_lagged_two_weeks")] <- ave(country_daily_excess_deaths[, i], country_daily_excess_deaths$iso3c, FUN = function(x){
    c(rep(NA, 13), x)[1:length(x)]
  })
}

### Add engineered features:

# Vaccinated per population over 65:
country_daily_excess_deaths$vaccinated_pct_over_pop_65 <- NA
country_daily_excess_deaths$vaccinated_pct_over_pop_65[!is.na(country_daily_excess_deaths$aged_65_older)] <- (100-country_daily_excess_deaths$vaccinated_pct[!is.na(country_daily_excess_deaths$aged_65_older)]) / country_daily_excess_deaths$aged_65_older[!is.na(country_daily_excess_deaths$aged_65_older)]

country_daily_excess_deaths$fully_vaccinated_pct_over_pop_65 <- NA
country_daily_excess_deaths$fully_vaccinated_pct_over_pop_65[!is.na(country_daily_excess_deaths$aged_65_older)] <- (100-country_daily_excess_deaths$fully_vaccinated_pct[!is.na(country_daily_excess_deaths$aged_65_older)]) / country_daily_excess_deaths$aged_65_older[!is.na(country_daily_excess_deaths$aged_65_older)]

# Covid deaths x vaccination rate
country_daily_excess_deaths$vaccinated_pct_over_covid_deaths <- (100-country_daily_excess_deaths$vaccinated_pct)* country_daily_excess_deaths$daily_covid_deaths_per_100k

# Covid cases x vaccination rate
country_daily_excess_deaths$vaccinated_pct_over_covid_cases <- (100-country_daily_excess_deaths$vaccinated_pct)* country_daily_excess_deaths$daily_covid_cases_per_100k

# Covid deaths x fully vaccination rate
country_daily_excess_deaths$fully_vaccinated_pct_over_covid_deaths <- (100-country_daily_excess_deaths$fully_vaccinated_pct)* country_daily_excess_deaths$daily_covid_deaths_per_100k

# Covid cases x fully vaccination rate
country_daily_excess_deaths$fully_vaccinated_pct_over_covid_cases <- (100-country_daily_excess_deaths$fully_vaccinated_pct)* country_daily_excess_deaths$daily_covid_cases_per_100k

# Covid deaths x vaccination rate over 65s
country_daily_excess_deaths$vaccinated_pct_65_plus_over_covid_deaths <- (100-country_daily_excess_deaths$vaccinated_pct_over_pop_65)* country_daily_excess_deaths$daily_covid_deaths_per_100k

# Covid deaths x full vaccination rate over 65s 
country_daily_excess_deaths$fully_vaccinated_pct_65_plus_over_covid_deaths <- (100-country_daily_excess_deaths$vaccinated_pct_over_pop_65)* country_daily_excess_deaths$daily_covid_deaths_per_100k

# Indicator for country where the virus was first discovered:
country_daily_excess_deaths$virus_discovery_country <- country_daily_excess_deaths$iso3c == "CHN"

# Adding linearly imputed vaccinations data columns (as some do not release regularly - this assumes linear trend between known values):
for(i in c("vaccinated_pct",
           "fully_vaccinated_pct",
           "cumulative_daily_vaccinations_per_100k",
           "vaccinated_pct_lagged_two_weeks",
           "fully_vaccinated_pct_lagged_two_weeks",
           "cumulative_daily_vaccinations_per_100k_lagged_two_weeks",
           "vaccinated_pct_over_pop_65",
           "fully_vaccinated_pct_over_pop_65")){
  
  country_daily_excess_deaths[, paste0(i, "_intp")] <- ave(country_daily_excess_deaths[, i], country_daily_excess_deaths$iso3c, FUN = function(x){
    zoo::na.approx(x, na.rm = F)
  })
}

# Adding IMF's estimates of GDP at constant prices in 2019
# Source: https://www.imf.org/en/Publications/WEO/weo-database/2021/April/download-entire-database (indicator - 'NGDPRPPPPC')
imf_gdppc <- read_csv('source-data/imf_gdppc_ppp.csv')
country_daily_excess_deaths <- merge(country_daily_excess_deaths, imf_gdppc, by = 'iso3c', all.x = T)

# Adding temporal recency indicator:
country_daily_excess_deaths$temporal_recency <- country_daily_excess_deaths$date - Sys.Date()

add_china_approx_test <- F
if(add_china_approx_test){
# If selected, adds prior for positive rate in China based on total tests done by August 9th 2021: https://www.statista.com/statistics/1028731/covid19-tests-select-countries-worldwide/. One then conservatively assume half of these tests were done in the first 6 months of the 2020, with the rest after, distributed evenly by day, and that testing continues at this rate thereafter. In reality, tests are likely higher on days with more cases (as their purpose is to diagnose them):
daily_tests_chn <- (1/2)*160000000/(as.numeric(as.Date("2021-08-09")-as.Date("2020-06-01")))

country_daily_excess_deaths$daily_positive_rate[country_daily_excess_deaths$iso3c == "CHN" & country_daily_excess_deaths$date >= as.Date("2020-06-01") ] <- 100*country_daily_excess_deaths$daily_covid_cases[country_daily_excess_deaths$iso3c == "CHN" & country_daily_excess_deaths$date >= as.Date("2020-06-01") ] / daily_tests_chn

# Note: We do not recommend one attempt the same for total tests and tests per 100k, because these tends to vary by outbreak severity and imputing a stable number would be thus misleading. The imputation above is more reasonable because the variation is all below the lower bound of test positivity rates as measured by OWA (i.e. we are simply adding the information that tests in China are overwhelmingly unlikely to result in a positive official case).
}

inv_pop_weighted <- F
if(inv_pop_weighted){
# If selected, generates inverse Log-population weighted version of regional, contiguous and distance-weighted country averages (as smaller countries are likely to respond strongly to their neighbours than larger ones)
for(i in c(grep("contiguous", colnames(country_daily_excess_deaths), value = T), 
           grep("dist", colnames(country_daily_excess_deaths), value = T))){
  country_daily_excess_deaths[, paste0(i, "_inv_pop_weighted")] <- country_daily_excess_deaths[, i]/log(country_daily_excess_deaths$population)
}
}

# Step 11: export data frame to RDS  ---------------------------------------

# Export dataframe to RDS
# fwrite(country_daily_excess_deaths, "output-data/country_daily_excess_deaths_with_covariates.csv")

saveRDS(country_daily_excess_deaths, "output-data/country_daily_excess_deaths_with_covariates.RDS")
