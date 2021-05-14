# Step 1: import libraries ---------------------------------------

# Import libraries
library(tidyverse)
library(data.table)
library(lubridate)
library(countrycode)
library(readr)
options(scipen=999)

# Step 2: import excess deaths data and convert into a daily time series ---------------------------------------

# Import excess deaths
all_weekly_excess_deaths <- fread("https://raw.githubusercontent.com/TheEconomist/covid-19-excess-deaths-tracker/master/output-data/excess-deaths/all_weekly_excess_deaths.csv")
all_monthly_excess_deaths <- fread("https://raw.githubusercontent.com/TheEconomist/covid-19-excess-deaths-tracker/master/output-data/excess-deaths/all_monthly_excess_deaths.csv")
all_quarterly_excess_deaths <- fread("https://raw.githubusercontent.com/TheEconomist/covid-19-excess-deaths-tracker/master/output-data/excess-deaths/all_quarterly_excess_deaths.csv")

# Create list of countries with weekly excess deaths
weekly_excess_countries <- all_weekly_excess_deaths %>%
  filter(country == region) %>%
  pull(country) %>%
  unique()

# Create list of countries with monthly excess deaths
monthly_excess_countries <- all_monthly_excess_deaths %>%
  filter(country == region) %>%
  pull(country) %>%
  unique()

# Create list of countries with quarterly excess deaths
quarterly_excess_countries <- all_quarterly_excess_deaths %>%
  filter(country == region) %>%
  pull(country) %>%
  unique()

# Convert weekly excess deaths to a daily time series
weekly_to_daily_excess_deaths <- expand.grid(start_date = seq(as.Date("2020-01-01"), as.Date(today()), by = "days"),
                                             country = weekly_excess_countries) %>%
  mutate(country_code = countryname(country,destination="iso3c"),
         start_date = as.Date(start_date)) %>% # Bind on the start date of each week
  left_join(all_weekly_excess_deaths %>% 
              filter(region %in% weekly_excess_countries,
                     !(country == "United States" & region == "Georgia")) %>% # Remove the US state of Georgia
              dplyr::select(country,start_date,days,population,total_deaths,expected_deaths,excess_deaths) %>%
              mutate(start_date = as.Date(start_date))) %>%
  mutate(end_date = start_date) %>%
  left_join(all_weekly_excess_deaths %>% # Bind on the end date of each week
              mutate(label = "in_sample") %>%
              filter(region %in% weekly_excess_countries,
                     !(country == "United States" & region == "Georgia")) %>% # Remove the US state of Georgia
              dplyr::select(country,end_date,label) %>%
              mutate(end_date = as.Date(end_date))) %>%
  group_by(country) %>%
  fill(label,.direction = "up") %>%
  drop_na(label) %>% # Remove all dates after the end of the excess deaths sample
  dplyr::select(-c(end_date,label)) %>%
  fill(days,population,total_deaths,expected_deaths,excess_deaths) %>% # Fill down all start dates to the entire week
  ungroup() %>%
  drop_na() %>%
  mutate(date = start_date,
         daily_total_deaths = total_deaths / days,
         daily_expected_deaths = expected_deaths / days,
         daily_excess_deaths = excess_deaths / days,
         daily_total_deaths_per_100k = ((total_deaths / days) / population) * 100000,
         daily_expected_deaths_per_100k = ((expected_deaths / days) / population) * 100000,
         daily_excess_deaths_per_100k = ((excess_deaths / days) / population) * 100000) %>%
  dplyr::select(date,country_code,daily_total_deaths,daily_total_deaths_per_100k,
                daily_expected_deaths,daily_expected_deaths_per_100k,
                daily_excess_deaths,daily_excess_deaths_per_100k)

# Convert monthly excess deaths to a daily time series
monthly_to_daily_excess_deaths <- expand.grid(start_date = seq(as.Date("2020-01-01"), as.Date(today()), by = "days"),
                                              country = monthly_excess_countries) %>%
  mutate(country_code = countryname(country,destination="iso3c"),
         start_date = as.Date(start_date)) %>% # Bind on the start date of each month
  left_join(all_monthly_excess_deaths %>% 
              filter(region %in% monthly_excess_countries,
                     !(country == "United States" & region == "Georgia")) %>% # Remove the US state of Georgia
              dplyr::select(country,start_date,days,population,total_deaths,expected_deaths,excess_deaths) %>%
              mutate(start_date = as.Date(start_date))) %>%
  mutate(end_date = start_date) %>%
  left_join(all_monthly_excess_deaths %>% # Bind on the end date of each month
              mutate(label = "in_sample") %>%
              filter(region %in% monthly_excess_countries,
                     !(country == "United States" & region == "Georgia")) %>% # Remove the US state of Georgia
              dplyr::select(country,end_date,label) %>%
              mutate(end_date = as.Date(end_date))) %>%
  group_by(country) %>%
  fill(label,.direction = "up") %>%
  drop_na(label) %>% # Remove all dates after the end of the excess deaths sample
  dplyr::select(-c(end_date,label)) %>%
  fill(days,population,total_deaths,expected_deaths,excess_deaths) %>% # Fill down all start dates to the entire month
  ungroup() %>%
  drop_na() %>%
  mutate(date = start_date,
         daily_total_deaths = total_deaths / days,
         daily_expected_deaths = expected_deaths / days,
         daily_excess_deaths = excess_deaths / days,
         daily_total_deaths_per_100k = ((total_deaths / days) / population) * 100000,
         daily_expected_deaths_per_100k = ((expected_deaths / days) / population) * 100000,
         daily_excess_deaths_per_100k = ((excess_deaths / days) / population) * 100000) %>%
  dplyr::select(date,country_code,daily_total_deaths,daily_total_deaths_per_100k,
                daily_expected_deaths,daily_expected_deaths_per_100k,
                daily_excess_deaths,daily_excess_deaths_per_100k)

# Convert quarterly excess deaths to a daily time series
quarterly_to_daily_excess_deaths <- expand.grid(start_date = seq(as.Date("2020-01-01"), as.Date(today()), by = "days"),
                                              country = quarterly_excess_countries) %>%
  mutate(country_code = countryname(country,destination="iso3c"),
         start_date = as.Date(start_date)) %>% # Bind on the start date of each month
  left_join(all_quarterly_excess_deaths %>% 
              filter(region %in% quarterly_excess_countries,
                     !(country == "United States" & region == "Georgia")) %>% # Remove the US state of Georgia
              dplyr::select(country,start_date,days,population,total_deaths,expected_deaths,excess_deaths) %>%
              mutate(start_date = as.Date(start_date))) %>%
  mutate(end_date = start_date) %>%
  left_join(all_quarterly_excess_deaths %>% # Bind on the end date of each month
              mutate(label = "in_sample") %>%
              filter(region %in% quarterly_excess_countries,
                     !(country == "United States" & region == "Georgia")) %>% # Remove the US state of Georgia
              dplyr::select(country,end_date,label) %>%
              mutate(end_date = as.Date(end_date))) %>%
  group_by(country) %>%
  fill(label,.direction = "up") %>%
  drop_na(label) %>% # Remove all dates after the end of the excess deaths sample
  dplyr::select(-c(end_date,label)) %>%
  fill(days,population,total_deaths,expected_deaths,excess_deaths) %>% # Fill down all start dates to the entire month
  ungroup() %>%
  drop_na() %>%
  mutate(date = start_date,
         daily_total_deaths = total_deaths / days,
         daily_expected_deaths = expected_deaths / days,
         daily_excess_deaths = excess_deaths / days,
         daily_total_deaths_per_100k = ((total_deaths / days) / population) * 100000,
         daily_expected_deaths_per_100k = ((expected_deaths / days) / population) * 100000,
         daily_excess_deaths_per_100k = ((excess_deaths / days) / population) * 100000) %>%
  dplyr::select(date,country_code,daily_total_deaths,daily_total_deaths_per_100k,
                daily_expected_deaths,daily_expected_deaths_per_100k,
                daily_excess_deaths,daily_excess_deaths_per_100k)

# Bind both weekly, monthly and quarterly excess deaths together into a daily time series
daily_excess_deaths <- bind_rows(weekly_to_daily_excess_deaths,
                                 monthly_to_daily_excess_deaths,
                                 quarterly_to_daily_excess_deaths) %>%
  filter(!country_code %in% c("ARM","AZE"))# Remove countries involved in Nagarno-Karabkh war
daily_excess_deaths$iso3c <- daily_excess_deaths$country_code
daily_excess_deaths$country_code <- NULL

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
         fully_vaccinated_pct = people_fully_vaccinated_per_hundred) %>%
  filter(date >= as.Date("2020-01-01"),
         !str_detect(iso3c,"OWID")) %>%
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
                fully_vaccinated_pct)

# Join excess deaths onto dataframe
country_daily_excess_deaths <- country_daily_data %>%
  ungroup() %>%
  left_join(daily_excess_deaths)

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
# Sort
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
# country_daily_excess_deaths[order(country_daily_excess_deaths$date), ]
for(i in c("daily_tests", "daily_covid_cases", "daily_covid_deaths", "daily_vaccinations")){
  country_daily_excess_deaths[, paste0("cumulative_", i, "_per_100k")] <- ave(
    country_daily_excess_deaths[, i], country_daily_excess_deaths$iso3c, 
    FUN = function(x){cumsum(ifelse(is.na(x), 0, x))}
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
wdi$iso2c <- NULL
wdi$country <- NULL

# Only latest observation
wdi <- wdi[order(wdi$year), ]
wdi <- wdi[!is.na(wdi$iso3c), ]

for(i in setdiff(colnames(wdi), c("year", "iso3c"))){
  wdi[, i] <- ave(wdi[, i], wdi$iso3c, 
                  FUN = function(x){
    if(max(which(!is.na(x))) == -Inf){
      NA
    } else {
    x[max(which(!is.na(x)))]
    }
  })
}

# Collapse rows to one per country (multiple happens when data comes from different years)
for(i in setdiff(colnames(wdi), "iso3c")){
  wdi[, i] <- ave(wdi[, i], wdi$iso3c, FUN = function(x) mean(x, na.rm = T))
}
wdi <- unique(wdi)

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

# Import estimated IFR if infections random and no difference in care (source: https://www.economist.com/graphic-detail/2020/11/16/why-rich-countries-are-so-vulnerable-to-covid-19 and github: https://github.com/TheEconomist/covid-19-age-adjusted-ifr):
ifr <- read_csv("https://raw.githubusercontent.com/TheEconomist/covid-19-age-adjusted-ifr/main/ifr_by_iso2c.csv")
ifr$demography_adjusted_ifr <- ifr$area_ifr

ifr <- ifr %>% 
  mutate(iso3c = countrycode(iso2c, "iso2c", "iso3c")) %>%
  select(demography_adjusted_ifr, iso3c)

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

# Add to list of static datasets:
static_data[[length(static_data) + 1]] <- unwto


# Step 5: import other data sources (country-level, time-varying) ---------------------------------------
time_varying_data <- list()

# Google mobility reports:
mob <- fread("https://www.gstatic.com/covid19/mobility/Global_Mobility_Report.csv")

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

# Add polity response data (sourc: https://raw.githubusercontent.com/OxCGRT/covid-policy-tracker/master/data/OxCGRT_latest.csv)
ox <- data.frame(fread("https://raw.githubusercontent.com/OxCGRT/covid-policy-tracker/master/data/OxCGRT_latest.csv"))

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
sero <- read_csv("source-data/serotracker.csv")

# Restrict to national or regional surveys
sero <- sero[sero$`Grade of Estimate Scope` %in% c("National", "Regional"), ]

# Restrict to random or quasi-random sample
sero$`Sampling Method`[sero$`Prevalence Estimate Name` == "200609_Bergamo_HealthAgency_GenPop"] <- 'Convenience' # miss-coded in original.
sero <- sero[sero$`Sampling Method` %in% c("Sequential", "Simplified probability", "Stratified probability", "Stratified non-probability"), ]

# Restrict to general adult populations
sero <- sero[sero$`Sample Frame (groups of interest)` %in% c("Household and community samples", "Residual sera ", "Multiple populations", "Blood donors") & sero$`Sample Frame (age)` %in% c("Adults (18-64 years)",
                                                                                                                                                                                             "Multiple groups",
                                                                                                                                                                                             "Not reported"), ]

# Add date
sero$start_date <- NA
sero$end_date <- NA

for(i in 1:nrow(sero)){
  sero$start_date[i] <- paste0(unlist(strsplit(sero$`Study Dates`[i], " - ")), "/2020")[1]
  sero$end_date[i] <- paste0(unlist(strsplit(sero$`Study Dates`[i], " - ")), "/2020")[2]
}
sero$start_date <- as.Date(sero$start_date, format = "%m/%d/%Y")
sero$end_date <- as.Date(sero$end_date, format = "%m/%d/%Y")
sero$start_date[is.na(sero$start_date)] <- sero$end_date[is.na(sero$start_date)]

# For studies going from 2020 to 2021:
sero$end_date[sero$end_date < sero$start_date] <- as.Date(sero$end_date[sero$end_date < sero$start_date] + 365)

sero$date <- sero$start_date + floor((sero$end_date-sero$start_date)/2)

# Fix study date year to include 2021 surveys:
sero$date_created <- as.Date(sero$`Date Created`, format = "%d/%m/%Y")
sero$date[as.Date(sero$date_created - 365) > sero$end_date & sero$date_created > as.Date("2021-01-01")] <- as.Date(sero$date[as.Date(sero$date_created - 365) > sero$end_date & sero$date_created > as.Date("2021-01-01")] + 365)

# Generate iso3c, rename and select columns
sero <- sero %>% mutate(
  iso3c = countrycode(Country, "country.name", "iso3c"),
  seroprevalence = as.numeric(gsub("%", "", Seroprevalence)),
  seroprevalence_N = `Denominator Value`,
  seroprevalence_study_level = `Grade of Estimate Scope`
  ) %>% select(
  iso3c,
  date,
  seroprevalence,
  seroprevalence_N,
  seroprevalence_study_level
)

sero$seroprevalence_national <- sero$seroprevalence_regional_or_national <- sero$seroprevalence
sero$seroprevalence_national[!sero$seroprevalence_study_level == "National"] <- NA

# Average serostudies that have the same date by N:
sero <- data.frame(sero)
sero$id <- paste0(sero$iso3c, "_", sero$date)
for(i in unique(sero$id)){
    sero[sero$id == i, "seroprevalence_regional_or_national"] <- 
      weighted.mean(sero[sero$id == i,
                         "seroprevalence_regional_or_national"],
                    sero[sero$id == i, "seroprevalence_N"])
    
    sero[sero$id == i,
         "seroprevalence_national"] <- 
      weighted.mean(sero[sero$id == i & 
                           sero$seroprevalence_study_level == "National",
                         "seroprevalence_national"],
                    sero[sero$id == i & 
                           sero$seroprevalence_study_level == "National",
                         "seroprevalence_N"])
    
    # This combines the N of surveys completed in the same day in the same country
    sero[sero$id == i, "seroprevalence_N"] <- sum(
      sero[sero$id == i, "seroprevalence_N"])
}

sero <- unique(sero[, c("iso3c", "date",
                        "seroprevalence_regional_or_national",
                        "seroprevalence_national",
                        "seroprevalence_N")])


# Calculate 3-month sample-size-weighted average by country
sero <- merge(sero[sero$iso3c %in% country_daily_excess_deaths$iso3c, ], unique(country_daily_excess_deaths[, c("iso3c", "date", "region", "cumulative_daily_covid_cases_per_100k", "population")]), all = T)

# Define a function to calculate weighted rolling average
weighted_rolling_average <- function(df, value, weights, 
                         group = "iso3c",
                         window_span = 45,
                         use_loess = F){
  
  averages <- rep(NA, nrow(df))
  
  for(j in unique(df[, group])){
    
    x <- df[df[, group] == j, value]
    w <- df[df[, group] == j, weights]
    
    # Either us loess, or
    if(use_loess){
      fit_df <- data.frame(y = 1:length(x),
                           x,
                           w)
      if(nrow(na.omit(fit_df)) == 0){
        res <- rep(NA, length(x))
      } else {
        fit_df <<- fit_df
      res <- predict(loess(x ~ y, weights = w, data = fit_df, span = 1),
                     newdata = fit_df)
      }
    } else {
    
    # Loop over observations and calculate weighted average
    
    # Define window to use for average
    window <- rep(window_span, length(x))
    window[1:window_span] <- 1:window_span
    window[length(x):(length(x)-window_span+1)] <- 1:window_span
    window <- window - 1
    
    res <- rep(NA, length(x))
    for(i in 1:length(x)){
        res[i] <- weighted.mean(x = x[(i-window[i]):(i+window[i])], 
                                w = w[(i-window[i]):(i+window[i])],
                                na.rm = T)
      }
      
    }
    averages[df[, group] == j] <- res
  }
    
  return(averages)}

# Sort data by date and define weights
sero <- sero[order(sero$date), ]
sero$weights <- log(sero$seroprevalence_N)

# For all serosurveys
sero$seroprevalence_reg_or_nat_30d_wma <- weighted_rolling_average(df = sero, 
           value = "seroprevalence_regional_or_national", 
           weights = "weights",
           window_span = 30)

# For national serosurveys only
sero$seroprevalence_nat_30d_wma <- weighted_rolling_average(df = sero, 
                                                                                       value = "seroprevalence_national", 
                                                                                       weights = "weights",
                                                                                       window_span = 30)

# Specifiy seroprevalence to be 0 before first case:
sero$seroprevalence_reg_or_nat_30d_wma[sero$cumulative_daily_covid_cases_per_100k == 0  | is.na(sero$cumulative_daily_covid_cases_per_100k)] <- 0
sero$seroprevalence_nat_30d_wma[sero$cumulative_daily_covid_cases_per_100k == 0  | is.na(sero$cumulative_daily_covid_cases_per_100k) ] <- 0

# Linearly interpolate change in seroprevalence
sero <- sero[order(sero$date), ]
library(zoo)
sero$seroprevalence_reg_or_nat_30d_wma_interpolated <- ave(sero$seroprevalence_reg_or_nat_30d_wma, sero$iso3c,
                                              FUN = function(x){
                                                if(sum(!is.na(x)) < 0){
                                                  x
                                                } else {
                                                  na.approx(x, na.rm = F)
                                                }
                                              })
sero$seroprevalence_nat_30d_wma_interpolated <- ave(sero$seroprevalence_nat_30d_wma, sero$iso3c, FUN = function(x){
                                                             if(sum(!is.na(x)) < 0){
                                                               x
                                                             } else {
                                                               na.approx(x, na.rm = F)
                                                             }
                                                           })

# Specify seroprevalence to be monotonically increasing:
sero <- sero[order(sero$date), ]
sero$seroprevalence_reg_or_nat_30d_wma_interpolated <- ave(sero$seroprevalence_reg_or_nat_30d_wma_interpolated, sero$iso3c,
                                              FUN = function(x){
                                                cummax(ifelse(is.na(x), 0, x))
                                              })

sero$seroprevalence_nat_30d_wma_interpolated <- ave(sero$seroprevalence_nat_30d_wma_interpolated, sero$iso3c,
                                              FUN = function(x){
                                                cummax(ifelse(is.na(x), 0, x))
                                              })

# Transform to 30-day average change in seroprevalence
library(data.table)
sero <- sero[order(sero$date), ]
sero$sero_nat_delta <- ave(sero$seroprevalence_nat_30d_wma_interpolated,
                           sero$iso3c,
                           FUN = function(x){
                             x <- x - c(0, x)[1:length(x)]
                             frollmean(x, n=30, align = "center", 
                                       na.rm = T)
                           })
sero$sero_nat_or_reg_delta <- ave(sero$seroprevalence_reg_or_nat_30d_wma_interpolated,
                           sero$iso3c,
                           FUN = function(x){
                             x <- x - c(0, x)[1:length(x)]
                             frollmean(x, n=30, align = "center", 
                                       na.rm = T)
                           })


#ggplot(sero, aes(x=date, y=sero_nat_or_reg_delta,
#                 col = iso3c))+geom_line()+theme(legend.position = "none")

# Rename a few columns for clarity
sero$sero_delta_country_surveys <- sero$sero_nat_delta
sero$sero_delta_country_or_country_region_surveys <- sero$sero_nat_or_reg_delta

# Add to time-varying data
time_varying_data[[length(time_varying_data) + 1]] <- unique(sero[, c("iso3c", "date", "sero_nat_or_reg_delta", "sero_nat_delta")])

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

# Step 7: fix regions and country names to The Economist standard   ---------------------------------------
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
    imf_economy
  )

country_daily_excess_deaths$country <- NULL
country_daily_excess_deaths <- merge(country_daily_excess_deaths, 
                                     econ_df, by = "iso3c", 
                                     all.x = T)


# Step 8: add in large subregions/non-repo excess deaths data ---------------------------------------
# China (source: https://www.bmj.com/content/372/bmj.n415):
# Data from online supplemental materials. I have requested further data but was not supplied it.
china <- read_csv("source-data/china_bmj_excess_deaths.csv", skip = 1)
china_wuhan <- china[, 1:5]
china_hubei_except_wuhan <- china[, c(1:2, 6:8)]
china_outside_hubei <- china[, c(1:2, 9:11)]

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
# With population-change multiplier:
china$expected_deaths <- china$expected_deaths*(china$population/china$mean_population_2015_2019)
# Note: supplemental figure 2 suggests the mean should work well, no strong over-time-trend in per capita death.

# To inspect our estimates for China excess deaths by area, uncomment the below chunk:
ggplot(china, aes(x=as.numeric(Week), y=100000*reporting_adjusted_deaths/population, col = "2020"))+
  geom_line()+
  geom_line(aes(y=100000*expected_deaths/population, col = "Expected"))+
  geom_hline(aes(yintercept = 0), col = "black")+ylim(c(-10, 100))+
theme_minimal()+facet_grid(.~type)+xlab("Week in 2020")+ylab("Deaths per 100k population")

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


# India:
india <- read_csv("source-data/india_daily_excess_deaths.csv")
india$iso3c <- "IND"
india$date <- as.Date(india$date, format = "%d/%m/%Y")

# Questions raised about Kerala data makes us discard it (see: https://www.indiaspend.com/covid-19/mortality-data-kerala-mumbai-too-soon-to-say-india-covid19-less-deadly-second-wave-737270)
mumbai <- india[india$country == "Mumbai", ]
# In contrast, the Mumbai data appears to check out against seroprevalence ++ and by distribution of deaths by age: https://science.thewire.in/the-sciences/covid-19-mumbai-all-cause-mortality-data-ifr-bmc-seroprevalence-survey/

# Expand this data to match date-range of big dataset:
date_range <- unique(country_daily_excess_deaths$date)
mumbai <- merge(mumbai, data.frame("date" = date_range), by = "date", all = T)
mumbai$iso3c <- "IND"

# Merge in missing columns from India data:
mumbai <- merge(mumbai, country_daily_excess_deaths[, c("date", "iso3c", setdiff(colnames(country_daily_excess_deaths), colnames(india)))], by = c("iso3c", "date"))
mumbai <- mumbai[, colnames(country_daily_excess_deaths)]

mumbai$iso3c <- "IND_Mumbai"
mumbai$is_subregion <- "YES"

# Merge this in with big data
country_daily_excess_deaths$is_subregion <- "NO"
country_daily_excess_deaths <- rbind(country_daily_excess_deaths, mumbai)


# Step 8: calculate region averages  ---------------------------------------

# Calculate regional averages by day (weighted by population)

# 0. Select definition of region (NB: 'region' are as defined by World Bank Development Indicators)
library(countrycode)
country_daily_excess_deaths$continent <- countrycode(ifelse(country_daily_excess_deaths$iso3c != "IND_Mumbai", country_daily_excess_deaths$iso3c, "IND"), "iso3c", "continent")

country_daily_excess_deaths$region <- countrycode(ifelse(country_daily_excess_deaths$iso3c != "IND_Mumbai", country_daily_excess_deaths$iso3c, "IND"), "iso3c", "region")
country_daily_excess_deaths$region[country_daily_excess_deaths$iso3c == "SHN"] <- "Europe & Central Asia" # fix for Saint Helena

# 1. Define region-average function:
region_average <- function(variable = country_daily_excess_deaths$daily_total_deaths_per_100k,
                           region = country_daily_excess_deaths$region,
                           time = country_daily_excess_deaths$date,
                           weights = log(country_daily_excess_deaths$population),
                           include_current_obs = FALSE){
  
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
                      unlist(lapply(1:nrow(x), FUN = function(p){
                             weighted.mean(x= x[-p, 1],
                                           w= x[-p, 2],
                                           na.rm = T)}))
                         })[, 1]
    }
  
  return(res)}


# 2. Define variables to calculate region averages for:
region_average_vars <- c("sero_nat_or_reg_delta",
                         "sero_nat_delta",
                         "daily_excess_deaths_per_100k",
                         "daily_tests_per_100k",
                         "daily_covid_cases_per_100k",
                         "daily_covid_deaths_per_100k",
                         "daily_positive_rate", 
                         "median_age",
                         "wdi_life_expectancy_at_birth",
                         "demography_adjusted_ifr")

# 3. Calculate by applying function defined above by region and subregion:
for(p in region_average_vars){
  country_daily_excess_deaths[, 
                              paste0(p, "_region_average")] <- region_average(variable = country_daily_excess_deaths[, p],
                                                                              include_current_obs = FALSE)
  
  country_daily_excess_deaths[, 
                              paste0(p, "_sub_region_average")] <- region_average(variable = country_daily_excess_deaths[, p],
                                                                                  region = country_daily_excess_deaths$subregion,
                                                                              include_current_obs = FALSE)
  
  country_daily_excess_deaths[, 
                              paste0(p, "_econ_region_average")] <- region_average(variable = country_daily_excess_deaths[, p],
                                                                                  region = country_daily_excess_deaths$econ_region,
                                                                                  include_current_obs = FALSE)
  
  print(paste("Region average completed for:", p))
} # NB: excluding observation in question is crucial if doing this for e.g. observed excess deaths

# Step 8: calculate distance averages  ---------------------------------------
# 1. get distance dyads (source: http://www.cepii.fr/cepii/en/bdd_modele/presentation.asp?id=6):
dist_dy <- data.frame(read_xls("source-data/dist_cepii.xls"))

# Applying a few fixes to iso codes:
# DRC, Montenegro, Romania, Serbia, Timor Leste, South Sudan
dist_dy$iso_d[dist_dy$iso_d == "ROM"] <- "ROU" # Romania
dist_dy$iso_o[dist_dy$iso_o == "ROM"] <- "ROU" # 
dist_dy$iso_d[dist_dy$iso_d == "ZAR"] <- "COD" # Congo
dist_dy$iso_o[dist_dy$iso_o == "ZAR"] <- "COD" # 
dist_dy$iso_d[dist_dy$iso_d == "SCG"] <- "SRB" # Serbia
dist_dy$iso_o[dist_dy$iso_o == "SCG"] <- "SRB" # 
dist_dy$iso_d[dist_dy$iso_d == "TMP"] <- "TLS" # Timor-Leste
dist_dy$iso_o[dist_dy$iso_o == "TMP"] <- "TLS" #

# Montenegro (use SCG values)
temp <- dist_dy[dist_dy$iso_d == "SRB" | dist_dy$iso_o == "SRB", ]
temp$iso_d[temp$iso_d == "SRB"] <- "MNE"
temp$iso_o[temp$iso_o == "SRB"] <- "MNE"
dist_dy <- rbind(dist_dy, temp)

# South Sudan (use SUD values)
temp <- dist_dy[dist_dy$iso_d == "SDN" | dist_dy$iso_o == "SDN", ]
temp$iso_d[temp$iso_d == "SDN"] <- "SSD"
temp$iso_o[temp$iso_o == "SDN"] <- "SSD"
dist_dy <- rbind(dist_dy, temp)

# Mumbai (use India values)
temp <- dist_dy[dist_dy$iso_d == "IND" | dist_dy$iso_o == "IND", ]
temp$iso_d[temp$iso_d == "IND"] <- "IND_Mumbai"
temp$iso_o[temp$iso_o == "IND"] <- "IND_Mumbai"
dist_dy <- rbind(dist_dy, temp)


# 2. define function to get average of neighbouring countries:
contig_ave <- function(var = region_average_vars[1],
                       t_iso3c = unique(country_daily_excess_deaths$iso3)[1],
                       dist_dy,
                       country_daily_excess_deaths){
    temp <- country_daily_excess_deaths %>% 
      filter(iso3c %in% unlist(dist_dy[dist_dy$iso_o == t_iso3c & dist_dy$contig == 1, "iso_d"]))
    
    if(nrow(temp) == 0){
      return(rep(NA, length(unique(country_daily_excess_deaths$date))))
    }

    return(
      unlist(lapply(unique(temp$date), FUN = function(i){
        mean(temp[temp$date == i, p], na.rm = T)
      })))
}

# 3. define function to get distance-weighted average
dist_ave <- function(var,
                     t_iso3c,
                     dist_dy,
                     country_daily_excess_deaths){
  
  temp <- country_daily_excess_deaths[country_daily_excess_deaths$iso3c != t_iso3c, c(var, "iso3c", "date")]
  temp <- merge(temp, dist_dy[dist_dy$iso_o == t_iso3c, c("iso_d", "dist")], by.x="iso3c", by.y="iso_d", all.x = T)
  temp$weight <- 1/log(temp$dist+1)
  temp$weight[is.na(temp$dist)] <- 0
  
  return(
    unlist(lapply(unique(temp$date), 
                         FUN = function(i){
                           weighted.mean(x= temp[temp$date == i, var],
                                         w= temp[temp$date == i, "weight"],
                                         na.rm = T)
                         })))
}

# 4. apply over variables and countries 

# Load a progress bar (this takes some time)
library(progress)
pb <- progress_bar$new(
  format = "  calculating [:bar] :percent eta: :eta",
  total = length(region_average_vars)*length(unique(country_daily_excess_deaths$iso3c)), 
  clear = FALSE, width= 60)

# Loop over variables...
for(p in region_average_vars){
  country_daily_excess_deaths[, paste0(p, "_dist_average")] <- NA
  country_daily_excess_deaths[, paste0(p, "_contiguous_country_average")] <- NA
  # An countries
  for(j in unique(country_daily_excess_deaths$iso3c)){
    
    # If we have distance data
    if(j %in% unique(dist_dy$iso_o)){
      
      # Calculate distance-weighted average
  country_daily_excess_deaths[country_daily_excess_deaths$iso3c == j, 
                              paste0(p, "_dist_average")] <-
    dist_ave(var = p,
             t_iso3c = j,
             dist_dy,
             country_daily_excess_deaths)
  
      # And mean of neighbouring countries
  country_daily_excess_deaths[country_daily_excess_deaths$iso3c == j, 
                              paste0(p, "_contiguous_country_average")] <-
    contig_ave(var = p,
             t_iso3c = j,
             dist_dy,
             country_daily_excess_deaths)
    }
    
    # Incrementing the progress bar counter for each country-variable
    pb$tick()
  }
}

# Step 9: export data frame to csv    ---------------------------------------

# Export dataframe
fwrite(country_daily_excess_deaths, "output-data/country_daily_excess_deaths_with_covariates.csv")


