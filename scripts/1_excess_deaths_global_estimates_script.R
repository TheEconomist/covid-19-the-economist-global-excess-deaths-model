# Step 1: import libraries ---------------------------------------

# Import libraries
library(tidyverse)
library(data.table)
library(lubridate)
library(countrycode)
library(readr)
library(readxl)
library(zoo)
options(scipen=999)

#The data saved in 0_ will be pre-processed here
#a function to load a csv files
load_csv <- function(name, guess_max = NULL){
  if(is.null(guess_max)){
    read_csv(
      file.path(here::here(), "source-data",
                paste0(name, ".csv"))
    )
  } else{
    read_csv(
      file.path(here::here(), "source-data",
                paste0(name, ".csv")),
      guess_max = guess_max
    )
  }
}

latestDate <- "2021-08-19"

## Excess deaths from economist:
all_weekly_excess_deaths <- load_csv("all_weekly_excess_deaths", guess_max = 10000)
all_monthly_excess_deaths <- load_csv("all_monthly_excess_deaths")
all_quarterly_excess_deaths <- load_csv("all_quarterly_excess_deaths")

#we will reformat this to make one dataset

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
  filter(!country_code %in% c("ARM","AZE")) %>% # Remove countries involved in Nagarno-Karabkh war
  rename(iso3c = country_code) %>%
  filter(date <= latestDate)

##Download OWID data:
country_daily_data <- load_csv("country_daily_data", guess_max = 30000) %>%
  #remove non-countries:
  filter(nchar(iso_code) == 3) %>%
  mutate(date = as.Date(date),
         country = location,
         iso3c = iso_code, #must set Taiwans regions as not officially assigned
         region = if_else(iso3c == "TWN",  "Asia", countrycode(iso3c, origin="iso3c",destination="un.region.name")),
         subregion = if_else(iso3c == "TWN",  "Eastern Asia", countrycode(iso3c, origin="iso3c",destination="un.regionsub.name")),
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
  filter(date >= as.Date("2020-01-01") & date <= latestDate) %>%
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
# Add missing countries (Turkmenistan and DPRK)
na_countries <- data.frame(date = as.Date("2020-01-01"),
                           country = c("North Korea",
                                       "Turkmenistan",
                                       "Western Sahara",
                                       "Palau",
                                       "Puerto Rico",
                                       "Kosovo"),
                           iso3c = c("PRK",
                                     "TKM",
                                     "ESH",
                                     "PLW",
                                     "PRI",
                                     "XKS"),
                           region = c("Asia",
                                      "Asia",
                                      "Africa",
                                      "Oceania",
                                      "Americas",
                                      "Europe"),
                           subregion = c("Eastern Asia",
                                         "Central Asia",
                                         "Northern Africa",
                                         "Micronesia",
                                         "Latin America and the Caribbean",
                                         "Southern Europe"),
                           population = c(25670000, 5942000, 567402, 17907, 3285874,
                                          1935259))
na_countries[, setdiff(colnames(country_daily_data), colnames(na_countries))] <- NA
country_daily_data <- rbind(
  country_daily_data,
  na_countries
)
#combine with excess deaths data
country_daily_excess_deaths <- country_daily_data %>%
  ungroup() %>%
  left_join(daily_excess_deaths)
# Ensure data has all days for all countries
country_daily_excess_deaths <- merge(country_daily_excess_deaths, expand.grid(iso3c = unique(country_daily_excess_deaths$iso3c),
                                                                              date = seq.Date(min(country_daily_excess_deaths$date), 
                                                                                              max(country_daily_excess_deaths$date), by = 'day'),
                                                                              stringsAsFactors = FALSE), by = c('iso3c', 'date'), all = TRUE) %>%
  mutate(
    across(
      c(country,iso3c,region,subregion,population,
        hospital_beds_per_thousand,
        population_density,median_age,aged_65_older,
        aged_70_older,life_expectancy),
      .fns = ~ ave(.x, iso3c, FUN = 
                     function(x){
                       na.omit(x)[1]
                     })
    )
  )
# Define function
leading_zeros <- function(x){
  if(is.na(x[1]) & sum(is.na(x)) != length(x)){
    x[1:min(which(!is.na(x))-1)] <- 0
  }
  x
}
country_daily_excess_deaths <- country_daily_excess_deaths %>%
  #Fill in leading 0s for covid data:
  arrange(date) %>%
  group_by(iso3c) %>%
  mutate(
    # Cycle through relevant columns an impute leading zeroes
    across(
      c(daily_covid_deaths, daily_covid_deaths_per_100k,
           daily_covid_cases, daily_covid_cases_per_100k,
           daily_total_deaths,
           daily_total_deaths_per_100k,
           vaccinated_pct,
           fully_vaccinated_pct,
           daily_vaccinations,
           daily_vaccinations_per_100k),
    .fns = ~ leading_zeros(.x)
    )
  )
# Generate cumulative tests, cases, deaths, and vaccinations:
country_daily_excess_deaths <- country_daily_excess_deaths %>%
  mutate(
    across(
      c(daily_tests, daily_covid_cases, daily_covid_deaths, daily_vaccinations),
      function(x){cumsum(ifelse(is.na(x), 0, x))*
          (100000/population)},
      .names = "cumulative_{.col}_per_100k"
    )
  ) %>%
  ungroup() %>%
  mutate(
    # Generate day of week
    weekday = as.POSIXlt(date)$wday
  ) %>%
  filter(date <= latestDate)

###Static Data
static_data <- list()

##Vdem data:
vdem <- load_csv("vdem") %>%
  rename(
    iso3c = country_text_id,
    vdem_liberal_democracy_score = v2x_libdem,
    vdem_freedom_of_expression_score = v2x_freexp_altinf
  ) %>% 
  #get rid of missing values
  filter(!(is.na(vdem_liberal_democracy_score)) & !(is.na(vdem_freedom_of_expression_score))) %>%
  group_by(iso3c) %>%
  #limit to recent years
  filter(year <= 2020) %>%
  #take the values from the latest year for each country
  filter(year == max(year)) %>%
  select(iso3c,
         vdem_freedom_of_expression_score,
         vdem_liberal_democracy_score)
#add to list
static_data[[length(static_data) + 1]] <- vdem

##Democracy Binary data:
democracy_binary <- load_csv("democracy_binary") %>%
  filter(
    # Restrict to most recent year:
    year == 2015
    ) %>%
  mutate(
    # Generate iso3c:
    iso3c <- countrycode(ccode, "cown", "iso3c"),
    #for countries that could not be assigned
    iso3c = if_else(
      is.na(iso3c),
      countrycode(country,
                  "country.name", "iso3c"),
      iso3c
    ),
    #assign kosovo its own code
    iso3c = if_else(
      country == "KOSOVO",
      "XKX",
      iso3c
    )
  ) %>%
  # Make descriptive column names and select relevant columns
  rename(
    boix_democracy_yes_no = democracy,
    boix_democracy_duration_years = democracy_duration) %>% 
  select(iso3c,
         boix_democracy_yes_no,
         boix_democracy_duration_years)
#add to list
static_data[[length(static_data) + 1]] <- democracy_binary

##Import freedom house:
freedom_house <- load_csv("freedom_house") %>%
  filter(
    #restrict to entities designated as countries (+ Hong Kong) in most recent year
    Edition == 2020,
    `C/T` == "c" | `Country/Territory` == "Hong Kong"
  ) %>% # Generate iso3c
  mutate(
    iso3c = countrycode(`Country/Territory`, "country.name", "iso3c"),
    #manually assign kosovo and micronesia
    iso3c = case_when(
      `Country/Territory` == "Kosovo" ~ "XKX",
      `Country/Territory` == "Micronesia" ~ "FSM",
      TRUE ~ iso3c
    )
  )
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
#add to list
static_data[[length(static_data) + 1]] <- freedom_house

##Centre for systemic peace data:
polity <- load_csv("polity")
# Generate iso3c and restrict to most recent year
polity <- polity %>%
  filter(year == 2018) %>%
  mutate(iso3c = countrycode(ccode, "cown", "iso3c"),
         #use names for those that haven't worked
         iso3c = if_else(
           is.na(iso3c),
           countrycode(country, "country.name", "iso3c"),
           iso3c
         ),
         #Kosovo, sudan and south sudan
         iso3c = case_when(
           country == "Kosovo" ~ "XKS",
           country == "Sudan-North" ~ "SDN",
           country == "South Sudan" ~ "SSD",
           TRUE ~ iso3c
         )
         )%>% 
        #Make descriptive column names and select relevant columns
  rename(
    polity_democracy_score = polity2) %>% 
  select(iso3c,
         polity_democracy_score)
#add to list
static_data[[length(static_data) + 1]] <- polity
  

##A dataset of islands
islands <- c("Aruba",
              "Anguilla",
             "Antigua and Barbuda",
             "Australia",
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
             "Pitcairn",
             "Saint Kitts and Nevis",
             "Saint Lucia",
             "Saint Vincent and the Grenadines",
             "Samoa",
             "Sao Tome and Principe",
             "Seychelles",
             "Singapore",
             "Solomon Islands",
             "Sri Lanka",
             "Republic of China (Taiwan)",
             "Tonga",
             "Trinidad and Tobago",
             "Tuvalu",
             "United Kingdom",
             "Vanuatu"
             )
#make dataset
islands <- data.frame(iso3c = countrycode(islands, "country.name",
                                          "iso3c"),
                      island = TRUE)
# Ensure all countries either island or not island
islands <- merge(
  country_daily_excess_deaths %>% 
    select(iso3c) %>%
    unique(),
  islands, by = 'iso3c', all = T
  ) %>%
  mutate(island = if_else(
    is.na(island),
    FALSE,
    island
  ))
#add to list
static_data[[length(static_data) + 1]] <- islands

##WDI data:
wdi <- load_csv("wdi") %>%
  mutate(
    iso3c = countrycode(iso2c, "iso2c", "iso3c"),
    #add in kosovo
    iso3c = if_else(
      country == "Kosovo",
      "XKX",
      iso3c
    )
  ) %>%
  select(!c(country, iso2c)) %>%
  #remove non countries
  filter(!is.na(iso3c))
# Only latest observation
wdi <- data.frame(wdi) %>%
  arrange(year) %>%
  mutate(
    across(
      !c(year, iso3c),
      ~ave(.x, iso3c, 
           FUN = function(x){
             if(max(which(!is.na(x))) == -Inf){
               NA
             } else {
               x[max(which(!is.na(x)))]
             }
           })
    )
  ) %>%
  #drop years
  select(!year) %>%
  # Collapse rows to one per country (multiple happens when data comes from different years)
  group_by(iso3c) %>%
  summarise(
    across(
      .cols=everything(),
      .fns = ~mean(.x, na.rm = T)
    )
  ) %>%
  ungroup()
#add to list
static_data[[length(static_data) + 1]] <- wdi

#Import coordinates of capital cities and largest cities:
world.cities <- load_csv("worldcities")
world.cities <- world.cities %>% 
  rename(iso3c = iso3) %>% 
  #modify some countries to aggregate:
  mutate(
    iso3c = if_else( #armenia given argentinas code
      country == "Armenia",
      "ARM",
      iso3c
    ),
  capital = if_else( #set canadas capital to ottawa
    city_ascii == "Ottawa" & iso3c == "CAN",
    "primary",
    capital
  ),
  capital = if_else( #macau is its own capital
    city_ascii == "Macau" & iso3c == "MAC",
    "primary",
    capital
  ),
  capital = if_else( #San Juan is the capital of Puerto Rico
    city_ascii == "San Juan" & iso3c == "PRI",
    "primary",
    capital
  ),
  iso3c = if_else(
    city_ascii %in% c("Gaza", "Al Quds"), #put Gaza and Al Quds in palestine
    "PSE",
    iso3c
  ),
  #add populations for both from wikipedia
  population = if_else(
    city_ascii == "Gaza" & iso3c == "PSE",
    590481,
    population
  ),
  population = if_else(
    city_ascii == "Al Quds" & iso3c == "PSE",
    0, #set to zero, since it's de-facto Isreal
    population
  ),
  iso3c = if_else(
    country == "Svalbard", #put svalbard in norway
    "NOR",
    iso3c
  ),
  population = if_else(
      city_ascii == "The Valley", #population of the capital of Anguilla taken from wikipedia
      1067,
      population
  ),
  population = if_else(
    city_ascii == "Philipsburg" & iso3c == "SXM", #population of the capital of Sint Maarten taken from wikipedia
    1894,
    population
  ),
  population = if_else(
    city_ascii == "Tifariti" & iso3c == "ESH", #population of a city in west sahara taken from wikipedia
    3000,
    population
  ),
  #all other NA population we set to 0 to avoid errors
  population = if_else(
    is.na(population),
    0,
    population
  ),
  iso3c = if_else(#french territories
    country %in% c("Wallis And Futuna", "Saint Pierre And Miquelon",
                   "Saint Martin", "Saint Barthelemy", "Guadeloupe",
                   "French Guiana", "Martinique", "Mayotte", "Reunion"),
    "FRA",
    iso3c
  ),
  iso3c = if_else(#british territories (with no data)
    country %in% c("Norfolk Island", "South Georgia And South Sandwich Islands"),
    "GBR",
    iso3c
  ),
  iso3c = if_else(#Austrailian territories
    country == "Christmas Island",
    "AUS",
    iso3c
  ),
  iso3c = if_else(#American territories
    country %in% c("American Samoa", "Guam", "Northern Mariana Islands"),
    "USA",
    iso3c
  )
  ) %>%
  group_by(iso3c) %>%
  mutate(largest_city_in_country = if_else(
    population == max(population, na.rm = TRUE),
    TRUE,
    FALSE
  ))
#widen data and reduce to capitals and largest cities
world.cities <- full_join(
  world.cities %>%
    filter(largest_city_in_country) %>%
    rename(lat_largest_city = lat, lng_largest_city = lng,
           largest_city_pop = population) %>%
    select(iso3c, lat_largest_city, lng_largest_city, largest_city_pop),
  world.cities %>%
    filter(capital == "primary") %>%
    arrange(iso3c, -population) %>% #order so that highest pop captial is first
    slice(1) %>% #take only the first row for each group (the captial with the biggest pop)
    rename(lat_capital = lat, lng_capital = lng) %>%
    ungroup() %>%
    select(iso3c, lat_capital, lng_capital)
)
# Convert largest city pop to % of total population
world.cities <- merge(world.cities, na.omit(unique(country_daily_excess_deaths[, c("iso3c", "population")])), by = 'iso3c', all.x = T) %>%
  mutate(largest_city_pop_pct = largest_city_pop/population,
         #limit to one (e.g. captial can't have more people than country)
         largest_city_pop_pct = if_else(largest_city_pop_pct > 1, 1, largest_city_pop_pct)) %>%
  select(!c(largest_city_pop, population))
#add to list
static_data[[length(static_data) + 1]] <- world.cities

##Import Economist IFR data
#source:https://www.economist.com/graphic-detail/2020/11/16/why-rich-countries-are-so-vulnerable-to-covid-19 and github: https://github.com/TheEconomist/covid-19-age-adjusted-ifr
ifr <- load_csv("ifr")
ifr <- ifr %>% 
  rename(demography_adjusted_ifr = area_ifr) %>%
  mutate(iso3c = countrycode(iso2c, "iso2c", "iso3c")) %>%
  select(demography_adjusted_ifr, iso3c)
#add to list
static_data[[length(static_data) + 1]] <- ifr

##Tourism Arrivals
unwto <- load_csv("unwto")
#Fill in country name
unwto <- unwto %>%
  rename(
    country = `Basic data and indicators`
  ) %>%
  fill(
    #fill in missing country values
    country
  ) %>%
  filter(
    #select the correct rows
    ...6 == 'Total arrivals',
    !is.na(country)
  ) %>%
  select(
    c(country, ...6, Units, num_range("", range = 2000:2019))
  ) %>% 
  # Get most recent data
  pivot_longer(cols = num_range("", range = 2000:2019), 
               names_to = "year",
               values_to = "tourist_arrivals_in_thousands_2019") %>%
  mutate(year = as.numeric(year),
         tourist_arrivals_in_thousands_2019 = as.numeric(tourist_arrivals_in_thousands_2019)) %>%
  filter(!is.na(tourist_arrivals_in_thousands_2019)) %>%
  group_by(country) %>%
  filter(year == max(year)) %>%
  ungroup() %>%
  #add country codes
  mutate(
    iso3c = countrycode(country, "country.name", "iso3c") #all countries that fail arn't countries or don't have any data so we'll leave it
  ) %>%
  select(
    iso3c,
    tourist_arrivals_in_thousands_2019
  )
#add to list
static_data[[length(static_data) + 1]] <- unwto

###Time Varying Data:
time_varying_data <- list()
##Google Mobility reports:
mob <- load_csv("mob")
#Generate iso3c, rename and select columns
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
#add to list
time_varying_data[[length(time_varying_data) + 1]] <- mob


##Policy response data
ox <- load_csv("ox", guess_max = 50000)
ox <- ox %>%
  filter(
    #Use only national data:
    Jurisdiction == "NAT_TOTAL"
  )
ox <- 
  mutate(
    #Only use national policies (not those with geographical scope, as we do not know the extent of the geographical scope):
    ox,
    across(
      .cols = c(C1_School.closing, C2_Workplace.closing, C3_Cancel.public.events, 
      C4_Restrictions.on.gatherings, C5_Close.public.transport, 
      C6_Stay.at.home.requirements, C7_Restrictions.on.internal.movement, 
      H6_Facial.Coverings),
      .fns = ~ if_else(
        !(pull(ox, paste0(substr(cur_column(), 1, 2), "_Flag")) %in% c(1,NA)),
        0,
        .x
      )
    )
  ) %>% 
  mutate(oxcgrt_schools_closed = `C1_School.closing`,
         oxcgrt_workplaces_closed = `C2_Workplace.closing`,
         oxcgrt_cancel_public_events = `C3_Cancel.public.events`,
         oxcgrt_gathering_restrictions = `C4_Restrictions.on.gatherings`,
         oxcgrt_public_transport_closed = `C5_Close.public.transport`,
         oxcgrt_stay_at_home_required = `C6_Stay.at.home.requirements`,
         oxcgrt_internal_movement_restrictions = `C7_Restrictions.on.internal.movement`,
         oxcgrt_international_movement_restrictions = `C8_International.travel.controls`,
         oxcgrt_face_masks_required = `H6_Facial.Coverings`,
         iso3c = if_else(CountryName == "Kosovo", "XKS", countrycode(CountryName, "country.name", "iso3c")),
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
#add to list
time_varying_data[[length(time_varying_data) + 1]] <- ox


##Import seroprevalence data:
sero <- load_csv("sero") %>%
  mutate(`Sampling Method` = if_else(`Prevalence Estimate Name` == "200609_Bergamo_HealthAgency_GenPop",
                                     'Convenience', `Sampling Method`)) %>% # miss-coded in original.
  filter(`Grade of Estimate Scope` %in% c("National", "Regional"), #Restrict to national or regional surveys
         `Sampling Method` %in% c("Sequential", "Simplified probability", "Stratified probability", "Stratified non-probability"),
         #Restrict to random or quasi-random sample
         `Sample Frame (groups of interest)` %in% c("Household and community samples", "Residual sera ", "Multiple populations", "Blood donors"), 
         #Restrict to general adult populations
         `Sample Frame (age)` %in% c("Adults (18-64 years)",
                                          "Multiple groups",
                                          "Not reported")
         ) %>%
  mutate(
    # Add dates
    end_date = dmy(`Sampling End Date`),
    start_date = if_else(is.na(`Sampling Start Date`),
                         end_date,
                         dmy(`Sampling Start Date`)),
    #getting the midpoint
    date = start_date + floor(end_date - start_date)/2,
    #date created
    date_created = dmy(`Date Created`)
  ) %>% # Generate iso3c, rename and select columns
  mutate(
  iso3c = countrycode(Country, "country.name", "iso3c"),
  seroprevalence = as.numeric(gsub("%", "", `Serum positive prevalence`)),
  seroprevalence_N = `Denominator Value`,
  seroprevalence_study_level = `Grade of Estimate Scope`
  ) %>% select(
  iso3c,
  date,
  seroprevalence,
  seroprevalence_N,
  seroprevalence_study_level
 ) %>%
  mutate(
    seroprevalence_national = if_else(
      !(seroprevalence_study_level == "National"),
      as.numeric(NA), 
      seroprevalence
      ),
    seroprevalence_regional_or_national = seroprevalence,
    seroprevalence_N_national = if_else(
      !(seroprevalence_study_level == "National"),
      as.numeric(NA), 
      seroprevalence_N
    )
  )
# Average serostudies that have the same date by N:
sero <- data.frame(sero) %>%
  mutate(
    id = paste0(iso3c, "_", date)
  ) %>%
  group_by(id, iso3c, date) %>%
  summarise(
    seroprevalence_regional_or_national = weighted.mean(
      seroprevalence_regional_or_national,
      seroprevalence_N
    ),
    seroprevalence_national = weighted.mean(
      seroprevalence_national,
      seroprevalence_N_national
    ),
    seroprevalence_N = sum(seroprevalence_N)
  ) %>%
  ungroup() %>%
  select(!id)
# Calculate 3-month sample-size-weighted average by country
sero <- merge(
  sero %>% filter(iso3c %in% country_daily_excess_deaths$iso3c),
  country_daily_excess_deaths %>%
    select(iso3c, date, region, cumulative_daily_covid_cases_per_100k, population) %>%
    unique(),
  all = T)
#Define a function to calculate weighted rolling average
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
    
  return(averages)
  }
# Sort data by date and define weights
sero <- sero %>%
  arrange(date) %>%
  mutate(weights = log(seroprevalence_N))
sero <- mutate(sero,
         # For all serosurveys
         seroprevalence_reg_or_nat_30d_wma = 
           weighted_rolling_average(df = sero, 
                                    value = "seroprevalence_regional_or_national", 
                                    weights = "weights",
                                    window_span = 30),
         seroprevalence_nat_30d_wma = weighted_rolling_average(df = sero, 
                                                               value = "seroprevalence_national", 
                                                               weights = "weights",
                                                               window_span = 30),
         # Specifiy seroprevalence to be 0 before first case:
         seroprevalence_reg_or_nat_30d_wma = if_else(
           cumulative_daily_covid_cases_per_100k == 0 | is.na(sero$cumulative_daily_covid_cases_per_100k),
           0,
           seroprevalence_reg_or_nat_30d_wma
         ),
         seroprevalence_nat_30d_wma = if_else(
           cumulative_daily_covid_cases_per_100k == 0 | is.na(sero$cumulative_daily_covid_cases_per_100k),
           0,
           seroprevalence_nat_30d_wma
         )) %>%
         # Linearly interpolate change in seroprevalence
  mutate(
         seroprevalence_reg_or_nat_30d_wma_interpolated = ave(seroprevalence_reg_or_nat_30d_wma, iso3c,
                                                           FUN = function(x){
                                                             if(sum(!is.na(x)) < 0){
                                                               x
                                                             } else {
                                                               na.approx(x, na.rm = F)
                                                             }
                                                           }),
         seroprevalence_nat_30d_wma_interpolated = ave(seroprevalence_nat_30d_wma, iso3c, FUN = function(x){
           if(sum(!is.na(x)) < 0){
             x
           } else {
             na.approx(x, na.rm = F)
           }
         })
         ) %>%
  # Specify seroprevalence to be monotonically increasing:
  mutate(
    seroprevalence_reg_or_nat_30d_wma_interpolated = ave(seroprevalence_reg_or_nat_30d_wma_interpolated, iso3c,
                                                         FUN = function(x){
                                                           cummax(ifelse(is.na(x), 0, x))
                                                         }),
    seroprevalence_nat_30d_wma_interpolated = ave(seroprevalence_nat_30d_wma_interpolated, iso3c,
                                                  FUN = function(x){
                                                    cummax(ifelse(is.na(x), 0, x))
                                                  })
  ) %>%
  # Transform to 30-day average change in seroprevalence
  mutate(
    sero_nat_delta = ave(seroprevalence_nat_30d_wma_interpolated,
                         iso3c,
                         FUN = function(x){
                           x <- x - c(0, x)[1:length(x)]
                           frollmean(x, n=30, align = "center", 
                                     na.rm = T)
                         }),
    sero_nat_or_reg_delta = ave(seroprevalence_reg_or_nat_30d_wma_interpolated,
                                iso3c,
                                FUN = function(x){
                                  x <- x - c(0, x)[1:length(x)]
                                  frollmean(x, n=30, align = "center", 
                                            na.rm = T)
                                })
  )
#plot
ggplot(sero, aes(x=date, y=sero_nat_or_reg_delta,
                 col = iso3c))+geom_line()+theme(legend.position = "none")
sero <- sero %>%
  select(iso3c, date, sero_nat_or_reg_delta, sero_nat_delta) %>%
  unique() %>%
  filter(date <= latestDate)
#add to list
time_varying_data[[length(time_varying_data) + 1]] <- sero

###Merge together
for(i in static_data){
  country_daily_excess_deaths <- merge(country_daily_excess_deaths,
                                       unique(i), by = "iso3c", all.x = T)
}

for(i in time_varying_data){
  country_daily_excess_deaths <- merge(country_daily_excess_deaths,
                                       i, by = c("iso3c", "date"), all.x = T)
}

# Check that no countries has multiple observations for the same date:
max(table(paste0(country_daily_excess_deaths$iso3c, "_", country_daily_excess_deaths$date)))==1

##Import economist standard names
econ_df <- load_csv("econ_df") %>%
  rename(
    country = Name,
    economist_region = Regions,
    wb_income_group = `Income group WB`,
    imf_economy = `Economy IMF`,
    iso3c = ISOA3
    ) %>%
  select(
    country,
    iso3c,
    wb_income_group,
    imf_economy
  ) %>%
  mutate(
    #set up kosovo code
    iso3c = if_else(iso3c == "KOS", "XKS", iso3c)
  )
#merge into data
country_daily_excess_deaths <- country_daily_excess_deaths %>%
  select(!country) %>%
  left_join(
    econ_df
  )

###Additional Non-repo Excess Deaths
##China:
china <- load_csv("china")
#split apart
china <- china %>%
  select(Week, `Starting date of the week`,
         `Mean No. of reported Deaths, 2015-19`, `Delay adjustment ratio (%)*`,
         `No. of reported deaths in 2020`) %>%
  mutate(type = "Wuhan",
         population = 2300887, # Population associated with each reporting area also from supplemental materials
         mean_population_2015_2019 = (2202663 + 2280200 + 2314269 + 2293425 + 2300887)/5,
         total_population = 11081000 # from http://www.hb.xinhuanet.com/2019-03/26/c_1124281764.htm
  ) %>%
  rbind(
    china %>%
      select(Week, `Starting date of the week`,
             `Mean No. of reported Deaths, 2015-19_1`, `Delay adjustment ratio (%) *`,
             `No. of reported deaths in 2020_1`) %>%
      rename(`Mean No. of reported Deaths, 2015-19` = `Mean No. of reported Deaths, 2015-19_1`,
             `Delay adjustment ratio (%)*` = `Delay adjustment ratio (%) *`,
             `No. of reported deaths in 2020` = `No. of reported deaths in 2020_1`) %>%
      mutate(type = "Hubei, except Wuhan",
             population = 10156616,
             mean_population_2015_2019 = (10348961 + 10216200 + 10252213 + 10179298 + 10156616)/5,
             total_population = 59270000 - 11081000 # from https://www.statista.com/statistics/279013/population-in-china-by-region/
      )
  ) %>%
  rbind(
    china %>%
      select(Week, `Starting date of the week`,
             `Mean No. of reported Deaths, 2015-19_2`, `Delay adjustment ratio (%)* *`,
             `No. of reported deaths in 2020_2`) %>%
      rename(`Mean No. of reported Deaths, 2015-19` = `Mean No. of reported Deaths, 2015-19_2`,
             `Delay adjustment ratio (%)*` = `Delay adjustment ratio (%)* *`,
             `No. of reported deaths in 2020` = `No. of reported deaths in 2020_2`) %>%
      mutate(type = "China, outside Hubei",
             population = 325888003,
             mean_population_2015_2019 = (319755726 + 321459243
                                          + 323652034 + 324722209
                                          + 325888003)/5 ,
             total_population = 1400050000 - 59270000 - 11081000 # from https://www.statista.com/statistics/263765/total-population-of-china/
      )
  )

china <- china %>%
  mutate(
    # This adjusts for deaths data not being recorded when the paper was written:
    reporting_adjusted_deaths = `No. of reported deaths in 2020`/(`Delay adjustment ratio (%)*`/100),
    # Since they do not supply the raw data, we cannot do better than:
    expected_deaths = `Mean No. of reported Deaths, 2015-19`,
    # With population-change multiplier:
    expected_deaths = expected_deaths*(population/mean_population_2015_2019)
  )
# Note: supplemental figure 2 suggests the mean should work well, no strong over-time-trend in per capita death.

# To inspect our estimates for China excess deaths by area, uncomment the below chunk:
ggplot(china, aes(x=as.numeric(Week), y=100000*reporting_adjusted_deaths/population, col = "2020"))+
  geom_line()+
  geom_line(aes(y=100000*expected_deaths/population, col = "Expected"))+
  geom_hline(aes(yintercept = 0), col = "black")+ylim(c(-10, 100))+
theme_minimal()+facet_grid(.~type)+xlab("Week in 2020")+ylab("Deaths per 100k population")

# Specify excess deaths
china <- china %>%
  mutate(
    excess_deaths = reporting_adjusted_deaths - expected_deaths,
    excess_deaths_per_100k =  100000*excess_deaths/population,
    # We next combine the reporting regions, weighting them by the total population they represent.
    pop_weights = total_population / 1400050000
  ) %>%
  group_by(Week, `Starting date of the week`) %>%
  summarise(#calculate weighted weekly excess deaths for whole country and then convert to daily rate
    daily_excess_deaths_per_100k = sum(excess_deaths_per_100k*pop_weights)/7  #changed name from country_excess_deaths_per_100k to daily_excess_deaths_per_100k
  ) %>%
  ungroup() %>%
  filter(`Starting date of the week` != "Jan-March") %>%
  mutate(date = dmy(paste0(`Starting date of the week`, " 2020"))
  ) %>%
  select(date, `daily_excess_deaths_per_100k`)
# Add in other 6 days:
china_daily <- china
for(i in 1:6){
  china_daily <- rbind(china_daily, 
                 china %>%
                         mutate(date = date + i)
                       )
}
china_daily <- china_daily %>%
  mutate(
    daily_excess_deaths = daily_excess_deaths_per_100k * 1400050000 / 100000,
    iso3c = "CHN"
  )
# Merge this in with country data
country_daily_excess_deaths <- country_daily_excess_deaths %>%
  merge(
    china_daily, by = c("iso3c", "date"), all = TRUE
  ) %>%
    mutate(
      daily_excess_deaths_per_100k = if_else(
        is.na(daily_excess_deaths_per_100k.y),
        daily_excess_deaths_per_100k.x,
        daily_excess_deaths_per_100k.y
      ),
      daily_excess_deaths = if_else(
        is.na(daily_excess_deaths.y),
        daily_excess_deaths.x,
        daily_excess_deaths.y
      )
    ) %>%
    select(!c(
      daily_excess_deaths_per_100k.x,
      daily_excess_deaths_per_100k.y,
      daily_excess_deaths.x,
      daily_excess_deaths.y
    ))

###Non country data:
##India:
#source:?
india <- load_csv("india") %>%
  mutate(
    iso3c = "IND",
    date = dmy(date)
  )
# Questions raised about Kerala data makes us discard it (see: https://www.indiaspend.com/covid-19/mortality-data-kerala-mumbai-too-soon-to-say-india-covid19-less-deadly-second-wave-737270)
mumbai <- india %>%
  filter(country == "Mumbai")
# In contrast, the Mumbai data appears to check out against seroprevalence ++ and by distribution of deaths by age: https://science.thewire.in/the-sciences/covid-19-mumbai-all-cause-mortality-data-ifr-bmc-seroprevalence-survey/

# Expand this data to match date-range of big dataset:
date_range <- unique(country_daily_excess_deaths$date)
mumbai <- merge(mumbai, data.frame("date" = date_range), by = "date", all = T) %>%
  fill(c(country, region, subregion, population, median_age, aged_70_older, life_expectancy),
       .direction = "up") %>%
  fill(c(country, region, subregion, population, median_age, aged_70_older, life_expectancy),
       .direction = "down") %>%
  mutate(
    iso3c = "IND"
  )

# Merge in missing columns from India data:
mumbai <- merge(mumbai, country_daily_excess_deaths[, c("date", "iso3c", setdiff(colnames(country_daily_excess_deaths), colnames(india)))], by = c("iso3c", "date"))
mumbai <- mumbai[, colnames(country_daily_excess_deaths)]
mumbai <- mumbai %>%
  mutate(
    #to differentiate it from india
    iso3c = "IND_Mumbai",
    is_subregion = TRUE
  )
# Merge this in with country data
country_daily_excess_deaths <- country_daily_excess_deaths %>%
  mutate(
    is_subregion = FALSE
  ) %>%
  rbind(
    mumbai
  )


# Step 8: calculate region averages  ---------------------------------------

# Calculate regional averages by day (weighted by population)

# 0. Select definition of region (NB: 'region' are as defined by World Bank Development Indicators)
country_daily_excess_deaths <- country_daily_excess_deaths %>%
  filter(iso3c != "WLF") %>% #drop Walis and Futuna, included in france
  mutate(
    continent = countrycode(ifelse(iso3c != "IND_Mumbai", iso3c, "IND"), "iso3c", "continent"),
    #fix for Mumbai and Kosovo
    continent = case_when(
      iso3c == "IND_Mumbai" ~ "Asia",
      iso3c == "XKS" ~ "Europe",
      TRUE ~ continent
    ),
    region = countrycode(iso3c, "iso3c", "region"),
    #fix for saint Helena and Mumbai etc.
    region = case_when(
      iso3c == "SHN" ~ "Europe & Central Asia",
      iso3c == "IND_Mumbai" ~ "South Asia",
      iso3c == "XKS" ~ "Europe & Central Asia",
      iso3c == "ESH" ~ "Middle East & North Africa",
      TRUE ~ region
    )
  )

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
  
  return(res)
  }


# apply across variables
country_daily_excess_deaths <- country_daily_excess_deaths %>%
  mutate(
    across(
      .cols = c(sero_nat_or_reg_delta,
                sero_nat_delta,
                daily_excess_deaths_per_100k,
                daily_tests_per_100k,
                daily_covid_cases_per_100k,
                daily_covid_deaths_per_100k,
                daily_positive_rate, 
                median_age,
                wdi_life_expectancy_at_birth,
                demography_adjusted_ifr),
      .fns = list(
        region_average = ~region_average(variable = .x,
                                         region = region,
                                         time = date,
                                         weights = log(population),
                                         include_current_obs = FALSE),
        sub_region_average = ~region_average(variable = .x,
                                            region = subregion,
                                            time = date,
                                            weights = log(population),
                                            include_current_obs = FALSE),
        econ_region_average = ~region_average(variable = .x,
                                             region = wb_income_group, #note not sure what econ_region is supposed to be!!!
                                             time = date,
                                             weights = log(population),
                                             include_current_obs = FALSE)
      )
    )
  ) # NB: excluding observation in question is crucial if doing this for e.g. observed excess deaths

# Step 8: calculate distance averages  ---------------------------------------
# 1. get distance dyads
dist_dy <- load_csv("dist_dy")

# Applying a few fixes to iso codes:
dist_dy <- dist_dy %>%
  mutate(
    across(
      c(iso_o, iso_d),
      ~case_when(
        .x == "ROM" ~ "ROU",
        .x == "ZAR" ~ "COD",
        .x == "YUG" ~ "SRB",
        .x == "TMP" ~ "TLS",
        TRUE ~ .x
      )
    )
  )
#add missing countries
dist_dy <- dist_dy %>%
  rbind(
    # Montenegro (use SCG values)
    dist_dy %>% 
      filter(
        iso_d == "SRB" | iso_o == "SRB"
      ) %>%
      mutate(
        iso_d = if_else(iso_d == "SRB", "MNE", iso_d),
        iso_o =if_else(iso_o == "SRB", "MNE", iso_o)
      )
  ) %>%
  rbind(
    # South Sudan (use SUD values)
    dist_dy %>% 
      filter(
        iso_d == "SDN" | iso_o == "SDN"
      ) %>%
      mutate(
        iso_d = if_else(iso_d == "SDN", "SSD", iso_d),
        iso_o =if_else(iso_o == "SDN", "SSD", iso_o)
      )
  ) %>%
  rbind(
    # Mumbai (use India values)
    dist_dy %>% 
      filter(
        iso_d == "IND" | iso_o == "IND"
      ) %>%
      mutate(
        iso_d = if_else(iso_d == "IND", "IND_Mumbai", iso_d),
        iso_o =if_else(iso_o == "IND", "IND_Mumbai", iso_o)
      )
  ) %>%
  rbind(
    # Kosovo (use Serbia values)
    dist_dy %>% 
      filter(
        iso_d == "SRB" | iso_o == "SRB"
      ) %>%
      mutate(
        iso_d = if_else(iso_d == "SRB", "XKX", iso_d),
        iso_o =if_else(iso_o == "SRB", "XKX", iso_o)
      )
  ) %>%
  rbind(
    # Palestine (use Israel values)
    dist_dy %>% 
      filter(
        iso_d == "ISR" | iso_o == "ISR"
      ) %>%
      mutate(
        iso_d = if_else(iso_d == "ISR", "PSE", iso_d),
        iso_o =if_else(iso_o == "ISR", "PSE", iso_o)
      )
  )
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

# Export dataframe as RDS to save on space
saveRDS(
  country_daily_excess_deaths,
  "output-data/country_daily_excess_deaths_with_covariates.Rds"
)


