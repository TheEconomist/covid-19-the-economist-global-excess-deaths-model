# Step 1: Import libraries ------------------------------------------------------------------------------
# Load packages
library(countrycode)
library(readr)
library(anytime)
library(ggplot2)
library(lubridate)
library(tidyverse)
library(data.table)

# Step 2: Get excess mortality estimates and population------------------------------------------------------------------------------

# Get additional subnational units from the World Mortality Project:
# Source: https://raw.githubusercontent.com/akarlinsky/world_mortality/main/local_mortality/local_mortality.csv

lmort <- data.frame(read_csv("source-data/local_mortality.csv"))

# Get country iso3c code
lmort$iso3c <- countrycode(lmort$country_name, "country.name", "iso3c")

# Keep only units with at least 4 years of data at least 2 of which were pre 2020:
lmort$years_of_observations <- ave(lmort$year, lmort$local_unit_name, FUN = function(x) length(unique(x)))
lmort$first_year_of_observations <- as.numeric(ave(lmort$year, lmort$local_unit_name, FUN = function(x) min(unique(x))))
lmort <- lmort[lmort$years_of_observations >= 4 & lmort$first_year_of_observations <= 2018, ]

# Remove places where we cannot get local case and death counts over time (Yemen, Turkey, Syria, and Hyderabad municipal corporation):
lmort <- lmort[!lmort$local_unit_name %in% c("Aden Governorate", "Damascus City", "Istanbul City", "Ankara City", "Hyderabad"), ]

# Remove Chennai city as it is within Tamil Nadu state (and we don't have a way to disentangle the two):
lmort <- lmort[!lmort$local_unit_name %in% c("Chennai City"), ]

# Estimate excess deaths (no year effect unless at least 3 years of pre-pandemic data to remove possibility of random variation due to small number of years):
lmort$expected_deaths <- NA
lmort$expected_deaths[!lmort$local_unit_name %in% c("Jakarta Province", "Mumbai City", "Kolkata City")] <- predict(newdata = lmort[!lmort$local_unit_name %in% c("Jakarta Province", "Mumbai City", "Kolkata City"), ],
                                                                                                                   lm(deaths ~ as.factor(local_unit_name)*as.factor(time)+as.factor(local_unit_name), data = lmort[lmort$year <= 2019 & !lmort$local_unit_name %in% c("Jakarta Province", "Mumbai City", "Kolkata City"), ]))

lmort$expected_deaths[lmort$local_unit_name %in% c("Jakarta Province", "Mumbai City", "Kolkata City")] <- predict(newdata = lmort[lmort$local_unit_name %in% c("Jakarta Province", "Mumbai City", "Kolkata City"), ],
                                                                                                                  lm(deaths ~ as.factor(local_unit_name)*year+as.factor(local_unit_name)*as.factor(time)+as.factor(local_unit_name), data = lmort[lmort$year <= 2019 & lmort$local_unit_name %in% c("Jakarta Province", "Mumbai City", "Kolkata City"), ]))


# Evenly distribute across dates:
dates <- data.frame(date = as.Date("2010-01-01"):as.Date("2025-01-01"), week = NA, month = NA)
dates$date <- as.Date(dates$date, origin ="1970-01-01")
dates$week <- week(dates$date)
dates$month <- month(dates$date)
dates$year <- year(dates$date)

# Add identifier to lmort observations
lmort <- lmort[order(lmort$local_unit_name), ]
lmort$ID <- 1:nrow(lmort)

# Expand to daily dataset
monthly <- merge(lmort[lmort$time_unit == "monthly", ], dates,
                 by.x = c("year", "time"),
                 by.y = c("year", "month"))
weekly <- merge(lmort[lmort$time_unit == "weekly", ], dates,
                by.x = c("year", "time"),
                by.y = c("year", "week"))
# Combine weekly and monthly data
lmort <- rbind(monthly[, intersect(colnames(monthly), colnames(weekly))],
               weekly[, intersect(colnames(monthly), colnames(weekly))])

# Distribute (make daily)
lmort$n_obs <- ave(lmort$ID, lmort$ID, FUN = length)
lmort$n_obs[lmort$time_unit == "weekly"] <- 7

lmort$expected_deaths <- lmort$expected_deaths / lmort$n_obs
lmort$deaths <- lmort$deaths / lmort$n_obs
lmort$excess_deaths <- lmort$deaths - lmort$expected_deaths

inspect = T
if(inspect){
  ggplot(lmort[lmort$year %in% 2015:2021,], aes(x=date, y=deaths/1000))+geom_line()+facet_wrap(.~local_unit_name)+geom_line(aes(y=expected_deaths/1000, col = "expected deaths"), alpha = 0.5)+scale_y_continuous(trans = "log10")
  
  ggplot(lmort[lmort$year %in% 2015:2021,],
         aes(x=date, y=excess_deaths))+geom_line()+facet_wrap(.~local_unit_name)
  # +scale_y_continuous(trans = "pseudo_log")
}

# Use "name" as location identifier column from now on:
lmort$name <- lmort$local_unit_name

# Ensure all dates in dataset:

# Cycle through locations
for(i in unique(lmort$name)){
  
  # Create container with non-time-varying covariates and all dates)
  temp <- cbind(dates, lmort[lmort$name == i, ][1,
                                                setdiff(colnames(lmort), colnames(dates))])
  
  # Ensureing all variables are set to NA
  for(v in c("deaths", "expected_deaths",
             "excess_deaths", "ID", "n_obs")){
    temp[, v] <- NA
  }
  
  # Add these NA dates to main dataset
  lmort <- rbind(lmort, temp[!temp$date %in% lmort$date[lmort$name ==i], colnames(lmort)])
}

# Get population:
lmort$population <- NA
lmort$population[lmort$local_unit_name == "Tamil Nadu State"] <- 82722262 # https://www.indiaonlinepages.com/population/tamil-nadu-population.html
lmort$population[lmort$local_unit_name == "Madhya Pradesh State"] <- 86044251 # https://www.indiaonlinepages.com/population/madhya-pradesh-population.html
lmort$population[lmort$local_unit_name == "Andhra Pradesh State"] <- 53206421 # https://www.indiaonlinepages.com/population/andhra-pradesh-population.html
#lmort$population[lmort$local_unit_name == "Chennai City"] <- 7553790 # https://github.com/elseasama/covid19chennai/blob/gh-pages/chennai_data/demographics.csv
lmort$population[lmort$local_unit_name == "Mumbai City"] <- 13047654 # midpoint between 2019 and 2021 estimate: https://www.indiaonlinepages.com/population/mumbai-population.html
lmort$population[lmort$local_unit_name == "Kolkata City"] <- 15634592 # https://www.indiaonlinepages.com/population/kolkata-population.html
lmort$population[lmort$local_unit_name == "Jakarta Province"] <- 10800000 # https://worldpopulationreview.com/world-cities/jakarta-population


# Step 3: Get coordinates ------------------------------------------------------------------------------

# Add coordinates of largest city and approximate centroid (source: google maps / wikipedia):
coordinates <- rbind.data.frame(
  c("Tamil Nadu State", 13.0836939, 80.270186, "10°45'N", "78°34'E"),
  c("Mumbai City", 19.0759899, 72.8773928, 19.0759899, 72.8773928),
  c("Jakarta Province", "-6°12′N", "106°49′E", "-6°12′N", "106°49′E"),
  c("Kolkata City", "22°34′N", "88°22′E", "22°34′N", "88°22′E"),
  c("Madhya Pradesh State", "22°43′N", "75°50′E", "23°48′N", "78°28′E"),
  c("Andhra Pradesh State", "17°42′N", "83°17′E", 16.50, 80.64))
# Convert these to decimal:
library(measurements)
colnames(coordinates) <- c("name",
                           "lat_largest_city",
                           "lng_largest_city",
                           "centroid_lat",
                           "centroid_long")
for(i in 2:ncol(coordinates)){coordinates[, i] <- as.character(coordinates[, i])}

for(i in 1:nrow(coordinates)){
  for(j in 2:ncol(coordinates)){
    if(is.na(as.numeric(coordinates[i, j]))){
      coordinates[i, j] <- gsub("°", " ",
                                coordinates[i, j])
      coordinates[i, j] <- gsub("′N", "",
                                coordinates[i, j])
      coordinates[i, j] <- gsub("′E", "",
                                coordinates[i, j])
      coordinates[i, j] <- measurements::conv_unit(coordinates[i, j], from = 'deg_dec_min', to = 'dec_deg')
    }
  }
}

for(i in 2:ncol(coordinates)){coordinates[, i] <- as.numeric(coordinates[, i])}

# Check that correctly converted:
inspect = F
if(inspect){
  library(tidyverse)
  world <- map_data("world")
  
  ggplot() +
    geom_map(
      data = world, map = world,
      aes(map_id = region),
      color = "black", size = 0.1)+
    geom_point(data = coordinates,
               aes(x= lng_largest_city,
                   y= lat_largest_city, col = name))+
    geom_point(data = coordinates,
               aes(x= centroid_long,
                   y= centroid_lat, col = name),
               alpha = 0.3)+theme_minimal()
}

# Step 4: Get mobility data ------------------------------------------------------------------------------

# Load mobility data:
# Google mobility reports:
mob_raw <- fread("https://www.gstatic.com/covid19/mobility/Global_Mobility_Report.csv")

# Select units:
mob <- mob_raw
mob$name <- NA
mob$name[mob$sub_region_1 == "Jakarta"] <- "Jakarta Province"
mob$name[mob$sub_region_1 == "Andhra Pradesh" & mob$sub_region_2 == ""] <- "Andhra Pradesh State"
mob$name[mob$sub_region_1 == "Madhya Pradesh" & mob$sub_region_2 == ""] <- "Madhya Pradesh State"
mob$name[mob$sub_region_1 == "Tamil Nadu" & mob$sub_region_2 == ""] <- "Tamil Nadu State"
mob$name[mob$sub_region_2 == "Kolkata"] <- "Kolkata City"
mob$name[mob$sub_region_2 == "Mumbai"] <- "Mumbai City"
mob <- mob[!is.na(mob$name), ]

# To inspect:
inspect = F
if(inspect){
  library(ggplot2)
  ggplot(mob, aes(x=date, y=residential_percent_change_from_baseline, col = name))+geom_line()
}

# Generate iso3c, rename and select columns
mob <- mob %>%
  mutate(
    iso3c = paste0(countrycode(country_region_code, "iso2c", "iso3c"), "_", name),
  ) %>%
  rename(
    mobility_retail_rec_pct_of_baseline = retail_and_recreation_percent_change_from_baseline,
    mobility_grocery_and_pharma_pct_of_baseline = grocery_and_pharmacy_percent_change_from_baseline,
    mobility_parks_pct_of_baseline = parks_percent_change_from_baseline,
    mobility_transit_rec_pct_of_baseline = transit_stations_percent_change_from_baseline,
    mobility_workplaces_rec_pct_of_baseline = workplaces_percent_change_from_baseline,
  ) %>%
  select(name,
         date,
         mobility_retail_rec_pct_of_baseline,
         mobility_grocery_and_pharma_pct_of_baseline,
         mobility_parks_pct_of_baseline,
         mobility_transit_rec_pct_of_baseline,
         mobility_workplaces_rec_pct_of_baseline)


# Step 5: Get covid data ------------------------------------------------------------------------------

# India:
# States/districts
# Source: https://data.covid19india.org/csv/latest/states.csv
ind_states <- read_csv("source-data/ind_states_ts.csv")

# Select states:
ind_states$name <- NA
ind_states$name[ind_states$State == "Tamil Nadu"] <- "Tamil Nadu State"
ind_states$name[ind_states$State == "Madhya Pradesh"] <- "Madhya Pradesh State"
ind_states$name[ind_states$State == "Andhra Pradesh"] <- "Andhra Pradesh State"
ind_states <- ind_states[!is.na(ind_states$name), ]

# Generate target columns:
ind_states$date <- ind_states$Date
ind_states <- ind_states[order(ind_states$Date), ]
ind_states$total_cases <- ind_states$Confirmed
ind_states$total_deaths <- ind_states$Deceased
ind_states$total_tests <- ind_states$Tested

# Deal with negative / zero values (causes unrealistic spikes in data)
fill_na_last_max <- function(x){
  
  temp <- x
  not_na <- which(!is.na(temp))
  if(length(not_na) > 0){
    
    # Convert to weekly median for interpolation
    for(i in which(!is.na(x))){
      temp[i] <- median(x[max(c(1, i-3)):min(c(length(x), i+3))], na.rm = T)
    }
    
    x[is.na(x) | x < cummax(ifelse(is.na(x), 0, x))] <- cummax(ifelse(is.na(temp), 0, x))[is.na(x) | x < cummax(ifelse(is.na(x), 0, x))]
    x[setdiff(1:length(x), min(not_na):max(not_na))] <- NA
  }
  x
}

ind_states <- ind_states[order(ind_states$date), ]
ind_states <- as.data.frame(ind_states)
for(i in c('total_cases', 'total_deaths', 'total_tests')){
  ind_states[, i] <- ave(ind_states[, i], ind_states$name, FUN = fill_na_last_max)
}

ind_states$new_cases <- ave(ind_states$total_cases, ind_states$name, FUN = function(x){
  x <- x - c(NA, x)[1:length(x)]
})
ind_states$new_deaths <- ave(ind_states$total_deaths, ind_states$name, FUN = function(x){
  x <- x - c(NA, x)[1:length(x)]
})
ind_states$new_tests <- ave(ind_states$total_tests, ind_states$name, FUN = function(x){
  x <- x - c(NA, x)[1:length(x)]
})

# Subset to target columns:
ind_states <- ind_states[, c("name", "date",
                             "total_cases",
                             "total_deaths",
                             "total_tests",
                             "new_cases",
                             "new_deaths",
                             "new_tests")]

ggplot(ind_states, aes(x=date))+geom_line(aes(y=total_tests))+facet_grid(.~name)
ggplot(ind_states, aes(x=date))+geom_line(aes(y=total_deaths))+facet_grid(.~name)
ggplot(ind_states, aes(x=date))+geom_line(aes(y=total_cases))+facet_grid(.~name)
ggplot(ind_states, aes(x=date))+geom_line(aes(y=new_tests))+facet_grid(.~name)
ggplot(ind_states, aes(x=date))+geom_line(aes(y=new_deaths))+facet_grid(.~name)
ggplot(ind_states, aes(x=date))+geom_line(aes(y=new_cases))+facet_grid(.~name)

# Source: https://api.covid19india.org/csv/latest/districts.csv  
ind_districts <- read_csv("source-data/ind_districts_ts.csv")

# Select districts:
ind_districts$name <- NA
ind_districts$name[ind_districts$District == "Mumbai"] <- "Mumbai City"
ind_districts$name[ind_districts$District == "Kolkata"] <- "Kolkata City"
ind_districts <- ind_districts[!is.na(ind_districts$name), ]

# Correctly indicate NA testing data for Mumbai:
ind_districts[ind_districts$name == 'Mumbai City' & ind_districts$Date > as.Date('2021-01-30'), 'Tested'] <- NA

# Generate target columns:
ind_districts$date <- ind_districts$Date
ind_districts <- ind_districts[order(ind_districts$Date), ]
ind_districts$total_cases <- ind_districts$Confirmed
ind_districts$total_deaths <- ind_districts$Deceased
ind_districts$total_tests <- ind_districts$Tested

# Deal with negative / zero values (causes unrealistic spikes in data)
ind_districts <- ind_districts[order(ind_districts$date), ]
ind_districts <- as.data.frame(ind_districts)
for(i in c('total_cases', 'total_deaths', 'total_tests')){
  ind_districts[, i] <- ave(ind_districts[, i], ind_districts$name, FUN = fill_na_last_max)
}

ind_districts$new_cases <- ave(ind_districts$total_cases, ind_districts$name, FUN = function(x){
  x <- x - c(NA, x)[1:length(x)]
})
ind_districts$new_deaths <- ave(ind_districts$total_deaths, ind_districts$name, FUN = function(x){
  x <- x - c(NA, x)[1:length(x)]
})
ind_districts$new_tests <- ave(ind_districts$total_tests, ind_districts$name, FUN = function(x){
  x <- x - c(NA, x)[1:length(x)]
})

# Subset to target columns:
ind_districts <- ind_districts[, c("name", "date",
                                   "total_cases",
                                   "total_deaths",
                                   "total_tests",
                                   "new_cases",
                                   "new_deaths",
                                   "new_tests")]

ggplot(ind_districts, aes(x=date))+geom_line(aes(y=total_cases))+facet_grid(.~name)
ggplot(ind_districts, aes(x=date))+geom_line(aes(y=total_deaths))+facet_grid(.~name)
ggplot(ind_districts, aes(x=date))+geom_line(aes(y=total_tests))+facet_grid(.~name)

# Indonesia (Jakarta):
# Source: https://corona.jakarta.go.id/en/data-pemantauan
jakarta <- read_csv("source-data/jakarta_ts.csv")[, 1:3]

# Fix date variable:
jakarta$date <- as.Date(jakarta$date, format = "%d/%m/%Y")

# Generate target columns:
jakarta <- jakarta[order(jakarta$date), ]
jakarta$new_cases <- jakarta$total_cases - c(NA, jakarta$total_cases)[1:nrow(jakarta)]
jakarta$new_deaths <- jakarta$total_deaths - c(NA, jakarta$total_deaths)[1:nrow(jakarta)]
jakarta$total_tests <- NA
jakarta$new_tests <- NA
jakarta$name <- "Jakarta Province"

jakarta <- jakarta[, c("name", "date",
                       "total_cases",
                       "total_deaths",
                       "total_tests",
                       "new_cases",
                       "new_deaths",
                       "new_tests")]
# Merge all together
sub_covid <- rbind(ind_states,
                   ind_districts, jakarta)

# If new cases, deaths or tests negative, then set as NA
sub_covid$new_cases[sub_covid$new_cases < 0] <- NA
sub_covid$new_deaths[sub_covid$new_deaths < 0] <- NA
sub_covid$new_tests[sub_covid$new_tests < 0] <- NA

# Step 6: Merge everything together ------------------------------------------------------------------------------
dat <- merge(lmort, mob, by = c("date", "name"), all.x = T)
dat <- merge(dat, coordinates, by = c("name"), all.x = T)
dat <- merge(dat, sub_covid, by = c("date", "name"),
             all.x = T)
dat <- dat[!is.na(dat$name), ]


# Set covid deaths and cases to 0 if none detected in country in which it is part
dat$total_cases[dat$date <= as.Date("2020-02-04") & dat$iso3c == "IND"] <- 0
dat$total_deaths[dat$date <= as.Date("2020-03-11") & dat$iso3c == "IND"] <- 0
dat$total_cases[dat$date <= as.Date("2020-03-07") & dat$iso3c == "IDN"] <- 0
dat$total_deaths[dat$date <= as.Date("2020-03-11") & dat$iso3c == "IDN"] <- 0

dat$new_cases[dat$date <= as.Date("2020-02-04") & dat$iso3c == "IND"] <- 0
dat$new_deaths[dat$date <= as.Date("2020-03-11") & dat$iso3c == "IND"] <- 0
dat$new_cases[dat$date <= as.Date("2020-03-07") & dat$iso3c == "IDN"] <- 0
dat$new_deaths[dat$date <= as.Date("2020-03-11") & dat$iso3c == "IDN"] <- 0

# 7-day smooth function
smooth <- function(x){
  temp <- x
  for(i in 1:length(x)){
    temp[i] <- mean(x[max(c(1,i-3)):min(c(length(x), i+3))], na.rm = T)
  }
  temp
}

dat <- dat[order(dat$date), ]
dat$new_cases_smoothed <- ave(dat$new_cases, dat$name, FUN = smooth)
dat$new_deaths_smoothed <- ave(dat$new_deaths, dat$name, FUN = smooth)
dat$new_tests_smoothed <- ave(dat$new_tests, dat$name, FUN = smooth)

# Generate columns corresponding to big dataset:
dat <- dat %>%
  mutate(
    daily_total_deaths = deaths,
    daily_total_deaths_per_100k = deaths*100000/population,
    daily_expected_deaths = expected_deaths,
    daily_expected_deaths_per_100k = expected_deaths*100000/population,
    daily_excess_deaths = excess_deaths,
    daily_excess_deaths_per_100k = excess_deaths*100000/population,
    cumulative_daily_tests_per_100k = total_tests*1e5/population,
    cumulative_daily_covid_cases_per_100k = total_cases*1e5/population,
    cumulative_daily_covid_deaths_per_100k = total_deaths*1e5/population,
    daily_covid_deaths = new_deaths_smoothed,
    daily_covid_deaths_per_100k = (daily_covid_deaths / population) * 100000,
    daily_covid_cases = new_cases_smoothed,
    daily_covid_cases_per_100k = (daily_covid_cases / population) * 100000,
    daily_tests = new_tests_smoothed,
    daily_tests_per_100k = (daily_tests / population) * 100000,
    daily_positive_rate = (daily_covid_cases / daily_tests) * 100,
    daily_covid_cases_raw = new_cases, 
    daily_covid_deaths_raw = new_deaths) %>%
  rename(centroid_latitude = centroid_lat,
         centroid_longitude = centroid_long,
         lat_capital = lat_largest_city,
         lng_capital = lng_largest_city)

# To check which data we are missing at the sub-regional level:
#
dat$population_density <- NA
dat$population_density[dat$local_unit_name == "Jakarta Province" ] <- 15906.5
dat$population_density[dat$local_unit_name == "Kolkata City"] <- 22000
dat$population_density[dat$local_unit_name == "Tamil Nadu State"] <- 550
dat$population_density[dat$local_unit_name == "Mumbai City"] <- 21000
dat$population_density[dat$local_unit_name == "Andhra Pradesh State"] <- 308
dat$population_density[dat$local_unit_name == "Madhya Pradesh State"] <- 240

# setdiff(colnames(country_daily_excess_deaths)[1:93], colnames(dat))

# Removing a few columns to hard-code consistency with main dataset:
dat$time <- NULL
dat$ID <- NULL

inspect = F
if(inspect){
  pdat <- dat[dat$year <= 2022, ]
  for(i in colnames(pdat)){
    pdat$plot <- pdat[, i]
    print(ggplot(pdat, aes(x=date, y=plot, col=name))+geom_line()+geom_point()+geom_vline(aes(xintercept = as.Date('2021-05-31')))+ggtitle(i))
    readline(prompt="Press [enter] to continue")
  }
}

# Step 7: add China subnational data -----------------------------------------------------------------
# Source: the annual Death Cause Surveillance Dataset published by the China CDC
# https://ncncd.chinacdc.cn/xzzq_1/202101/t20210111_223706.htm

china <- read_csv('source-data/China_CDC_Death_Cause_Surveillance_Dataset.csv')

# Get population of observation areas, and restrict to relevant years and units:
china$`Population in the observation areas` <- ave(china$`Population in the observation areas`, china$Year, FUN = function(x) na.omit(x)[1])
china$Year <- as.numeric(china$Year)
china$Month <- match(china$Month, month.name)
china <- na.omit(china[china$Month != 'Total' & china$Year >= 2015, ])

# Rename columns:
colnames(china) <- c('year', 'month', 'percent', 'deaths', 'population')

# Generate expected deaths
china$expected_deaths <- predict(newdata = china,
                                 lm(deaths ~ as.factor(month)+year,
                                    data = china[china$year %in% 2015:2019, ]))

# Convert to daily deaths
dates <- data.frame(date = as.Date((-365*20+Sys.Date()):Sys.Date(), origin = origin))
dates$month <- month(dates$date)
dates$year <- year(dates$date)
dates$n_month <- 1
china <- merge(china, dates, all.x=T)
china$n_month <- ave(china$n_month, paste(china$year, '-', china$month), FUN = sum)
for(i in c('deaths', 'expected_deaths')){
  china[, i] <- china[, i]/china$n_month
}

china <- china %>%
  select(-percent, -n_month) %>%
  mutate(
    excess_deaths = deaths - expected_deaths,
    daily_total_deaths = deaths,
    daily_total_deaths_per_100k = deaths*100000/population,
    daily_expected_deaths = expected_deaths,
    daily_expected_deaths_per_100k = expected_deaths*100000/population,
    daily_excess_deaths = excess_deaths,
    daily_excess_deaths_per_100k = excess_deaths*100000/population)

# Merge in other covariates
china_covars <- readRDS("output-data/country_daily_excess_deaths_with_covariates.RDS")
china_covars <- china_covars[china_covars$iso3c == 'CHN', ]

# Record when China proper excess deaths data ends: 
full_china_data_ends <- max(china_covars$date[!is.na(china_covars$daily_excess_deaths_per_100k)])

# Select covariates that are supplied subnationally (and excludes excess deaths data in the newly captured data)
china_covars <- china_covars[, !colnames(china_covars) %in% setdiff(colnames(china), 'date') & colnames(china_covars) %in% colnames(dat)] 
china <- merge(china, china_covars, by='date')

# Assume that the sample is representative (as asserted):
china$daily_covid_deaths <- china$daily_covid_deaths * (china$population / 1402000000)
china$daily_covid_cases <- china$daily_covid_cases * (china$population / 1402000000)
china$daily_tests <- china$daily_tests * (china$population / 1402000000)

# Specify source country:
china$iso3c <- 'CHN'
china$name <- 'CDC_DCSD'

# Remove dates for which data for entire country is available from separate source: 
china <- china[china$date > full_china_data_ends, ]

# Inspect the result
ggplot(china, aes(x=as.Date(date), y=deaths))+geom_line()+geom_line(aes(y=expected_deaths,  col='expected'))+geom_line(aes(y=deaths-expected_deaths))

# Merge into rest of subnational data
china[, setdiff(colnames(dat), colnames(china))] <- NA
china <- china[china$date >= as.Date('2020-01-01', origin = origin), colnames(dat)]
dat <- rbind(dat, china) 
dat_bc <- dat

# Step 8: Impute leading zeroes and vaccination data if applicable------------------------------------------------------------------------------

# Fix for NA testing data
dat$daily_positive_rate[dat$daily_positive_rate > 100] <- NA

# Restrict to 2020 +
dat <- dat[dat$date >= as.Date("2020-01-01"), ]

# Fill in leading 0s for covid data:
# Sort
dat <- dat[order(dat$date), ]

# Define function
leading_zeros <- function(x){
  if(is.na(x[1]) & sum(is.na(x)) != length(x)){
    x[1:min(which(!is.na(x))-1)] <- 0
  }
  x
}

# Cycle through relevant columns an impute leading zeroes
dat <- dat[order(dat$date), ]
for(i in c("daily_covid_deaths", "daily_covid_deaths_per_100k",
           "daily_covid_cases", "daily_covid_cases_per_100k",
           "daily_total_deaths",
           "daily_total_deaths_per_100k")){
  dat[, i] <-
    ave(dat[, i],
        dat$name,
        FUN = function(x) leading_zeros(x))
}

# Converting NaN to NA:
for(i in colnames(dat)){
  dat[is.nan(dat[,i ]), i] <- NA
}

# Impute zero vaccinations if applicable:
vacc_covars <- readRDS("output-data/country_daily_excess_deaths_with_covariates.RDS")
vacc_covars <- vacc_covars[vacc_covars$iso3c %in% unique(dat$iso3c), ]

# Add leading zeroes to variables related to vaccinations - as by definition no subunit of a country can start vaccinations before the country they are part of:
vaccination_variables <- c("daily_vaccinations_per_100k",
                           "daily_vaccinations",
                           "vaccinated_pct",
                           "fully_vaccinated_pct",
                           "cumulative_daily_vaccinations_per_100k")

for(i in vaccination_variables){
  dat[, i] <- NA
  for(j in unique(dat$iso3c)){
    if(sum(!is.na(vacc_covars[vacc_covars$iso3c == j, i])) > 0){
      dat[dat$iso3c == j & dat$date < min(vacc_covars$date[vacc_covars$iso3c == j & !is.na(vacc_covars[, i])])] <- 0
    }
  }
}

# Step 9: Write to file ------------------------------------------------------------------------------
saveRDS(dat, "output-data/model-objects/auxilliary_subnational_data.RDS")
