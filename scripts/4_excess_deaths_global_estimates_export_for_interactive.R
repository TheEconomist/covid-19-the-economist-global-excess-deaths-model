# Step 1: import libraries ------------------------------------------------------------------------------
cat('\n Step 1: import libraries')

# This script constructs custom data frames to populate the The Economist's interactive presentation of this estimates

# Import libraries
library(tidyverse)
library(data.table)
library(lubridate)
library(readr)
library(countrycode)
library(ggplot2)
options(scipen=999)
inspect <- F

# Step 2: import official covid-19 data from Our World in Data ------------------------------------------------------------------------------
cat('\n Step 2: import official covid-19 data')

## A. Import official covid data from Our World In Data
country_daily_data <- fread("https://raw.githubusercontent.com/owid/covid-19-data/master/public/data/owid-covid-data.csv") 
country_daily_data <- country_daily_data[order(country_daily_data$date), 
                                         c("location",
                                           "date",
                                           "iso_code",
                                           "continent",
                                           "population",
                                           "new_deaths",
                                           "new_deaths_smoothed",
                                           "total_deaths",
                                           "people_vaccinated",
                                           "people_fully_vaccinated",
                                           "new_vaccinations_smoothed",
                                           "total_vaccinations",
                                           "aged_65_older",
                                           "aged_70_older")]
country_data <- unique(country_daily_data[, c('location', 'iso_code', 'aged_65_older', 'aged_70_older')]) 

# After the switch from JHU to WHO data, OWID sometimes misses country-days. We ensure all country-days are present here:
unique_dates <- unique(country_daily_data$date)
cat('\nAdding missing OWID country-dates: ')
country_daily_data <- country_daily_data[order(country_daily_data$date), ]
for(i in setdiff(unique(country_daily_data$location), c('Northern Cyprus', 'Wales', 'Western Sahara', 'Taiwan', 'Macau'))){
  for(j in (Sys.Date()-10):Sys.Date()){
    if(!any(country_daily_data$date == j & country_daily_data$location == i)){
      temp <- country_daily_data[country_daily_data$date == max(country_daily_data$date[country_daily_data$date < j]) & country_daily_data$location == i, ]
      if(nrow(temp) == 0){
        stop(paste0('Data missing for more than 10 days for ', i))
      }
      temp[, c("new_deaths",
               "new_deaths_smoothed")] <- NA
      temp$date <- as.IDate(j, origin = '1970-01-01')
      
      country_daily_data <- rbind(country_daily_data, temp)
      cat(paste0(j, '-', i, '..'))
    }
  }
}
cat('\nAdding missing OWID country-dates - completed.')

## Use last known value for total deaths and vaccinations (if known within the last month):
country_daily_data <- country_daily_data[order(country_daily_data$date), ]

fill_if_known_in_last_30 <- function(x){
  temp <- x
  for(i in 1:length(x)){
    if(is.na(x[i])){
      x[i] <- rev(c(NA, na.omit(temp[max(c(1, i-30)):(i-1)])))[1]
    }
  }
  x
}
country_daily_data$total_deaths <- as.numeric(ave(country_daily_data$total_deaths, country_daily_data$location, FUN = fill_if_known_in_last_30))
country_daily_data$total_vaccinations <- as.numeric(ave(country_daily_data$total_vaccinations, country_daily_data$location, FUN = fill_if_known_in_last_30))

## Use last known value for new deaths (if known within the last 15 days):
country_daily_data <- country_daily_data[order(country_daily_data$date), ]

fill_if_known_in_last_10 <- function(x){
  temp <- x
  for(i in 1:length(x)){
    if(is.na(x[i])){
      x[i] <- rev(c(NA, na.omit(temp[max(c(1, i-15)):(i-1)])))[1]
    }
  }
  x
}
country_daily_data$new_deaths <- as.numeric(ave(country_daily_data$new_deaths, country_daily_data$location, FUN = fill_if_known_in_last_30))


# Create 7-day average of deaths for use in graphics (replacing the OWD version)
country_daily_data$new_deaths_smoothed <- ave(country_daily_data$new_deaths, country_daily_data$location, FUN = function(x){
  unlist(lapply(1:length(x), FUN = function(i){
    mean(x[max(c(1,i-7)):i], na.rm = T)
  }))
})

# Step 3: Generate data for main map ------------------------------------------------------------------------------
cat('\n Step 3: Generate data for main map')

# Import data:
daily <- read_csv("output-data/export_country_per_100k.csv", show_col_types = F)
cumulative <- read_csv("output-data/export_country_per_100k_cumulative.csv", show_col_types = F)

# Reshape the OWD data to prepare for merging with our estimates (create per 100k, etc)
main_map_covid_data <- country_daily_data[country_daily_data$date > max(country_daily_data$date)-7, ]
main_map_covid_data$sum_obs <- as.numeric(ave(main_map_covid_data$new_deaths_smoothed, main_map_covid_data$date, FUN = function(x) sum(!is.na(x))))
main_map_covid_data <- main_map_covid_data[main_map_covid_data$date == max(main_map_covid_data$date[main_map_covid_data$sum_obs > 50]), ]
main_map_covid_data <- main_map_covid_data %>%
  mutate(date = as.Date(date),
         iso3c = iso_code,
         daily_covid_deaths_per_100k = (new_deaths_smoothed / population) * 100000,
         cumulative_covid_deaths_per_100k = (total_deaths / population) * 100000) %>%
  mutate(iso3c = ifelse(location == 'Kosovo', 'KSV', iso3c)) %>%
  filter(date == max(date)) %>% # Select most recent date
  dplyr::select(iso3c,daily_covid_deaths_per_100k,cumulative_covid_deaths_per_100k)

# Merge together
main_map <- merge(daily[daily$date == max(daily$date), c("iso3c", "date",  "estimated_daily_excess_deaths_per_100k", "estimated_daily_excess_deaths_ci_95_top_per_100k",
                                                         "estimated_daily_excess_deaths_ci_95_bot_per_100k")], 
                  cumulative[cumulative$date == max(cumulative$date), c("iso3c", "cumulative_estimated_daily_excess_deaths_per_100k",
                                                                        "cumulative_estimated_daily_excess_deaths_ci_95_top_per_100k",
                                                                        "cumulative_estimated_daily_excess_deaths_ci_95_bot_per_100k")])
main_map <- merge(main_map, main_map_covid_data, all.x=T)

# Write to file
write_csv(main_map, "output-data/output-for-interactive/main_map.csv")
rm(daily)
rm(cumulative)
rm(main_map)
rm(main_map_covid_data)
gc()

# Step 4: World, country or region by day (data generation) ------------------------------------------------------------------------------
cat('\n Step 4: World, country or region by day (data generation)')

# Get official covid data in long format:
cat('\n - Step 4.1')
# 1. Generate custom regions not in Our World in Data: North America and Latin America
lat_am <- data.frame(country_daily_data[country_daily_data$continent %in% 
                                          c("North America", "South America") &
                                          !country_daily_data$location %in% 
                                          c("United States", "Canada"), ])
lat_am$location <- "Latin America and Caribbean"
north_am <- data.frame(country_daily_data[country_daily_data$location %in% 
                                            c("United States", "Canada"), ])
north_am$location <- "North America"

for(i in c("new_deaths_smoothed",
           "total_deaths",
           "new_vaccinations_smoothed", 
           "total_vaccinations",
           "people_vaccinated",
           "people_fully_vaccinated",
           "population")){
  lat_am[, i] <- ave(lat_am[, i], lat_am$date, 
                     FUN = function(x) sum(x, na.rm = T))
  north_am[, i] <- ave(north_am[, i], north_am$date, 
                       FUN = function(x) sum(x, na.rm = T))
}

# Remove duplicate dates
lat_am <- lat_am[!duplicated(lat_am$date), ]
north_am <- north_am[!duplicated(north_am$date), ]

# Ensuring populations are consistent across time (not confounded by missing data)
lat_am$population <- max(lat_am$population)
north_am$population <- max(north_am$population)

# Set other data to NA (not used)
lat_am[, setdiff(colnames(lat_am), c("location", 
                                     "date", 
                                     "new_deaths_smoothed",
                                     "total_deaths",
                                     "new_vaccinations_smoothed", 
                                     "total_vaccinations",
                                     "people_vaccinated",
                                     "people_fully_vaccinated",
                                     "population"))] <- NA
north_am[, setdiff(colnames(north_am), c("location", 
                                         "date", 
                                         "new_deaths_smoothed",
                                         "total_deaths",
                                         "new_vaccinations_smoothed", 
                                         "total_vaccinations",
                                         "people_vaccinated",
                                         "people_fully_vaccinated",
                                         "population"))] <- NA

# Append to central dataset, and select subset of this dataset to prepare for merging:
covid_data_long <- rbind(country_daily_data[country_daily_data$location != "North America", ], lat_am, north_am) %>%
  mutate(date = as.Date(date),
         iso3c = iso_code,
         total_vaccinations_per_100 = (total_vaccinations / population) * 100,
         daily_vaccinations = new_vaccinations_smoothed,
         daily_vaccinations_per_100 = 100*new_vaccinations_smoothed / population,
         vaccinated = people_vaccinated,
         vaccinated_per_100 = 100*people_vaccinated / population, 
         fully_vaccinated = people_fully_vaccinated,
         fully_vaccinated_per_100 = 100*people_fully_vaccinated / population,
         daily_covid_deaths = new_deaths_smoothed,
         daily_covid_deaths_per_100k = (new_deaths_smoothed / population) * 100000,
         cumulative_covid_deaths = total_deaths,
         cumulative_covid_deaths_per_100k = (total_deaths / population) * 100000) %>%
  mutate(iso3c = ifelse(location == 'Kosovo', 'KSV', iso3c)) %>%
  dplyr::select(iso3c,
                location,
                date,
                population,
                daily_vaccinations,
                daily_vaccinations_per_100,
                total_vaccinations,
                total_vaccinations_per_100,
                vaccinated,
                vaccinated_per_100,
                fully_vaccinated,
                fully_vaccinated_per_100,
                daily_covid_deaths,
                daily_covid_deaths_per_100k,
                cumulative_covid_deaths,
                cumulative_covid_deaths_per_100k) %>%
  pivot_longer(cols = daily_covid_deaths:cumulative_covid_deaths_per_100k) %>% 
  dplyr::rename(population_owd = population,
                official_covid_data_type = name,
                official_covid_deaths = value)
rm(country_daily_data)
rm(lat_am)
rm(north_am)

# Add correspondence to facilitate later merging:
covid_data_long$merge_column <- NA  
covid_data_long$merge_column[covid_data_long$official_covid_data_type == "daily_covid_deaths"] <- "daily_excess_deaths"  
covid_data_long$merge_column[covid_data_long$official_covid_data_type == "daily_covid_deaths_per_100k"] <- "daily_excess_deaths_per_100k"  
covid_data_long$merge_column[covid_data_long$official_covid_data_type == "cumulative_covid_deaths"] <- "daily_excess_deaths_cumulative"  
covid_data_long$merge_column[covid_data_long$official_covid_data_type == "cumulative_covid_deaths_per_100k"] <- "daily_excess_deaths_per_100k_cumulative"  

# 2. Define function to get model estimates in "long" format:
cat('\n - Step 4.2')

long_exp_df <- function(files = c("output-data/export_world.csv",
                                  "output-data/export_world_per_100k.csv",
                                  "output-data/export_world_cumulative.csv",
                                  "output-data/export_world_per_100k_cumulative.csv"),
                        types = c("daily_excess_deaths",
                                  "daily_excess_deaths_cumulative",
                                  "daily_excess_deaths_per_100k_cumulative",
                                  "daily_excess_deaths_per_100k"),
                        col_names = c("location", 
                                      "date", 
                                      "population", 
                                      "estimate", 
                                      "estimate_top_95",
                                      "estimate_top_90",
                                      "estimate_top_50",
                                      "estimate_bot_50",
                                      "estimate_bot_90",
                                      "estimate_bot_95",
                                      "raw_estimate",
                                      "recorded",
                                      "daily_covid_deaths")){
  
  # Check that type is provided for each
  if(length(files) != length(types)){
    stop("Please provide a type for each file.")
  }
  
  # Create container data frame
  result <- data.frame()
  
  # Cycle through files, rename columns, add type, and bind together
  for(i in 1:length(files)){
    temp <- read_csv(files[i], show_col_types = F)
    colnames(temp) <- col_names
    temp$type <- types[i]
    
    result <- rbind(result, temp)
  }
  
  # Return combined data
  return(result)
}

# 3. Load world estimates
cat('\n - Step 4.3')

world_long <- long_exp_df(files = c("output-data/export_world.csv",
                                    "output-data/export_world_per_100k.csv",
                                    "output-data/export_world_cumulative.csv",
                                    "output-data/export_world_per_100k_cumulative.csv"),
                          types = c("daily_excess_deaths",
                                    "daily_excess_deaths_per_100k",
                                    "daily_excess_deaths_cumulative",
                                    "daily_excess_deaths_per_100k_cumulative"))

# 4. Load region estimates
cat('\n - Step 4.4')

region_long <- long_exp_df(files = c("output-data/export_regions.csv",
                                     "output-data/export_regions_per_100k.csv",
                                     "output-data/export_regions_cumulative.csv",
                                     "output-data/export_regions_per_100k_cumulative.csv"),
                           types = c("daily_excess_deaths",
                                     "daily_excess_deaths_per_100k",
                                     "daily_excess_deaths_cumulative",
                                     "daily_excess_deaths_per_100k_cumulative"))

# As we prefer Canada + United States as North America, we remove the duplicate (see below where this is added). We also remove countries which figure separately in the standard region specification
region_long <- region_long[!region_long$location %in% c("North America", "Asia", "Oceania",
                                                        "China",
                                                        "India",
                                                        "Russia",
                                                        "United States"),]

# Load alternative region estimates (Lat Am vs North America, EU as separate entity)
region_alt <- long_exp_df(files = c("output-data/output-by-alternative-regions/export_regions_lat_am_na_eu.csv",
                                    "output-data/output-by-alternative-regions/export_regions_lat_am_na_eu_per_100k.csv",
                                    "output-data/output-by-alternative-regions/export_regions_lat_am_na_eu_cumulative.csv",
                                    "output-data/output-by-alternative-regions/export_regions_lat_am_na_eu_per_100k_cumulative.csv"),
                          types = c("daily_excess_deaths",
                                    "daily_excess_deaths_per_100k",
                                    "daily_excess_deaths_cumulative",
                                    "daily_excess_deaths_per_100k_cumulative"))


# 5. Load country estimates
cat('\n - Step 4.5')

country_long <- long_exp_df(files = c("output-data/export_country.csv",
                                      "output-data/export_country_per_100k.csv",
                                      "output-data/export_country_cumulative.csv",
                                      "output-data/export_country_per_100k_cumulative.csv"),
                            types = c("daily_excess_deaths",
                                      "daily_excess_deaths_per_100k",
                                      "daily_excess_deaths_cumulative",
                                      "daily_excess_deaths_per_100k_cumulative"),
                            col_names = c("iso3c", 
                                          "date", 
                                          "population", 
                                          "estimate", 
                                          "estimate_top_95",
                                          "estimate_top_90",
                                          "estimate_top_50",
                                          "estimate_bot_50",
                                          "estimate_bot_90",
                                          "estimate_bot_95",
                                          "raw_estimate",
                                          "recorded",
                                          "daily_covid_deaths"))

# 6. Harmonize location names
cat('\n - Step 4.6')

country_long <- merge(country_long, unique(covid_data_long[, c("location", "iso3c")]),
                      by="iso3c", all.x = T)
region_long$iso3c <- NA
region_alt$iso3c <- NA
world_long$iso3c <- NA

# 7. Bind all of these together
cat('\n - Step 4.7')

export_long <- rbind(world_long, 
                     region_long,
                     region_alt,
                     country_long)
rm(world_long)
rm(region_long)
rm(region_alt)
country_long_iso3c <- unique(country_long$iso3c)
rm(country_long)
                       
export_long$daily_covid_deaths <- NULL # Remove this, as we are getting this data in the next step

# 8. Merge with official covid data
cat('\n - Step 4.8')

covid_data_long <- covid_data_long[covid_data_long$date %in% export_long$date & covid_data_long$location %in% export_long$location, ]
export_long <- merge(export_long,
                     covid_data_long[ , setdiff(colnames(covid_data_long), "iso3c")], 
                     by.x=c("type", "location", "date"),
                     by.y=c("merge_column", "location", "date"),

                                          all.x = T)

# 9. Make names follow The Economist standard:
cat('\n - Step 4.9 a')
econ_names <- read_csv("source-data/economist_country_names.csv", show_col_types = F) %>%
  rename(
    econ_name = Name,
    economist_region = Regions,
    iso3c = ISOA3
  ) %>%
  select(
    econ_name, 
    iso3c,
    economist_region
  ) %>% unique()

econ_names <- econ_names[!is.na(econ_names$iso3c) & econ_names$iso3c %in% export_long$iso3c, ]
econ_names <- econ_names[!duplicated(econ_names$iso3c), ]
                       
export_long <- merge(export_long, econ_names[, ],
                     by = "iso3c", all.x = T)

export_long$location[!is.na(export_long$econ_name)] <- export_long$econ_name[!is.na(export_long$econ_name)]
export_long$econ_name <- NULL
rm(econ_names)
                       
# Construct "is recorded" dummy variable:
cat('\n - Step 4.9 b')

export_long$known_excess_deaths <- FALSE
export_long$known_excess_deaths[!is.na(export_long$recorded)] <- TRUE
export_long$known_excess_deaths[export_long$type %in% c("daily_excess_deaths_cumulative", "daily_excess_deaths_per_100k_cumulative")] <- FALSE

# No region reports excess deaths directly:
export_long$known_excess_deaths[!export_long$iso3c %in% country_long_iso3c] <- FALSE

# The EU reports it for all countries for some dates:
# This cycles through all dates, checks if all EU countries have reported excess deaths, and if so, sets known_excess_deaths for the EU to "TRUE":
for(i in unique(export_long$date)){
  if(min(as.numeric(export_long$known_excess_deaths[export_long$iso3c %in% c("AUT", "BEL", "BGR", "HRV", "CYP", "CZE", "DNK", "EST", "FIN", "FRA", "DEU", "GRC", "HUN", "IRL", "ITA", "LVA", "LTU", "LUX", "MLT", "NLD", "POL", "PRT", "ROU", "SVK", "SVN", "ESP", "SWE") & export_long$date == i & export_long$type %in% c("daily_excess_deaths_per_100k", "daily_excess_deaths") ])) == 1){
    export_long$known_excess_deaths[export_long$type %in% c("daily_excess_deaths_per_100k", "daily_excess_deaths") & export_long$date == i & export_long$location == "European Union"] <- TRUE
  }
}

# This cycles through countries, and finds the latest date before which all numbers are based on recorded excess deaths. It then sets the "known_excess_deaths" to "TRUE" before that date for the cumulative totals.
export_long <- export_long[order(export_long$date), ]
for(i in unique(export_long$location)){
  temp <- export_long[export_long$location == i & export_long$type == "daily_excess_deaths", c("date", "known_excess_deaths")]
  if(sum(!temp$known_excess_deaths) > 0){
  export_long$known_excess_deaths[export_long$location == i & 
                export_long$type %in% c("daily_excess_deaths_cumulative",
                                        "daily_excess_deaths_per_100k_cumulative") &
                export_long$date < min(temp$date[!temp$known_excess_deaths])] <- TRUE
  }
}
rm(temp)

# Inspect if desired:
if(inspect){
  type <- "daily_excess_deaths"
  ggplot(export_long[export_long$location %in% c("United States",
                                                 "European Union", "Europe",
                                                 "World", "Latin America and Caribbean") & 
                       !is.na(export_long$location) &
                       export_long$type == type, ], 
         aes(x=date, y=official_covid_deaths, col = "official"))+
    geom_line(aes(col = "estimate"))+
    geom_line(aes(y=daily_vaccinations/1000, col = "Daily vaccinations, 1000s"))+
    geom_line(aes(y=estimate, col = "model estimate"))+xlab("")+
    facet_grid(.~location)+theme_minimal()+theme(legend.position = "bottom")+ylab("")+ggtitle(type)
  
  type <- "daily_excess_deaths_cumulative"
  ggplot(export_long[export_long$location %in% c("United States",
                                                 "European Union", "Europe",
                                                 "World", "Latin America and Caribbean")  & 
                       !is.na(export_long$location) &
                       export_long$type == type, ], 
         aes(x=date, y=official_covid_deaths, col = "official"))+
    geom_line(aes(col = "estimate"))+
    geom_line(aes(y=total_vaccinations/1000, col = "Total vaccinations, 1000s"))+
    geom_line(aes(y=estimate, col = "model estimate"))+xlab("")+
    facet_grid(.~location)+theme_minimal()+theme(legend.position = "bottom")+ylab("")+ggtitle(type)
  
  type <- "daily_excess_deaths_per_100k"
  ggplot(export_long[export_long$location %in% c("United States",
                                                 "European Union", "Europe",
                                                 "World", "Latin America and Caribbean")  & 
                       !is.na(export_long$location) &
                       export_long$type == type, ], 
         aes(x=date, y=official_covid_deaths, col = "official"))+
    geom_line(aes(col = "estimate"))+
    geom_line(aes(y=estimate, col = "model estimate"))+
    geom_line(aes(y=total_vaccinations_per_100/100, col = "Vaccinations per person"))+
    xlab("")+
    facet_grid(.~location)+theme_minimal()+theme(legend.position = "bottom")+ylab("")+ggtitle(type)
  
  type <- "daily_excess_deaths_per_100k_cumulative"
  ggplot(export_long[export_long$location %in% c("United States",
                                                 "European Union", "Europe",
                                                 "World", "Latin America and Caribbean")  & 
                       !is.na(export_long$location) &
                       export_long$type == type, ], 
         aes(x=date, y=official_covid_deaths, col = "official"))+
    geom_line(aes(col = "estimate"))+
    geom_line(aes(y=estimate, col = "model estimate"))+
    geom_line(aes(y=total_vaccinations_per_100, col = "Vaccinations per 100"))+
    facet_grid(.~location)+theme_minimal()+ylab("")+ggtitle(type)+xlab("")
}

# 10. Round location files to 2 digits for interactive
cat('\n - Step 4.10')
for(i in c("estimate", "estimate_top_95", "estimate_top_90", "estimate_top_50", "estimate_bot_50", "estimate_bot_90", "estimate_bot_95", "raw_estimate", "recorded", "official_covid_deaths")){
  export_long[, i] <- round(export_long[, i], 3)
}

# 11. Sort by date
cat('\n - Step 4.11')
export_long <- export_long[order(export_long$date), ]

# Step 5: Write line charts to files (and world cumulative for most recent date) ------------------------------------------------------------------------------
cat('\n Step 5: Write line charts to files (and world cumulative for most recent date)')

# Select columns to include:
columns_to_include <- c("location", "date", "type", "estimate", 
                         "estimate_top_95", 
                         "estimate_top_50",
                         "estimate_bot_50",
                         "estimate_bot_95",
                         "official_covid_deaths", 
                        "known_excess_deaths")
columns_to_export <- setdiff(columns_to_include, "type")

# By country:
cat('\n - Step 5.1')
# This file is updated once a week to conserve space on github:
if(max(read_csv("output-data/output-for-interactive/by_location_full_data.csv")$date) <= Sys.Date()-7){
  write_csv(export_long[!export_long$location %in% c("Africa", "Oceania", "Americas", "Asia", "Asia & Oceania", "Europe",  "Latin America and Caribbean", "North America", "World"), ], "output-data/output-for-interactive/by_location_full_data.csv")
  }

write_csv(export_long[!export_long$location %in% c("Africa", "Oceania", "Americas", "Asia", "Asia & Oceania", "Europe",  "Latin America and Caribbean", "North America", "World") & export_long$type == "daily_excess_deaths", columns_to_export], 
          "output-data/output-for-interactive/by_location.csv")

write_csv(export_long[!export_long$location %in% c("Africa", "Oceania", "Americas", "Asia", "Asia & Oceania", "Europe",  "Latin America and Caribbean", "North America", "World") & export_long$type == "daily_excess_deaths_per_100k", columns_to_export], 
          "output-data/output-for-interactive/by_location_per_100k.csv")

write_csv(export_long[!export_long$location %in% c("Africa", "Oceania", "Americas", "Asia", "Asia & Oceania", "Europe",  "Latin America and Caribbean", "North America", "World") & export_long$type == "daily_excess_deaths_cumulative", columns_to_export], "output-data/output-for-interactive/by_location_cumulative.csv")

write_csv(export_long[!export_long$location %in% c("Africa", "Oceania", "Americas", "Asia", "Asia & Oceania", "Europe",  "Latin America and Caribbean", "North America", "World") & export_long$type == "daily_excess_deaths_per_100k_cumulative", columns_to_export], "output-data/output-for-interactive/by_location_per_100k_cumulative.csv")

# By region:
cat('\n - Step 5.2')
# Select regions
regions_line_chart <- export_long[export_long$location %in% c("Africa", "Asia", "Oceania", "Europe",  "Latin America and Caribbean", "North America"), columns_to_include]

# Inspect if desired
if(inspect){
  ggplot(regions_line_chart[regions_line_chart$type == "daily_excess_deaths",], 
         aes(x=date))+
    geom_area(aes(y=official_covid_deaths, fill=location))+
    geom_line(aes(y=estimate, 
                  col = "Estimated excess deaths"))+
    geom_line(aes(y=estimate_top_95))+
    geom_line(aes(y=estimate_bot_95))+
    geom_line(aes(y=estimate_top_50, alpha= 0.5))+
    geom_line(aes(y=estimate_bot_50, alpha= 0.5))+
    theme_minimal()+theme(legend.position = "bottom", legend.title = element_blank())+
    xlab("")+ylab("")+facet_grid(.~location)
}

# Write to files
write_csv(regions_line_chart[regions_line_chart$type == "daily_excess_deaths", 
                             columns_to_export], 
          "output-data/output-for-interactive/regions_line_chart.csv")
write_csv(regions_line_chart[regions_line_chart$type == "daily_excess_deaths_cumulative",
                             columns_to_export], 
          "output-data/output-for-interactive/regions_line_chart_cumulative.csv")
write_csv(regions_line_chart[regions_line_chart$type == "daily_excess_deaths_per_100k",
                             columns_to_export], 
          "output-data/output-for-interactive/regions_line_chart_per_100k.csv")
write_csv(regions_line_chart[regions_line_chart$type == "daily_excess_deaths_per_100k_cumulative", columns_to_export], "output-data/output-for-interactive/regions_line_chart_per_100k_cumulative.csv")

# For the world:
cat('\n - Step 5.3')
world_line_chart <- export_long[export_long$location == "World", columns_to_include]

# Inspect if desired
if(inspect){
  ggplot(world_line_chart[world_line_chart$type == "daily_excess_deaths_cumulative",], 
         aes(x=date))+
    geom_area(aes(y=official_covid_deaths, fill=location))+
    geom_line(aes(y=estimate, 
                  col = "Estimated excess deaths"))+
    geom_line(aes(y=estimate_top_95))+
    geom_line(aes(y=estimate_bot_95))+
    geom_line(aes(y=estimate_top_50, alpha= 0.5))+
    geom_line(aes(y=estimate_bot_50, alpha= 0.5))+
    theme_minimal()+theme(legend.position = "bottom", legend.title = element_blank())+
    xlab("")+ylab("")+facet_grid(.~location)
}

# Write to files
write_csv(world_line_chart[world_line_chart$type == "daily_excess_deaths", 
                           columns_to_export], 
          "output-data/output-for-interactive/world_line_chart.csv")
write_csv(world_line_chart[world_line_chart$type == "daily_excess_deaths_cumulative",
                           columns_to_export], 
          "output-data/output-for-interactive/world_line_chart_cumulative.csv")
write_csv(world_line_chart[world_line_chart$type == "daily_excess_deaths_per_100k",
                           columns_to_export], 
          "output-data/output-for-interactive/world_line_chart_per_100k.csv")
write_csv(world_line_chart[world_line_chart$type == "daily_excess_deaths_per_100k_cumulative", columns_to_export], "output-data/output-for-interactive/world_line_chart_per_100k_cumulative.csv")

# World top-line chart:
cat('\n - Step 5.4')
world_top_line_chart <- world_line_chart[world_line_chart$type == "daily_excess_deaths_cumulative" & world_line_chart$date == max(world_line_chart$date[world_line_chart$type == "daily_excess_deaths_cumulative"], na.rm = T), columns_to_export]

# Sometimes the world total for confirmed deaths is not updated by the time we update our estimates. If so, we default to the previous day (within the last 3 weeks):                 
if(nrow(na.omit(world_top_line_chart)) != 1){
  warning('Defaulting to last known world official covid-19 confirmed deaths (within the past two weeks) as up-to-date value is missing') 
  world_top_line_chart$official_covid_deaths <- na.omit(rev(world_line_chart[world_line_chart$type == "daily_excess_deaths_cumulative" & world_line_chart$date %in% c(max(world_line_chart$date, na.rm = T)-1:14), "official_covid_deaths"]))[1]
}

# Check that cumulative daily excess deaths not NA:
if(nrow(na.omit(world_top_line_chart)) != 1){
  print(world_top_line_chart)
  stop('Missing most recent cumulative global confirmed covid-19 deaths. Check interactive export script.')
}
                       
write_csv(world_top_line_chart, "output-data/output-for-interactive/world_most_recent_cumulative_deaths.csv")

# Step 6: Generate data for table A ------------------------------------------------------------------------------
cat('\n Step 6: Generate data for table A')

# We here rely on "export_long" created above.

# Absolute terms:
# Select most recent and relevant columns from combined long data:
table_A_absolute <- export_long[export_long$date == max(export_long$date) & export_long$type == "daily_excess_deaths_cumulative", c("location",
                                                                                                                                    "official_covid_deaths",
                                                                                                                                    "estimate",
                                                                                                                                    "estimate_top_95",
                                                                                                                                    "estimate_bot_95",
                                                                                                                                    "vaccinated_per_100")]

# Generate % difference between estimate and official deaths
table_A_absolute$difference_pct <- round(100*(table_A_absolute$estimate - table_A_absolute$official_covid_deaths) / table_A_absolute$official_covid_deaths, 1) 

# Select correct column order:
table_A_absolute <- table_A_absolute[, c("location",
                                         "official_covid_deaths",
                                         "estimate",
                                         "estimate_top_95",
                                         "estimate_bot_95",
                                         "difference_pct",
                                         "vaccinated_per_100")]

# Per 100k:
# Select most recent and relevant columns from combined long data:
table_A_per_100k <- export_long[export_long$date == max(export_long$date) & export_long$type == "daily_excess_deaths_per_100k_cumulative", c("location",
                                                                                                                                             "official_covid_deaths",
                                                                                                                                             "estimate",
                                                                                                                                             "estimate_top_95",
                                                                                                                                             "estimate_bot_95",
                                                                                                                                             "vaccinated_per_100")]

# Generate % difference between estimate and official deaths
table_A_per_100k$difference_pct <- round(100*(table_A_per_100k$estimate - table_A_per_100k$official_covid_deaths) / table_A_per_100k$official_covid_deaths, 1) 

# Select correct column order:
table_A_per_100k <- table_A_per_100k[, c("location",
                                         "official_covid_deaths",
                                         "estimate",
                                         "estimate_top_95",
                                         "estimate_bot_95",
                                         "difference_pct",
                                         "vaccinated_per_100")]

colnames(table_A_per_100k) <- c("location",
                                "official_covid_deaths_per_100k", 
                                "estimate_per_100k",
                                "estimate_top_95_per_100k", 
                                "estimate_bot_95_per_100k",
                                "difference_pct",
                                "vaccinated_per_100")

colnames(table_A_absolute)[2:5] <- paste0(colnames(table_A_absolute)[2:5], "_absolute")

table_A <- merge(table_A_absolute, table_A_per_100k[, 1:5], by="location", all = T)

# Tweak the location name for Europe to indicate that the grouping includes the EU:
table_A$location[table_A$location == "Europe"] <- "Europe (incl. EU)"

# Fix to extremely small countries, conservatively rounding to nearest one using ceiling/floor for the confidence interval:
table_A$estimate_top_95_absolute[abs(table_A$estimate_top_95_absolute) < 1] <- ceiling(table_A$estimate_top_95_absolute[abs(table_A$estimate_top_95_absolute) < 1])
table_A$estimate_bot_95_absolute[abs(table_A$estimate_bot_95_absolute) < 1] <- floor(table_A$estimate_bot_95_absolute[abs(table_A$estimate_bot_95_absolute) < 1])

# Write to file
write_csv(table_A, "output-data/output-for-interactive/table_A.csv")

# Step 7: Generate data for second map ------------------------------------------------------------------------------
cat('\n Step 7: Generate data for second map')

# Load data:
second_map <-
  read_csv("output-data/output-for-interactive/main_map.csv", show_col_types = F)[, c(
    "iso3c",
    "date",
    "cumulative_estimated_daily_excess_deaths_per_100k",
    "cumulative_estimated_daily_excess_deaths_ci_95_top_per_100k",
    "cumulative_estimated_daily_excess_deaths_ci_95_bot_per_100k"
  )]

# Load estimated demography-adjusted IFR by iso3c:
ifr_by_iso <- readRDS("source-data/ifr_cache.RDS")
ifr_by_iso$iso2c[ifr_by_iso$area == "Namibia"] <- "NA"
ifr_by_iso$iso3c <- countrycode(ifr_by_iso$iso2c, "iso2c", "iso3c", warn = F)
ifr_by_iso$demography_adjusted_ifr_percent <- ifr_by_iso$area_ifr

# Load estimated share of population over 65:
age_over_65 <-
  unique(country_data[, c("iso_code", "aged_65_older")])
age_over_65$iso3c <- age_over_65$iso_code
age_over_65$aged_65_older_pct <- age_over_65$aged_65_older

second_map <-
  merge(second_map, na.omit(ifr_by_iso[, c("iso3c", "demography_adjusted_ifr_percent")]), all.x = T)
second_map <-
  merge(second_map, na.omit(age_over_65[, c("iso3c", "aged_65_older_pct")]), all.x = T)

# Get implied infections per 100 persons:
second_map$implied_infections_per_100_persons <-
  (1 / 1000) * second_map$cumulative_estimated_daily_excess_deaths_per_100k / (second_map$demography_adjusted_ifr_percent /
                                                                                 100)
second_map$implied_infections_per_100_persons_top_95 <-
  (1 / 1000) * second_map$cumulative_estimated_daily_excess_deaths_ci_95_top_per_100k /
  (second_map$demography_adjusted_ifr_percent / 100)
second_map$implied_infections_per_100_persons_bot_95 <-
  (1 / 1000) * second_map$cumulative_estimated_daily_excess_deaths_ci_95_bot_per_100k /
  (second_map$demography_adjusted_ifr_percent / 100)


# Ensure infections are not negative:
second_map$implied_infections_per_100_persons[second_map$implied_infections_per_100_persons < 0] <-  0
second_map$cumulative_estimated_daily_excess_deaths_ci_95_bot_per_100k[second_map$cumulative_estimated_daily_excess_deaths_ci_95_bot_per_100k < 0] <-  0
second_map$cumulative_estimated_daily_excess_deaths_ci_95_top_per_100k[second_map$cumulative_estimated_daily_excess_deaths_ci_95_top_per_100k < 0] <-  0

# Get estimated dead over 100k population over 65:
second_map$cumulative_estimated_daily_excess_deaths_per_100k_population_over_65 <-
  second_map$cumulative_estimated_daily_excess_deaths_per_100k / (second_map$aged_65_older_pct /
                                                                    100)
second_map$cumulative_estimated_daily_excess_deaths_per_100k_population_over_65_top_95 <-
  second_map$cumulative_estimated_daily_excess_deaths_ci_95_top_per_100k / (second_map$aged_65_older_pct /
                                                                              100)
second_map$cumulative_estimated_daily_excess_deaths_per_100k_population_over_65_bot_95 <-
  second_map$cumulative_estimated_daily_excess_deaths_ci_95_bot_per_100k / (second_map$aged_65_older_pct /
                                                                              100)

# Write to file:
write_csv(second_map,
          "output-data/output-for-interactive/second_map.csv")

# Step 8: Generate data for table B ------------------------------------------------------------------------------
cat('\n Step 8: Generate data for table B')

# Load data from table 1:
table_B <- read_csv("output-data/output-for-interactive/table_A.csv", show_col_types = F)
table_B$iso3c <- countrycode(table_B$location, "country.name", "iso3c", warn = F)
table_B$iso3c[table_B$location == "Micronesia"] <- "FSM"
table_B$iso3c[table_B$location == "Kosovo"] <- "KSV"

# Load data from map 2:
second_map_data <- read_csv("output-data/output-for-interactive/second_map.csv", show_col_types = F)

# Merge the two:
table_B <- merge(table_B, second_map_data, by = "iso3c", all.x = T)  

# Generate columns: 
# Demography adjusted deaths:
table_B$cumulative_estimated_daily_excess_deaths_per_100k_demography_adjusted <- table_B$cumulative_estimated_daily_excess_deaths_per_100k / (table_B$demography_adjusted_ifr_percent/mean(table_B$demography_adjusted_ifr_percent, na.rm = T))

# Ensure infections are not negative:
table_B$implied_infections_per_100_persons[table_B$implied_infections_per_100_persons < 0] <- 0
table_B$implied_infections_per_100_persons_top_95[table_B$implied_infections_per_100_persons_top_95 < 0] <- 0
table_B$implied_infections_per_100_persons_bot_95[table_B$implied_infections_per_100_persons_bot_95 < 0] <- 0

# Remove rows with no data:
table_B <- table_B[!(is.na(table_B$cumulative_estimated_daily_excess_deaths_per_100k_population_over_65) & is.na(table_B$implied_infections_per_100_persons)), ]

# Write to file:
write_csv(table_B[, c("location",
                      "iso3c", 
                      "official_covid_deaths_per_100k",
                      "cumulative_estimated_daily_excess_deaths_per_100k", 
                      "estimate_top_95_per_100k", 
                      "estimate_bot_95_per_100k",
                      "cumulative_estimated_daily_excess_deaths_per_100k_population_over_65", 
                      "cumulative_estimated_daily_excess_deaths_per_100k_demography_adjusted",
                      "implied_infections_per_100_persons",
                      "implied_infections_per_100_persons_top_95",
                      "implied_infections_per_100_persons_bot_95")], 
          "output-data/output-for-interactive/table_B.csv")

# Extra exports:  ------------------------------------------------------------------------------
# Step 9: Histogram data ------------------------------------------------------------------------------
cat('\n Step 9: Histogram data')

# This is exported in case people want more information on the distribution of predictions at the world cumulative level for the present day.

# Load raw histogram data
hist <- read_csv("output-data/export_world_cumulative_histogram_data.csv", show_col_types = F)

# Pivot to long format
hist <- pivot_longer(hist, cols = grep("B", colnames(hist)))

# Restrict to most recent date
hist <- hist[hist$date == max(hist$date), ]

# Construct equal-spaced groups
N_groups <- 50

groups <- seq(min(hist$value), max(hist$value), len=N_groups+1)
hist$bin_min <- NA
hist$bin_max <- NA
hist$bin <- NA
hist$bin_n <- 1

hist_date <- unique(hist$date)[1]
hist$date <- NULL

# Place observations in groups
for(i in 2:length(groups)){
hist$bin_min[hist$value >= groups[i-1] & hist$value <= groups[i]] <- groups[i-1]  
hist$bin_max[hist$value >= groups[i-1] & hist$value <= groups[i]] <- groups[i]
hist$bin[hist$value >= groups[i-1] & hist$value <= groups[i]] <- i-1

if(sum(hist$bin == i-1, na.rm = T) == 0){
  hist <- rbind(hist, c(NA, NA, NA, NA, NA, 
                        groups[i-1], groups[i], i-1, 0))
}
}

hist$world <- unique(na.omit(hist$world)[1])
hist$date <- hist_date
hist$estimate <- unique(na.omit(hist$estimate)[1])

# Sum observations per bin:
hist$bin_n <- ave(hist$bin_n, hist$bin, FUN = sum)

hist <- unique(hist[, c("world", "date", "estimate", "bin_min", "bin_max", "bin_n")])

if(inspect){
ggplot(hist)+geom_rect(aes(xmin=bin_min, xmax=bin_max, ymax=bin_n, ymin=0))+
  theme_minimal()
}

# Write to file:
write_csv(hist, 
          "output-data/output-for-interactive/world_estimates_histogram.csv")


# Step 10: Implied infections over time ------------------------------------------------------------------------------
cat('\n Step 10: Implied infections over time')
# Load data:
infections <-
  read_csv("output-data/output-for-interactive/by_location.csv", show_col_types = F)
infections$iso3c <- countrycode(infections$location, "country.name", "iso3c", warn = F)
infections$iso3c <- ifelse(infections$location == 'Kosovo', 'KSV', infections$iso3c)
infections <- infections[!is.na(infections$iso3c), ]

# Load estimated demography-adjusted IFR by iso3c:
ifr_by_iso <- readRDS("source-data/ifr_cache.RDS")
ifr_by_iso$iso2c[ifr_by_iso$area == "Namibia"] <- "NA"
ifr_by_iso$iso3c <- countrycode(ifr_by_iso$iso2c, "iso2c", "iso3c", warn = F)
ifr_by_iso$demography_adjusted_ifr_percent <- ifr_by_iso$area_ifr

# Load estimated share of population over 65:
age_over_65 <-
  unique(country_data[, c("iso_code", "aged_65_older")])
age_over_65$iso3c <- age_over_65$iso_code
age_over_65$aged_65_older_pct <- age_over_65$aged_65_older

infections <-
  merge(infections, na.omit(ifr_by_iso[, c("iso3c", "demography_adjusted_ifr_percent")]), by = "iso3c", all.x = T)
infections <-
  merge(infections, na.omit(age_over_65[, c("iso3c", "aged_65_older_pct")]), by = "iso3c", all.x = T)

# Get implied infections:
infections$estimate <- infections$estimate / (infections$demography_adjusted_ifr_percent /
                                                                                 100)
infections$estimate_top_95 <- infections$estimate_top_95 /
  (infections$demography_adjusted_ifr_percent / 100)
infections$estimate_bot_95 <- infections$estimate_bot_95 /
  (infections$demography_adjusted_ifr_percent / 100)

# Ensure infections are not negative:
infections$estimate[infections$estimate < 0] <-  0
infections$estimate_top_95[infections$estimate_top_95 < 0] <-  0
infections$estimate_bot_95[infections$estimate_bot_95 < 0] <-  0

# Write to file:
write_csv(infections,
          "output-data/output-for-interactive/infections_per_day.csv")
# Generating world-wide estimate assuming IFR is 50% higher than that estimated in rich countries for the world on average:
infections$world_total <- ave(infections$estimate, infections$date, FUN = function(x) sum(x, na.rm=T))/1.5
infections$world_total_top_95 <- ave(infections$estimate_top_95, infections$date, FUN = function(x) sum(x, na.rm=T))/1.5
infections$world_total_bot_95 <- ave(infections$estimate_bot_95, infections$date, FUN = function(x) sum(x, na.rm=T))/1.5

if(inspect){
  ggplot(infections, aes(x=date, y=world_total))+geom_ribbon(aes(ymin=world_total_bot_95, ymax =world_total_top_95, xmin = date, xmax = date), fill = "lightgray")+geom_line()+theme_minimal()
}

# Load data:
infections <-
  read_csv("output-data/output-for-interactive/by_location_cumulative.csv", show_col_types = F)
infections$iso3c <- countrycode(infections$location, "country.name", "iso3c", warn = F)
infections$iso3c <- ifelse(infections$location == 'Kosovo', 'KSV', infections$iso3c)
infections <- infections[!is.na(infections$iso3c), ]

# Load estimated demography-adjusted IFR by iso3c:
ifr_by_iso <- readRDS("source-data/ifr_cache.RDS")
ifr_by_iso$iso2c[ifr_by_iso$area == "Namibia"] <- "NA"
ifr_by_iso$iso3c <- countrycode(ifr_by_iso$iso2c, "iso2c", "iso3c")
ifr_by_iso$demography_adjusted_ifr_percent <- ifr_by_iso$area_ifr

# Load estimated share of population over 65:
age_over_65 <-
  unique(country_data[, c("iso_code", "aged_65_older")])
age_over_65$iso3c <- age_over_65$iso_code
age_over_65$aged_65_older_pct <- age_over_65$aged_65_older

infections <-
  merge(infections, na.omit(ifr_by_iso[, c("iso3c", "demography_adjusted_ifr_percent")]), by = "iso3c", all.x = T)
infections <-
  merge(infections, na.omit(age_over_65[, c("iso3c", "aged_65_older_pct")]), by = "iso3c", all.x = T)

# Get implied infections:
infections$estimate <- infections$estimate / (infections$demography_adjusted_ifr_percent /
                                                100)
infections$estimate_top_95 <- infections$estimate_top_95 /
  (infections$demography_adjusted_ifr_percent / 100)
infections$estimate_bot_95 <- infections$estimate_bot_95 /
  (infections$demography_adjusted_ifr_percent / 100)

# If one wants total with plausible past exposure, set the below to true
none_infected_twice <- FALSE
if(none_infected_twice){
# Ensure infections are not negative:
infections$estimate[infections$estimate < 0] <-  0
infections$estimate_top_95[infections$estimate_top_95 < 0] <-  0
infections$estimate_bot_95[infections$estimate_bot_95 < 0] <-  0
}

# Ensure infections are not higher than population:
infections$estimate[infections$estimate < infections$population] <-  infections$population[infections$estimate < infections$population]
infections$estimate_top_95[infections$estimate_top_95 < infections$population] <-  infections$population[infections$estimate < infections$population]
infections$estimate_bot_95[infections$estimate_bot_95 < infections$population] <-  infections$population[infections$estimate < infections$population]

# Write to file:
write_csv(infections,
          "output-data/output-for-interactive/infections_per_day_cumulative.csv")
# Generating world-wide estimate assuming IFR is 50% higher than that estimated in rich countries for the world on average:
infections$world_total <- ave(infections$estimate, infections$date, FUN = function(x) sum(x, na.rm=T))/1.5
infections$world_total_top_95 <- ave(infections$estimate_top_95, infections$date, FUN = function(x) sum(x, na.rm=T))/1.5
infections$world_total_bot_95 <- ave(infections$estimate_bot_95, infections$date, FUN = function(x) sum(x, na.rm=T))/1.5

if(inspect){
ggplot(infections, aes(x=date, y=world_total))+geom_ribbon(aes(ymin=world_total_bot_95, ymax =world_total_top_95, xmin = date, xmax = date), fill = "lightgray")+geom_line()+theme_minimal()
}
# Final step: Add timestamp ------------------------------------------------------------------------------
cat('\n Final step: Add timestamp')

# Add timestamp:
tibble(timestamp = now(tzone = "UTC")) %>% 
  write_csv('output-data/output-for-interactive/timestamp.csv')


