# Step 1: import libraries ------------------------------------------------------------------------------

# Note: This script by default gives data at the country level, region level, and world level. To supply custom groupings, see "Step 5" below.

# Import libraries
library(tidyverse)
library(data.table)
library(lubridate)
library(readr)
library(countrycode)
library(ggplot2)
options(scipen=999)

# Set default for "inspect" option (if TRUE, prints plot to console for every export):
inspect = FALSE

# Step 2: Load predictions and covariates ------------------------------------------------------------------------------

# Load all model prediction + 200 bootstrap
pred_matrix <- readRDS("output-data/pred_matrix.RDS")

# Load covariates (iso3c, country name, population ++)
export_covariates <- readRDS("output-data/export_covariates.RDS")

# Drop our subnational region(s) (we include the countries in which they are part). These are always coded to have the national unit in which they appear as the first three characters ISO3C, then a descriptive, e.g. "IND_Mumbai":
pred_matrix <- pred_matrix[nchar(export_covariates$iso3c) == 3, ]
export_covariates <- export_covariates[nchar(export_covariates$iso3c) == 3, ]

# Convert model predictions from per 100k population to absolute numbers
pred_matrix <- pred_matrix*export_covariates$population / 100000

# Harmonize export dates (first of every week, except final prediction date)
export_covariates$week <- round(as.numeric(export_covariates$date)/7, 0)+1-min(round(as.numeric(export_covariates$date)/7, 0))
export_covariates$date[export_covariates$date != max(export_covariates$date)] <- ave(export_covariates$date[export_covariates$date != max(export_covariates$date)], export_covariates$week[export_covariates$date != max(export_covariates$date)],
                              FUN = function(x) min(x, na.rm = T))

# Remove possible duplicate dates:
pred_matrix <- pred_matrix[!duplicated(paste0(export_covariates$date, "_", export_covariates$iso3c)), ]
export_covariates <- export_covariates[!duplicated(paste0(export_covariates$date, "_", export_covariates$iso3c)), ]

# Check that this worked correctly:
min(table(export_covariates$date)) == max(table(export_covariates$date))

# Separate out the estimate
estimate <- as.numeric(pred_matrix[, 1])

# Harmonize country names with The Economist standard, maintaining row order
export_covariates$row_order <- 1:nrow(export_covariates)
export_covariates <- merge(export_covariates,
                           read_csv("source-data/economist_country_names.csv")[, c("Name", "ISOA3", "Regions", "Income group WB", "Economy IMF")],
                           by.x = "iso3c",
                           by.y = "ISOA3", all.x = T)
export_covariates <- export_covariates[order(export_covariates$row_order), ]
export_covariates$row_order <- NULL

export_covariates$country <- export_covariates$Name
export_covariates$country[is.na(export_covariates$country)] <- countrycode(
  export_covariates$iso3c[is.na(export_covariates$country)], "iso3c", "country.name")
export_covariates$country <- ifelse(export_covariates$iso3c == 'KSV', 'Kosovo', export_covariates$country)
export_covariates$Name <- NULL

# Define regions for main chart:
export_covariates$continent <- countrycode(export_covariates$iso3c, "iso3c", "continent")
export_covariates$continent <- ifelse(export_covariates$iso3c == 'KSV', 'Europe', export_covariates$continent)

# Step 3: Define function to construct confidence interval given grouping ------------------------------------------------------------------------------
confidence_intervals <- function(new_col_names = "estimated_daily_excess_deaths",
                                 group = "iso3c",
                                 unit = "iso3c",
                                 time = "date",
                                 population = "population",
                                 known_data_column = "daily_excess_deaths",
                                 recorded_data_column = "daily_covid_deaths",
                                 return_cumulative = F,
                                 drop_ci_if_known_data = T,
                                 covars = export_covariates,
                                 bootstrap_predictions = pred_matrix,
                                 model_prediction = estimate,
                                 include_model_prediction_in_ci = T,
                                 include_final_prediction_in_ci = T,
                                 recency_buffer = 0,
                                 return_histogram_data = F # This returns the full sorted bootstrap predictions instead of confidence intervals. 
                                 ){
  
  # Check that all observations have a group and time variable:
  if(sum(is.na(covars[, group])) > 0){
    stop("Some observations lack a grouping.")
  }
  
  # Check that no predictions are NA
  if(sum(is.na(bootstrap_predictions)) + sum(is.na(model_prediction)) != 0){
    stop("Some predictions are NA.")
  }
  
  # Check that dimensionality are the same for covars and predictions:
  if(nrow(covars) != nrow(bootstrap_predictions) | 
     nrow(covars) != length(model_prediction)){
    stop("Dimensionality mismatch: check that predictions are 1-1 mapped to covars")
  }
  
  # If most recent week has less than 3 days of observations, carry over from last week:
  if(max(covars[, time]) > Sys.Date()-recency_buffer){
    
    # Get most recent week with full data:
    most_recent <- sort(unique(covars[, time]), decreasing = T)[2]
    
    # Extract these observations:
    temp_boot <- bootstrap_predictions[covars[, time] == most_recent, ]
    temp_estimate <- estimate[covars[, time] == most_recent]
    temp_covars <- covars[covars[, time] == most_recent, ]
    
    # Replace the latest week with the last week of full data
    max_time <- max(covars[, time])
    for(i in unique(covars[, unit])){
      bootstrap_predictions[covars[, time] == max_time & covars[, unit] == i, ] <- temp_boot[temp_covars[, unit] == i, ]
      estimate[covars[, time] == max_time & covars[, unit] == i] <- temp_estimate[temp_covars[, unit] == i]
    }
    
  }
  
  # Separate out vector of recorded data
  recorded_data <- as.numeric(covars[, recorded_data_column])
  
  # If we have a known value, use this were applicable:
  raw_estimate <- estimate
  if(!missing(known_data_column)){
    known_data <- covars[, known_data_column]
    estimate[!is.na(known_data)] <- known_data[!is.na(known_data)]
  } else {
    known_data <- rep(NA, nrow(covars))
  }
  
  # If requested, do not return/use model confidence interval where data is known:
  if(drop_ci_if_known_data){
    for(i in 1:ncol(bootstrap_predictions)){
      bootstrap_predictions[!is.na(known_data), i] <- known_data[!is.na(known_data)]
    }
  }
  
  # Sum predictions by group-time if needed:
  covars$id <- paste0(covars[, group], "_", covars[, time])
  
  if(max(table(covars$id)) > 1){
    
    estimate <- ave(estimate, covars$id, FUN = function(x) sum(x))
    known_data <- ave(known_data, covars$id, FUN = function(x) sum(x, na.rm = T))
    raw_estimate <- ave(known_data, covars$id, FUN = function(x) sum(x, na.rm = T))
    
    recorded_data <- ave(recorded_data, covars$id, FUN = function(x) sum(x, na.rm = T))
    
    for(i in 1:ncol(bootstrap_predictions)){
      bootstrap_predictions[, i] <- ave(bootstrap_predictions[, i], 
                                        covars$id, FUN = function(x) sum(x))
    }
    
    # Also sum population
    covars$population <- ave(covars$population, covars$id, FUN = function(x) sum(x, na.rm = T))
  }
  
  # If requested, generate cumulative data
  if(return_cumulative){
    # Generate cumulative if requested
    bootstrap_predictions <- data.frame(bootstrap_predictions)
    
    # Add in unit and time variable
    bootstrap_predictions$row_order <- 1:nrow(bootstrap_predictions)
    bootstrap_predictions[, unit] <- covars[, unit]
    bootstrap_predictions[, time] <- covars[, time]
    
    # Add in raw estimate, known data, and recorded data
    bootstrap_predictions$raw_estimate <- raw_estimate
    bootstrap_predictions$known_data <- ifelse(is.na(known_data), 0, known_data)
    bootstrap_predictions$recorded_data <- ifelse(is.na(recorded_data), 0, recorded_data)
    
    
    # Sort by date
    bootstrap_predictions <- bootstrap_predictions[
      order(bootstrap_predictions[, time]), ]
    
    # multiplying is necessary here, as I for all but the last week only have one observation per week.
    week_mult <- rep(7, length(unique(bootstrap_predictions[, time])))
    week_mult[length(week_mult)] <- as.numeric(rev(sort(unique(bootstrap_predictions[, time])))[1]-rev(sort(unique(bootstrap_predictions[, time])))[2])
    
    for(i in setdiff(colnames(bootstrap_predictions), c("row_order", unit, time))){
      bootstrap_predictions[, i] <- ave(bootstrap_predictions[, i], bootstrap_predictions[, unit], FUN = function(x){ 
        cumsum(x*week_mult)})
    }
    
    # Return to original order, remove temporary columns, and extract raw and central estimate + known data as cumulative
    bootstrap_predictions <- bootstrap_predictions[
      order(bootstrap_predictions$row_order), ]
    
    bootstrap_predictions$iso3c <- NULL
    bootstrap_predictions$date <- NULL
    bootstrap_predictions$row_order <- NULL
    
    raw_estimate <- bootstrap_predictions$raw_estimate
    bootstrap_predictions$raw_estimate <- NULL
    
    recorded_data <- bootstrap_predictions$recorded_data
    bootstrap_predictions$recorded_data <- NULL
    
    estimate <- bootstrap_predictions[, 1]
    
    known_data <- bootstrap_predictions$known_data
    # If one wants cumulative data to be missing when a week is, uncomment below line 
    # known_data <- ifelse(is.na(known_data), NA, bootstrap_predictions$known_data)
    bootstrap_predictions$known_data <- NULL
    
    bootstrap_predictions <- as.matrix(bootstrap_predictions)
  }
  
  # Collapse groups:
  estimate <- estimate[!duplicated(covars$id)]
  raw_estimate <- raw_estimate[!duplicated(covars$id)] 
  bootstrap_predictions <- bootstrap_predictions[!duplicated(covars$id), ] 
  known_data <- known_data[!duplicated(covars$id)]
  recorded_data <- recorded_data[!duplicated(covars$id)]
  covars <- covars[!duplicated(covars$id), ]
  
  # Sort bootstrap prediction matrix:
  for(i in 1:nrow(bootstrap_predictions)){
    bootstrap_predictions[i, ] <- sort(bootstrap_predictions[i, ])
  }
  
  # If requested, return histogram data instead of standard export with CIs:
  if(return_histogram_data){
    histagram_data <- cbind(covars[, c(group, time, population)], estimate, bootstrap_predictions[, 2:ncol(bootstrap_predictions)])
    colnames(histagram_data)[1:4] <- c(colnames(covars[, c(group, time, population)]), "estimate")
    return(histagram_data)
  }
  
  # Extract 90 and 95% confidence intervals
  ci_95_top <- bootstrap_predictions[, round(ncol(bootstrap_predictions)*0.975, 0)]
  ci_90_top <- bootstrap_predictions[, round(ncol(bootstrap_predictions)*0.95, 0)]
  ci_50_top <- bootstrap_predictions[, round(ncol(bootstrap_predictions)*0.75, 0)]
  ci_50_bot <- bootstrap_predictions[, round(ncol(bootstrap_predictions)*0.25, 0)]
  ci_90_bot <- bootstrap_predictions[, round(ncol(bootstrap_predictions)*0.05, 0)]
  ci_95_bot <- bootstrap_predictions[, round(ncol(bootstrap_predictions)*0.025, 0)]
  
  # Ensure model prediction (i.e. raw estimate) within confidence interval if requested
  if(include_model_prediction_in_ci){
    ci_95_top <- ifelse(ci_95_top > raw_estimate, ci_95_top, raw_estimate)
    ci_90_top <- ifelse(ci_90_top > raw_estimate, ci_90_top, raw_estimate)
    ci_50_top <- ifelse(ci_50_top > raw_estimate, ci_50_top, raw_estimate)
    ci_50_bot <- ifelse(ci_50_bot < raw_estimate, ci_50_bot, raw_estimate)
    ci_90_bot <- ifelse(ci_90_bot < raw_estimate, ci_90_bot, raw_estimate)
    ci_95_bot <- ifelse(ci_95_bot < raw_estimate, ci_95_bot, raw_estimate)
  }
  if(include_final_prediction_in_ci){
    ci_95_top <- ifelse(ci_95_top > estimate, ci_95_top, estimate)
    ci_90_top <- ifelse(ci_90_top > estimate, ci_90_top, estimate)
    ci_50_top <- ifelse(ci_50_top > estimate, ci_50_top, estimate)
    ci_50_bot <- ifelse(ci_50_bot < estimate, ci_50_bot, estimate)
    ci_90_bot <- ifelse(ci_90_bot < estimate, ci_90_bot, estimate)
    ci_95_bot <- ifelse(ci_95_bot < estimate, ci_95_bot, estimate)
  }
  
  # Return result neatly formatted
  result <- cbind.data.frame(covars[, c(group, time, population)], 
                             estimate, 
                             ci_95_top,
                             ci_90_top,
                             ci_50_top,
                             ci_50_bot,
                             ci_90_bot,
                             ci_95_bot,
                             raw_estimate,
                             known_data,
                             recorded_data)
  colnames(result) <- c(colnames(covars[, c(group, time, population)]),
                        paste0(new_col_names),
                        paste0(new_col_names, "_ci_95_top"),
                        paste0(new_col_names, "_ci_90_top"),
                        paste0(new_col_names, "_ci_50_top"),
                        paste0(new_col_names, "_ci_50_bot"),
                        paste0(new_col_names, "_ci_90_bot"),
                        paste0(new_col_names, "_ci_95_bot"),
                        paste0(new_col_names, "_raw_estimate"),
                        "daily_excess_deaths",
                        "daily_covid_deaths")
  
  # If cumulative values are returned, note in column name:
  if(return_cumulative){
    colnames(result)[4:ncol(result)] <- paste0("cumulative_", colnames(result)[4:ncol(result)])
  }
  
  # Ensure no duplicates
  result <- unique(result)
  
  return(result)
}


gc() # Free up memory
# Step 4: Construct data frames used for graphics (both per 100k and absolute terms), per day ------------------------------------------------------------------------------

# Export 1: Country-week level, absolute units
country_export <- confidence_intervals(new_col_names = "estimated_daily_excess_deaths",
                                       group = "iso3c", 
                                       time = "date",
                                       covars = export_covariates,
                                       return_cumulative = F,
                                       drop_ci_if_known_data = F,
                                       bootstrap_predictions = pred_matrix,
                                       known_data_column = "daily_excess_deaths",
                                       model_prediction = estimate,
                                       include_model_prediction_in_ci = T)

# Inspect:
if(inspect){
  ggplot(country_export[country_export$iso3c %in% c("IND", "ZAF", "USA", "CHN", 
                                                    "IDN", "PAK", "BRA", "NGA"), ], 
         aes(x=date, 
             y=estimated_daily_excess_deaths,
             col = iso3c))+
    geom_line(aes(y=estimated_daily_excess_deaths_ci_95_top))+
    geom_line(aes(y=estimated_daily_excess_deaths_ci_90_top))+
    geom_line(aes(y=estimated_daily_excess_deaths_ci_50_top))+
    geom_line(aes(y=estimated_daily_excess_deaths_ci_50_bot))+
    geom_line(aes(y=estimated_daily_excess_deaths_ci_90_bot))+
    geom_line(aes(y=estimated_daily_excess_deaths_ci_95_bot))+
    geom_line(col="black", linetype = "dashed")+
    geom_line(aes(y=daily_excess_deaths), 
              col="black", linetype = "solid")+geom_line(aes(y=daily_covid_deaths), col = "red")+
    facet_wrap(.~iso3c)+theme_minimal()+
    theme(legend.position = "none")
}

# Write to file:
write_csv(country_export, "output-data/export_country.csv")

# Export 2: Country-week level, per 100k
country_export <- confidence_intervals(new_col_names = "estimated_daily_excess_deaths",
                                       group = "iso3c", 
                                       time = "date",
                                       covars = export_covariates,
                                       return_cumulative = F,
                                       drop_ci_if_known_data = F,
                                       bootstrap_predictions = pred_matrix,
                                       known_data_column = "daily_excess_deaths",
                                       model_prediction = estimate,
                                       include_model_prediction_in_ci = T)

per_capita_columns <- grep("deaths", colnames(country_export))

for(i in per_capita_columns){
  country_export[, i] <- 100000*country_export[, i]/country_export[, "population"]
}
colnames(country_export)[per_capita_columns] <- paste0(colnames(country_export)[per_capita_columns], "_per_100k")

# Inspect:
if(inspect){
  ggplot(country_export[country_export$iso3c %in% c("IND", "ZAF", "USA", "CHN", 
                                                    "IDN", "RUS", "BRA", "NGA", "MEX"), ], 
         aes(x=date, 
             y=estimated_daily_excess_deaths_per_100k,
             col = iso3c))+
    geom_line(aes(y=estimated_daily_excess_deaths_ci_95_top_per_100k))+
    geom_line(aes(y=estimated_daily_excess_deaths_ci_90_top_per_100k))+
    geom_line(aes(y=estimated_daily_excess_deaths_ci_90_bot_per_100k))+
    geom_line(aes(y=estimated_daily_excess_deaths_ci_95_bot_per_100k))+
    geom_line(col="black", linetype = "dashed")+
    geom_line(aes(y=daily_excess_deaths_per_100k), 
              col="black", linetype = "solid")+
    facet_wrap(.~iso3c)+theme_minimal()+
    theme(legend.position = "none")
}

# Write to file:
write_csv(country_export, "output-data/export_country_per_100k.csv")


# Export 3: Region-week level, absolute units
region_export <- confidence_intervals(new_col_names = "estimated_daily_excess_deaths",
                                      group = "continent", 
                                      time = "date",
                                      covars = export_covariates,
                                      return_cumulative = F,
                                      drop_ci_if_known_data = T,
                                      bootstrap_predictions = pred_matrix,
                                      known_data_column = "daily_excess_deaths",
                                      model_prediction = estimate,
                                      include_model_prediction_in_ci = F)

# Inspect:
if(inspect){
  ggplot(region_export, 
         aes(x=date, 
             y=estimated_daily_excess_deaths,
             col = continent))+
    geom_line(aes(y=estimated_daily_excess_deaths_ci_95_top))+
    geom_line(aes(y=estimated_daily_excess_deaths_ci_90_top))+
    geom_line(aes(y=estimated_daily_excess_deaths_ci_90_bot))+
    geom_line(aes(y=estimated_daily_excess_deaths_ci_95_bot))+
    geom_line(col="black", linetype = "dashed")+
    facet_wrap(.~continent)+theme_minimal()+
    theme(legend.position = "none")
}

# Write to file:
write_csv(region_export, "output-data/export_regions.csv")


# Define alternative set of regions:
export_covariates$continent_alt <- countrycode(export_covariates$iso3c, "iso3c",
                                               "continent")
export_covariates$continent_alt <- ifelse(export_covariates$iso3c == 'KSV', 'Europe', export_covariates$continent_alt)
export_covariates$continent_alt[export_covariates$continent_alt == "Europe"] <- "Europe, United States, Canada, and Oceania"
export_covariates$continent_alt[export_covariates$iso3c %in% c("USA", "CAN")] <- "Europe, United States, Canada, and Oceania"
export_covariates$continent_alt[export_covariates$continent_alt == "Oceania"] <- "Europe, United States, Canada, and Oceania"
export_covariates$continent_alt[export_covariates$continent_alt == "Americas"] <- "Latin America and Caribbean"

region_export <- confidence_intervals(new_col_names = "estimated_daily_excess_deaths",
                                      group = "continent_alt", 
                                      time = "date",
                                      covars = export_covariates,
                                      return_cumulative = F,
                                      drop_ci_if_known_data = T,
                                      bootstrap_predictions = pred_matrix,
                                      known_data_column = "daily_excess_deaths",
                                      model_prediction = estimate,
                                      include_model_prediction_in_ci = F)

# Inspect:
if(inspect){
  ggplot(region_export, 
         aes(x=date, 
             y=estimated_daily_excess_deaths,
             col = continent_alt))+
    geom_line(aes(y=estimated_daily_excess_deaths_ci_95_top))+
    geom_line(aes(y=estimated_daily_excess_deaths_ci_90_top))+
    geom_line(aes(y=estimated_daily_excess_deaths_ci_90_bot))+
    geom_line(aes(y=estimated_daily_excess_deaths_ci_95_bot))+
    geom_line(col="black", linetype = "dashed")+
    facet_wrap(.~continent_alt)+theme_minimal()+
    theme(legend.position = "none")
}


# Export 4: Region-week level, per 100k
region_export <- confidence_intervals(new_col_names = "estimated_daily_excess_deaths",
                                      group = "continent", 
                                      time = "date",
                                      covars = export_covariates,
                                      return_cumulative = F,
                                      drop_ci_if_known_data = T,
                                      bootstrap_predictions = pred_matrix,
                                      known_data_column = "daily_excess_deaths",
                                      model_prediction = estimate,
                                      include_model_prediction_in_ci = F)

per_capita_columns <- grep("deaths", colnames(region_export))

for(i in per_capita_columns){
  region_export[, i] <- 100000*region_export[, i]/region_export[, "population"]
}
colnames(region_export)[per_capita_columns] <- paste0(colnames(region_export)[per_capita_columns], "_per_100k")

# Inspect:
if(inspect){
  ggplot(region_export, 
         aes(x=date, 
             y=estimated_daily_excess_deaths_per_100k,
             col = continent))+
    geom_line(aes(y=estimated_daily_excess_deaths_ci_95_top_per_100k))+
    geom_line(aes(y=estimated_daily_excess_deaths_ci_90_top_per_100k))+
    geom_line(aes(y=estimated_daily_excess_deaths_ci_90_bot_per_100k))+
    geom_line(aes(y=estimated_daily_excess_deaths_ci_95_bot_per_100k))+
    geom_line(col="black", linetype = "dashed")+
    facet_wrap(.~continent)+theme_minimal()+
    theme(legend.position = "none")
}

# Write to file:
write_csv(region_export, "output-data/export_regions_per_100k.csv")


# Export 5: World level, absolute units
export_covariates$world <- "World"
world_export <- confidence_intervals(new_col_names = "estimated_daily_excess_deaths",
                                     group = "world", 
                                     time = "date",
                                     covars = export_covariates,
                                     return_cumulative = F,
                                     drop_ci_if_known_data = T,
                                     bootstrap_predictions = pred_matrix,
                                     known_data_column = "daily_excess_deaths",
                                     model_prediction = estimate,
                                     include_model_prediction_in_ci = F)

# Inspect:
if(inspect){
  ggplot(world_export, 
         aes(x=date, 
             y=estimated_daily_excess_deaths,
             col = world))+
    geom_line(aes(y=estimated_daily_excess_deaths_ci_95_top))+
    geom_line(aes(y=estimated_daily_excess_deaths_ci_90_top))+
    geom_line(aes(y=estimated_daily_excess_deaths_ci_50_bot))+
    geom_line(aes(y=estimated_daily_excess_deaths_ci_50_top))+
    geom_line(aes(y=estimated_daily_excess_deaths_ci_90_bot))+
    geom_line(aes(y=estimated_daily_excess_deaths_ci_95_bot))+
    geom_line(col="black", linetype = "dashed")+theme_minimal()+
    theme(legend.position = "none")
}

# Write to file:
write_csv(world_export, "output-data/export_world.csv")


# Export 6: World level, per 100k
world_export <- confidence_intervals(new_col_names = "estimated_daily_excess_deaths",
                                     group = "world", 
                                     time = "date",
                                     covars = export_covariates,
                                     return_cumulative = F,
                                     drop_ci_if_known_data = T,
                                     bootstrap_predictions = pred_matrix,
                                     known_data_column = "daily_excess_deaths",
                                     model_prediction = estimate,
                                     include_model_prediction_in_ci = F)

per_capita_columns <- grep("deaths", colnames(world_export))

for(i in per_capita_columns){
  world_export[, i] <- 100000*world_export[, i]/world_export[, "population"]
}
colnames(world_export)[per_capita_columns] <- paste0(colnames(world_export)[per_capita_columns], "_per_100k")

# Inspect:
if(inspect){
  ggplot(world_export, 
         aes(x=date, 
             y=estimated_daily_excess_deaths_per_100k,
             col = world))+
    geom_line(aes(y=estimated_daily_excess_deaths_ci_95_top_per_100k))+
    geom_line(aes(y=estimated_daily_excess_deaths_ci_90_top_per_100k))+
    geom_line(aes(y=estimated_daily_excess_deaths_ci_90_bot_per_100k))+
    geom_line(aes(y=estimated_daily_excess_deaths_ci_95_bot_per_100k))+
    geom_line(col="black", linetype = "dashed")+theme_minimal()+
    theme(legend.position = "none")
}

# Write to file:
write_csv(world_export, "output-data/export_world_per_100k.csv")


gc() # Free up memory
# Step 4: Construct data frames used for graphics (both per 100k and absolute terms), cumulative ------------------------------------------------------------------------------

# Export 1: Country-week level, absolute units
country_export <- confidence_intervals(new_col_names = "estimated_daily_excess_deaths",
                                       group = "iso3c", 
                                       time = "date",
                                       covars = export_covariates,
                                       return_cumulative = T,
                                       drop_ci_if_known_data = T,
                                       bootstrap_predictions = pred_matrix,
                                       known_data_column = "daily_excess_deaths",
                                       model_prediction = estimate,
                                       include_model_prediction_in_ci = F)

# Inspect:
if(inspect){
  ggplot(country_export[country_export$iso3c %in% c("IND", "CHN"), ], 
         aes(x=date, 
             y=cumulative_estimated_daily_excess_deaths,
             col = iso3c))+
    geom_line(aes(y=cumulative_estimated_daily_excess_deaths_ci_95_top))+
    geom_line(aes(y=cumulative_estimated_daily_excess_deaths_ci_90_top))+
    geom_line(aes(y=cumulative_estimated_daily_excess_deaths_ci_50_top))+
    geom_line(aes(y=cumulative_estimated_daily_excess_deaths_ci_50_bot))+
    geom_line(aes(y=cumulative_estimated_daily_excess_deaths_ci_90_bot))+
    geom_line(aes(y=cumulative_estimated_daily_excess_deaths_ci_95_bot))+
    geom_line(col="black", linetype = "dashed")+
    geom_line(aes(y=cumulative_daily_excess_deaths), 
              col="black", linetype = "solid")+
    geom_line(aes(y=cumulative_daily_covid_deaths), col = "red")+
    facet_wrap(.~iso3c)+theme_minimal()+
    theme(legend.position = "none")
}

# Write to file:
write_csv(country_export, "output-data/export_country_cumulative.csv")

# Export 2: Country-week level, per 100k
country_export <- confidence_intervals(new_col_names = "estimated_daily_excess_deaths",
                                       group = "iso3c", 
                                       time = "date",
                                       covars = export_covariates,
                                       return_cumulative = T,
                                       drop_ci_if_known_data = T,
                                       bootstrap_predictions = pred_matrix,
                                       known_data_column = "daily_excess_deaths",
                                       model_prediction = estimate,
                                       include_model_prediction_in_ci = F)

per_capita_columns <- grep("deaths", colnames(country_export))

for(i in per_capita_columns){
  country_export[, i] <- 100000*country_export[, i]/country_export[, "population"]
}
colnames(country_export)[per_capita_columns] <- paste0(colnames(country_export)[per_capita_columns], "_per_100k")

# Inspect:
if(inspect){
  ggplot(country_export[country_export$iso3c %in% c("IND", "USA", "MEX", "PER",
                                                    "RUS", "ZAF"), ], 
         aes(x=date, 
             y=cumulative_estimated_daily_excess_deaths_per_100k,
             col = iso3c))+
    geom_line(aes(y=cumulative_estimated_daily_excess_deaths_ci_95_top_per_100k))+
    geom_line(aes(y=cumulative_estimated_daily_excess_deaths_ci_90_top_per_100k))+
    geom_line(aes(y=cumulative_estimated_daily_excess_deaths_ci_90_bot_per_100k))+
    geom_line(aes(y=cumulative_estimated_daily_excess_deaths_ci_95_bot_per_100k))+
    geom_line(col="black", linetype = "dashed")+
    geom_line(aes(y=cumulative_daily_excess_deaths_per_100k), 
              col="black", linetype = "solid")+
    facet_wrap(.~iso3c)+theme_minimal()+
    theme(legend.position = "none")
}

# Write to file:
write_csv(country_export, "output-data/export_country_per_100k_cumulative.csv")


# Export 3: Region-week level, absolute units
region_export <- confidence_intervals(new_col_names = "estimated_daily_excess_deaths",
                                      group = "continent", 
                                      time = "date",
                                      covars = export_covariates,
                                      return_cumulative = T,
                                      drop_ci_if_known_data = T,
                                      bootstrap_predictions = pred_matrix,
                                      known_data_column = "daily_excess_deaths",
                                      model_prediction = estimate,
                                      include_model_prediction_in_ci = F)

# Inspect:
if(inspect){
  ggplot(region_export, 
         aes(x=date, 
             y=cumulative_estimated_daily_excess_deaths,
             col = continent))+
    geom_line(aes(y=cumulative_estimated_daily_excess_deaths_ci_95_top))+
    geom_line(aes(y=cumulative_estimated_daily_excess_deaths_ci_90_top))+
    geom_line(aes(y=cumulative_estimated_daily_excess_deaths_ci_50_top))+
    geom_line(aes(y=cumulative_estimated_daily_excess_deaths_ci_50_bot))+
    geom_line(aes(y=cumulative_estimated_daily_excess_deaths_ci_90_bot))+
    geom_line(aes(y=cumulative_estimated_daily_excess_deaths_ci_95_bot))+
    geom_line(col="black", linetype = "dashed")+geom_line(aes(y=cumulative_daily_covid_deaths), col = "red")+
    facet_wrap(.~continent)+theme_minimal()+
    theme(legend.position = "none")
}

# Write to file:
write_csv(region_export, "output-data/export_regions_cumulative.csv")


# Export 4: Region-week level, per 100k
region_export <- confidence_intervals(new_col_names = "estimated_daily_excess_deaths",
                                      group = "continent", 
                                      time = "date",
                                      covars = export_covariates,
                                      return_cumulative = T,
                                      drop_ci_if_known_data = T,
                                      bootstrap_predictions = pred_matrix,
                                      known_data_column = "daily_excess_deaths",
                                      model_prediction = estimate,
                                      include_model_prediction_in_ci = F)

per_capita_columns <- grep("deaths", colnames(region_export))

for(i in per_capita_columns){
  region_export[, i] <- 100000*region_export[, i]/region_export[, "population"]
}
colnames(region_export)[per_capita_columns] <- paste0(colnames(region_export)[per_capita_columns], "_per_100k")

# Inspect:
if(inspect){
  ggplot(region_export, 
         aes(x=date, 
             y=cumulative_estimated_daily_excess_deaths_per_100k,
             col = continent))+
    geom_line(aes(y=cumulative_estimated_daily_excess_deaths_ci_95_top_per_100k))+
    geom_line(aes(y=cumulative_estimated_daily_excess_deaths_ci_90_top_per_100k))+
    geom_line(aes(y=cumulative_estimated_daily_excess_deaths_ci_90_bot_per_100k))+
    geom_line(aes(y=cumulative_estimated_daily_excess_deaths_ci_95_bot_per_100k))+
    geom_line(col="black", linetype = "dashed")+
    facet_wrap(.~continent)+theme_minimal()+
    theme(legend.position = "none")
}

# Write to file:
write_csv(region_export, "output-data/export_regions_per_100k_cumulative.csv")


# Export 5: World level, absolute units
export_covariates$world <- "World"
world_export <- confidence_intervals(new_col_names = "estimated_daily_excess_deaths",
                                     group = "world", 
                                     time = "date",
                                     covars = export_covariates,
                                     return_cumulative = T,
                                     drop_ci_if_known_data = T,
                                     bootstrap_predictions = pred_matrix,
                                     known_data_column = "daily_excess_deaths",
                                     model_prediction = estimate,
                                     include_model_prediction_in_ci = F)

# Inspect:
  ggplot(world_export, 
         aes(x=date, 
             y=cumulative_estimated_daily_excess_deaths,
             col = world))+
    geom_line(aes(y=cumulative_estimated_daily_excess_deaths_ci_95_top))+
#    geom_line(aes(y=cumulative_estimated_daily_excess_deaths_ci_90_top))+
#    geom_line(aes(y=cumulative_estimated_daily_excess_deaths_ci_90_bot))+
    geom_line(aes(y=cumulative_estimated_daily_excess_deaths_ci_95_bot))+
    geom_line(aes(y=cumulative_daily_covid_deaths), col = "blue")+
    geom_ribbon(aes(ymin=cumulative_estimated_daily_excess_deaths_ci_95_top,
                    ymax=cumulative_estimated_daily_excess_deaths_ci_95_bot), 
                fill = 'darkred', alpha=0.3)+
    geom_line(col="black", linetype = "dashed")+theme_minimal()+
    theme(legend.position = "none")+xlab("Estimated excess deaths (red), confirmed covid-19 deaths (blue)")+ylab("Total deaths, World")
  ggsave('global_mortality.png', width = 8, height = 5)

# Write to file:
write_csv(world_export, "output-data/export_world_cumulative.csv")

# Export equivalent histagram data: 
export_covariates$world <- "World"
world_export <- confidence_intervals(new_col_names = "estimated_daily_excess_deaths",
                                     group = "world", 
                                     time = "date",
                                     covars = export_covariates,
                                     return_cumulative = T,
                                     drop_ci_if_known_data = T,
                                     bootstrap_predictions = pred_matrix,
                                     known_data_column = "daily_excess_deaths",
                                     model_prediction = estimate,
                                     include_model_prediction_in_ci = F,
                                     return_histogram_data = T)

# Write to file:
write_csv(world_export, "output-data/export_world_cumulative_histogram_data.csv")


# Export 6: World level, per 100k
world_export <- confidence_intervals(new_col_names = "estimated_daily_excess_deaths",
                                     group = "world", 
                                     time = "date",
                                     covars = export_covariates,
                                     return_cumulative = T,
                                     drop_ci_if_known_data = T,
                                     bootstrap_predictions = pred_matrix,
                                     known_data_column = "daily_excess_deaths",
                                     model_prediction = estimate,
                                     include_model_prediction_in_ci = F)

per_capita_columns <- grep("deaths", colnames(world_export))

for(i in per_capita_columns){
  world_export[, i] <- 100000*world_export[, i]/world_export[, "population"]
}
colnames(world_export)[per_capita_columns] <- paste0(colnames(world_export)[per_capita_columns], "_per_100k")

# Inspect:
if(inspect){
  ggplot(world_export, 
         aes(x=date, 
             y=cumulative_estimated_daily_excess_deaths_per_100k,
             col = world))+
    geom_line(aes(y=cumulative_estimated_daily_excess_deaths_ci_95_top_per_100k))+
    geom_line(aes(y=cumulative_estimated_daily_excess_deaths_ci_90_top_per_100k))+
    geom_line(aes(y=cumulative_estimated_daily_excess_deaths_ci_90_bot_per_100k))+
    geom_line(aes(y=cumulative_estimated_daily_excess_deaths_ci_95_bot_per_100k))+
    geom_line(col="black", linetype = "dashed")+theme_minimal()+
    theme(legend.position = "none")
}

# Write to file:
write_csv(world_export, "output-data/export_world_per_100k_cumulative.csv")


# Export 7: World level, absolute units, with alternative excess deaths metric (excess where known, otherwise covid deaths) (on request)
if(FALSE){
export_covariates$world <- "World"
world_export <- confidence_intervals(new_col_names = "estimated_daily_excess_deaths",
                                     group = "world", 
                                     time = "date",
                                     covars = export_covariates,
                                     return_cumulative = T,
                                     drop_ci_if_known_data = T,
                                     bootstrap_predictions = pred_matrix,
                                     known_data_column = "daily_excess_deaths",
                                     model_prediction = estimate,
                                     include_model_prediction_in_ci = F)


export_covariates_alt <- export_covariates
export_covariates_alt$daily_excess_deaths_alt <- export_covariates$daily_excess_deaths
export_covariates_alt$daily_excess_deaths_alt[is.na(export_covariates_alt$daily_excess_deaths_alt)] <- export_covariates_alt$daily_covid_deaths[is.na(export_covariates_alt$daily_excess_deaths_alt)]

world_export_alt <- confidence_intervals(new_col_names = "estimated_daily_excess_deaths",
                                         group = "world", 
                                         time = "date",
                                         covars = export_covariates_alt,
                                         return_cumulative = T,
                                         drop_ci_if_known_data = T,
                                         bootstrap_predictions = pred_matrix,
                                         known_data_column = "daily_excess_deaths",
                                         recorded_data_column = "daily_excess_deaths_alt",
                                         model_prediction = estimate,
                                         include_model_prediction_in_ci = F)

world_export$cumulative_daily_excess_deaths_alternative <- world_export_alt$cumulative_daily_covid_deaths

# Inspect:
if(inspect){
  ggplot(world_export, 
         aes(x=date, 
             y=cumulative_estimated_daily_excess_deaths,
             col = world))+
    geom_line(aes(y=cumulative_estimated_daily_excess_deaths_ci_95_top))+
    geom_line(aes(y=cumulative_estimated_daily_excess_deaths_ci_90_top))+
    geom_line(aes(y=cumulative_estimated_daily_excess_deaths_ci_90_bot))+
    geom_line(aes(y=cumulative_estimated_daily_excess_deaths_ci_95_bot))+
    geom_line(aes(y=cumulative_daily_excess_deaths_alternative, col = "red"))+
    geom_line(aes(y=cumulative_daily_covid_deaths), col = "black")+
    geom_line(col="black", linetype = "dashed")+theme_minimal()+
    theme(legend.position = "none")
}
}
# Write to file if desired:
# write_csv(world_export, "output-data/export_world_cumulative_with_alternative_excess_deaths.csv")

gc() # Free up memory
# Step 5: Construct custom exports used as inputs for interactive export scripts: ------------------------------------------------------------------------------

# Define alternative set of regions:
export_covariates$continent_alt <- countrycode(export_covariates$iso3c, "iso3c",
                                               "continent")
export_covariates$continent_alt <- ifelse(export_covariates$iso3c == 'KSV', 'Europe', export_covariates$continent_alt)
export_covariates$continent_alt[export_covariates$continent_alt == "Europe"] <- "Europe, United States, Canada, and Oceania"
export_covariates$continent_alt[export_covariates$iso3c %in% c("USA", "CAN")] <- "Europe, United States, Canada, and Oceania"
export_covariates$continent_alt[export_covariates$continent_alt == "Oceania"] <- "Europe, United States, Canada, and Oceania"
export_covariates$continent_alt[export_covariates$continent_alt == "Americas"] <- "Latin America and Caribbean"

region_export <- confidence_intervals(new_col_names = "estimated_daily_excess_deaths",
                                      group = "continent_alt", 
                                      time = "date",
                                      covars = export_covariates,
                                      return_cumulative = F,
                                      drop_ci_if_known_data = T,
                                      bootstrap_predictions = pred_matrix,
                                      known_data_column = "daily_excess_deaths",
                                      model_prediction = estimate,
                                      include_model_prediction_in_ci = F)

# Inspect:
if(inspect){
  ggplot(region_export, 
         aes(x=date, 
             y=estimated_daily_excess_deaths,
             col = continent_alt))+
    geom_line(aes(y=estimated_daily_excess_deaths_ci_95_top))+
    geom_line(aes(y=estimated_daily_excess_deaths_ci_90_top))+
    geom_line(aes(y=estimated_daily_excess_deaths_ci_90_bot))+
    geom_line(aes(y=estimated_daily_excess_deaths_ci_95_bot))+
    geom_line(col="black", linetype = "dashed")+
    facet_wrap(.~continent_alt)+theme_minimal()+
    theme(legend.position = "none")
}

# Write to file:
write_csv(region_export, "output-data/output-by-alternative-regions/export_regions_EU_NA_Oceania_collapsed.csv")

### Custom groupings for interactive (EU, Lat. Am, and North America and Oceania without being combined with Asia):
export_covariates$custom_regions <- countrycode(export_covariates$iso3c, "iso3c", "continent")
export_covariates$custom_regions <- ifelse(export_covariates$iso3c == 'KSV', 'Europe', export_covariates$custom_regions)
export_covariates$custom_regions[export_covariates$iso3c %in% c("USA", "CAN")] <- "North America"
export_covariates$custom_regions[export_covariates$custom_regions == "Americas"] <- "Latin America and Caribbean"
export_covariates$custom_regions[export_covariates$iso3c %in% c("AUT", "BEL", "BGR", "HRV", "CYP", "CZE", "DNK", "EST", "FIN", "FRA", "DEU", "GRC", "HUN", "IRL", "ITA", "LVA", "LTU", "LUX", "MLT", "NLD", "POL", "PRT", "ROU", "SVK", "SVN", "ESP", "SWE")] <- "European Union"
export_covariates$custom_regions[export_covariates$custom_regions == "Europe"] <- "Europe (not EU)"

# Absolute, per day
export <- confidence_intervals(new_col_names = "estimated_daily_excess_deaths",
                               group = "custom_regions", 
                               time = "date",
                               covars = export_covariates,
                               return_cumulative = F,
                               drop_ci_if_known_data = T,
                               bootstrap_predictions = pred_matrix,
                               known_data_column = "daily_excess_deaths",
                               model_prediction = estimate,
                               include_model_prediction_in_ci = F)
export <- export[export$custom_regions %in% c("Latin America and Caribbean",
                                              "North America", 
                                              "European Union", "Asia", "Oceania"), ]

# Inspect:
if(inspect){
  ggplot(export[, ], 
         aes(x=date, 
             y=estimated_daily_excess_deaths,
             col = custom_regions))+
    geom_line(aes(y=estimated_daily_excess_deaths_ci_95_top))+
    geom_line(aes(y=estimated_daily_excess_deaths_ci_90_top))+
    geom_line(aes(y=estimated_daily_excess_deaths_ci_90_bot))+
    geom_line(aes(y=estimated_daily_excess_deaths_ci_95_bot))+
    geom_line(col="black", linetype = "dashed")+
    facet_wrap(.~custom_regions)+theme_minimal()+
    theme(legend.position = "none")
}

# write to file:
write_csv(export, "output-data/output-by-alternative-regions/export_regions_lat_am_na_eu.csv")

per_capita_columns <- grep("deaths", colnames(export))

for(i in per_capita_columns){
  export[, i] <- 100000*export[, i]/export[, "population"]
}
colnames(export)[per_capita_columns] <- paste0(colnames(export)[per_capita_columns], "_per_100k")

if(inspect){
  ggplot(export[, ], 
         aes(x=date, 
             y=estimated_daily_excess_deaths_per_100k,
             col = custom_regions))+
    geom_line(aes(y=estimated_daily_excess_deaths_ci_95_top_per_100k))+
    geom_line(aes(y=estimated_daily_excess_deaths_ci_90_top_per_100k))+
    geom_line(aes(y=estimated_daily_excess_deaths_ci_90_bot_per_100k))+
    geom_line(aes(y=estimated_daily_excess_deaths_ci_95_bot_per_100k))+
    geom_line(col="black", linetype = "dashed")+
    facet_wrap(.~custom_regions)+theme_minimal()+
    theme(legend.position = "none")
}

# write to file:
write_csv(export, "output-data/output-by-alternative-regions/export_regions_lat_am_na_eu_per_100k.csv")

# Absolute, cumulative
export <- confidence_intervals(new_col_names = "estimated_daily_excess_deaths",
                               group = "custom_regions", 
                               time = "date",
                               covars = export_covariates,
                               return_cumulative = T,
                               drop_ci_if_known_data = T,
                               bootstrap_predictions = pred_matrix,
                               known_data_column = "daily_excess_deaths",
                               model_prediction = estimate,
                               include_model_prediction_in_ci = F)
export <- export[export$custom_regions %in% c("Latin America and Caribbean",
                                              "North America", 
                                              "European Union", "Asia", "Oceania"), ]

# Inspect:
if(inspect){
  ggplot(export[, ], 
         aes(x=date, 
             y=cumulative_estimated_daily_excess_deaths,
             col = custom_regions))+
    geom_line(aes(y=cumulative_estimated_daily_excess_deaths_ci_95_top))+
    geom_line(aes(y=cumulative_estimated_daily_excess_deaths_ci_90_top))+
    geom_line(aes(y=cumulative_estimated_daily_excess_deaths_ci_90_bot))+
    geom_line(aes(y=cumulative_estimated_daily_excess_deaths_ci_95_bot))+
    geom_line(col="black", linetype = "dashed")+
    facet_wrap(.~custom_regions)+theme_minimal()+
    theme(legend.position = "none")
}

# write to file:
write_csv(export, "output-data/output-by-alternative-regions/export_regions_lat_am_na_eu_cumulative.csv")

per_capita_columns <- grep("deaths", colnames(export))

for(i in per_capita_columns){
  export[, i] <- 100000*export[, i]/export[, "population"]
}
colnames(export)[per_capita_columns] <- paste0(colnames(export)[per_capita_columns], "_per_100k")

if(inspect){
  ggplot(export[, ], 
         aes(x=date, 
             y=cumulative_estimated_daily_excess_deaths_per_100k,
             col = custom_regions))+
    geom_line(aes(y=cumulative_estimated_daily_excess_deaths_ci_95_top_per_100k))+
    geom_line(aes(y=cumulative_estimated_daily_excess_deaths_ci_90_top_per_100k))+
    geom_line(aes(y=cumulative_estimated_daily_excess_deaths_ci_90_bot_per_100k))+
    geom_line(aes(y=cumulative_estimated_daily_excess_deaths_ci_95_bot_per_100k))+
    geom_line(col="black", linetype = "dashed")+
    facet_wrap(.~custom_regions)+theme_minimal()+
    theme(legend.position = "none")
}

# write to file:
write_csv(export, "output-data/output-by-alternative-regions/export_regions_lat_am_na_eu_per_100k_cumulative.csv")


gc() # Free up memory
# Step 6: Construct custom data frames based on alternative groupings or subsets of the data ------------------------------------------------------------------------------
# This script supports export of any grouping. 

# To get statistics for a given group, do the following
# Define your own groups:
# Here, we use the "countrycode" package to place countries (defined by iso3c) into groups based on the continents they below to. 
export_covariates$continent_alt <- countrycode(export_covariates$iso3c, "iso3c",
                                               "continent")
export_covariates$continent_alt <- ifelse(export_covariates$iso3c == 'KSV', 'Europe', export_covariates$continent_alt)

# Then manually specify the United States and Canada as a separate group
export_covariates$continent_alt[export_covariates$iso3c %in% c("USA", "CAN")] <- "US and Canada"

# And rename the rest of the Americas to "Latin America and Caribbean"
export_covariates$continent_alt[export_covariates$continent_alt == "Americas"] <- "Latin America and Caribbean"

# We then use our export function to export summary statistics for these groups:
region_export <- confidence_intervals(new_col_names = "estimated_daily_excess_deaths",
                                      group = "continent_alt", 
                                      time = "date",
                                      covars = export_covariates,
                                      return_cumulative = F,
                                      drop_ci_if_known_data = T,
                                      bootstrap_predictions = pred_matrix,
                                      known_data_column = "daily_excess_deaths",
                                      model_prediction = estimate,
                                      include_model_prediction_in_ci = F)

# Inspect:
if(inspect){
  ggplot(region_export, 
         aes(x=date, 
             y=estimated_daily_excess_deaths,
             col = continent_alt))+
    geom_line(aes(y=estimated_daily_excess_deaths_ci_95_top))+
    geom_line(aes(y=estimated_daily_excess_deaths_ci_90_top))+
    geom_line(aes(y=estimated_daily_excess_deaths_ci_90_bot))+
    geom_line(aes(y=estimated_daily_excess_deaths_ci_95_bot))+
    geom_line(col="black", linetype = "dashed")+
    facet_wrap(.~continent_alt)+theme_minimal()+
    theme(legend.position = "none")
}

# Write to file if desired
# write_csv(region_export, "output-data/export_alternative_regions.csv")

# At the request of CNN, I added the corresponding per 100k population, and cumulative numbers in absolute and per 100k population

# Same for per 100k
per_capita_columns <- grep("deaths", colnames(region_export))

for(i in per_capita_columns){
  region_export[, i] <- 100000*region_export[, i]/region_export[, "population"]
}
colnames(region_export)[per_capita_columns] <- paste0(colnames(region_export)[per_capita_columns], "_per_100k")

if(inspect){
  ggplot(region_export, 
         aes(x=date, 
             y=estimated_daily_excess_deaths_per_100k,
             col = continent_alt))+
    geom_line(aes(y=estimated_daily_excess_deaths_ci_95_top_per_100k))+
    geom_line(aes(y=estimated_daily_excess_deaths_ci_90_top_per_100k))+
    geom_line(aes(y=estimated_daily_excess_deaths_ci_90_bot_per_100k))+
    geom_line(aes(y=estimated_daily_excess_deaths_ci_95_bot_per_100k))+
    geom_line(col="black", linetype = "dashed")+
    facet_wrap(.~continent_alt)+theme_minimal()+
    theme(legend.position = "none")
}

# Write to file if desired
# write_csv(region_export, "output-data/export_alternative_regions_per_100k.csv")

region_export <- confidence_intervals(new_col_names = "estimated_daily_excess_deaths",
                                      group = "continent_alt", 
                                      time = "date",
                                      covars = export_covariates,
                                      return_cumulative = T,
                                      drop_ci_if_known_data = T,
                                      bootstrap_predictions = pred_matrix,
                                      known_data_column = "daily_excess_deaths",
                                      model_prediction = estimate,
                                      include_model_prediction_in_ci = F)

# Inspect the results:
if(inspect){
  ggplot(region_export, 
         aes(x=date, 
             y=cumulative_estimated_daily_excess_deaths,
             col = continent_alt))+
    geom_line(aes(y=cumulative_estimated_daily_excess_deaths_ci_95_top))+
    geom_line(aes(y=cumulative_estimated_daily_excess_deaths_ci_90_top))+
    geom_line(aes(y=cumulative_estimated_daily_excess_deaths_ci_90_bot))+
    geom_line(aes(y=cumulative_estimated_daily_excess_deaths_ci_95_bot))+
    geom_line(col="black", linetype = "dashed")+
    facet_wrap(.~continent_alt)+theme_minimal()+
    theme(legend.position = "none")
}

# Write to file if desired
# write_csv(region_export, "output-data/export_alternative_regions_cumulative.csv")

# Same for per 100k
per_capita_columns <- grep("deaths", colnames(region_export))

for(i in per_capita_columns){
  region_export[, i] <- 100000*region_export[, i]/region_export[, "population"]
}
colnames(region_export)[per_capita_columns] <- paste0(colnames(region_export)[per_capita_columns], "_per_100k")

if(inspect){
  ggplot(region_export, 
         aes(x=date, 
             y=cumulative_estimated_daily_excess_deaths_per_100k,
             col = continent_alt))+
    geom_line(aes(y=cumulative_estimated_daily_excess_deaths_ci_95_top_per_100k))+
    geom_line(aes(y=cumulative_estimated_daily_excess_deaths_ci_90_top_per_100k))+
    geom_line(aes(y=cumulative_estimated_daily_excess_deaths_ci_90_bot_per_100k))+
    geom_line(aes(y=cumulative_estimated_daily_excess_deaths_ci_95_bot_per_100k))+
    geom_line(col="black", linetype = "dashed")+
    facet_wrap(.~continent_alt)+theme_minimal()+
    theme(legend.position = "none")
}

# Write to file if desired
# write_csv(region_export, "output-data/export_alternative_regions_per_100k_cumulative.csv")


### Replication: Egypt spotlight chart for methodology:
# Export 1: Country-week level, absolute units
country_export <- confidence_intervals(new_col_names = "estimated_daily_excess_deaths",
                                       group = "iso3c", 
                                       time = "date",
                                       covars = export_covariates,
                                       return_cumulative = F,
                                       drop_ci_if_known_data = F,
                                       bootstrap_predictions = pred_matrix,
                                       known_data_column = "daily_excess_deaths",
                                       model_prediction = estimate,
                                       include_model_prediction_in_ci = T)

# Write to file if desired
# write_csv(country_export[country_export$iso3c %in% c("EGY"), ], "output-data/Egypt_Example_plot.csv")

# Inspect:
if(inspect){
  ggplot(country_export[country_export$iso3c %in% c("EGY"), ], 
         aes(x=date, 
             y=estimated_daily_excess_deaths,
             col = iso3c))+  geom_line(col="black", linetype = "dashed")+
    geom_line(aes(y=daily_excess_deaths), 
              col="black", linetype = "solid")+
    geom_line(aes(y=estimated_daily_excess_deaths_ci_90_top))+
    geom_line(aes(y=estimated_daily_excess_deaths_ci_90_bot))+
    geom_line(aes(y=estimated_daily_excess_deaths_raw_estimate))+
    facet_wrap(.~iso3c)+theme_minimal()+
    theme(legend.position = "none")
}


### OECD statistic (central estimate excess deaths vs official covid deaths):
# Export 1: Country-week level, absolute units
country_export <- confidence_intervals(new_col_names = "estimated_daily_excess_deaths",
                                       group = "iso3c", 
                                       time = "date",
                                       covars = export_covariates,
                                       return_cumulative = T,
                                       drop_ci_if_known_data = F,
                                       bootstrap_predictions = pred_matrix,
                                       known_data_column = "daily_excess_deaths",
                                       model_prediction = estimate,
                                       include_model_prediction_in_ci = T)

oecd <- country_export[country_export$iso3c %in% c("AUT","AUS","BEL","CAN","CHL","COL","CZE","DNK","EST","FIN","FRA","DEU","GRC","HUN","ISL","IRL","ISR","ITA","JPN","KOR","LVA","LTU","LUX","MEX", "NLD", "NZL","NOR","POL","PRT","SVK","SVN","ESP","SWE","CHE","TUR","GBR","USA") & country_export$date == max(country_export$date), ]
sum(oecd$cumulative_estimated_daily_excess_deaths)/sum(oecd$cumulative_daily_covid_deaths)


### Sub-Saharan Africa statistic (central estimate excess deaths vs official covid deaths):
export_covariates$region <- countrycode(export_covariates$iso3c, "iso3c", "region")
export_covariates$region <- ifelse(export_covariates$iso3c == 'KSV', 'Europe', export_covariates$region)
export_covariates$region <- ifelse(export_covariates$iso3c == "SHN", 'Sub-Saharan Africa', export_covariates$region) 
export_covariates$region <- ifelse(is.na(export_covariates$region), 'Other', export_covariates$region) 
ssa_region_export <- confidence_intervals(new_col_names = "estimated_daily_excess_deaths",
                                          group = "region",
                                          time = "date",
                                          covars = export_covariates,
                                          return_cumulative = T,
                                          drop_ci_if_known_data = T,
                                          bootstrap_predictions = pred_matrix,
                                          known_data_column = "daily_excess_deaths",
                                          model_prediction = estimate,
                                          include_model_prediction_in_ci = F)
ssa_region_export <- ssa_region_export[ssa_region_export$region == "Sub-Saharan Africa"
                                       & ssa_region_export$date == max(ssa_region_export$date), ]
sum(ssa_region_export$cumulative_estimated_daily_excess_deaths)/sum(ssa_region_export$cumulative_daily_covid_deaths)


### Results by income groups (added at the request of the World Bank):

income_groups <- read_csv("source-data/world_bank_income_groups.csv")
income_groups <- income_groups[income_groups$GroupCode%in% c("LIC", "LMC", "UMC", "HIC"), c("CountryCode", "GroupName")]
colnames(income_groups) <- c("iso3c", "World_Bank_income_group")

export_covariates$`Income group WB` <- NULL # To minimize confusion, and ensure we are using latest data
export_covariates$row_order <- 1:nrow(export_covariates)
wb_export_covariates <- merge(export_covariates, income_groups, by = "iso3c", all.x = T)
wb_export_covariates$World_Bank_income_group[is.na(wb_export_covariates$World_Bank_income_group)] <- "Unknown income group"
wb_export_covariates <- wb_export_covariates[order(wb_export_covariates$row_order), ]

# Absolute, per day
wb_export <- confidence_intervals(new_col_names = "estimated_daily_excess_deaths",
                                  group = "World_Bank_income_group", 
                                  time = "date",
                                  covars = wb_export_covariates,
                                  return_cumulative = F,
                                  drop_ci_if_known_data = T,
                                  bootstrap_predictions = pred_matrix,
                                  known_data_column = "daily_excess_deaths",
                                  model_prediction = estimate,
                                  include_model_prediction_in_ci = F)

# Inspect:
if(inspect){
  ggplot(wb_export[wb_export$World_Bank_income_group != "Unknown income group", ], 
         aes(x=date, 
             y=estimated_daily_excess_deaths,
             col = World_Bank_income_group))+
    geom_line(aes(y=estimated_daily_excess_deaths_ci_95_top))+
    geom_line(aes(y=estimated_daily_excess_deaths_ci_90_top))+
    geom_line(aes(y=estimated_daily_excess_deaths_ci_90_bot))+
    geom_line(aes(y=estimated_daily_excess_deaths_ci_95_bot))+
    geom_line(col="black", linetype = "dashed")+
    facet_wrap(.~World_Bank_income_group)+theme_minimal()+
    theme(legend.position = "none")
}

# write to file:
write_csv(wb_export, "output-data/output-by-world-bank-income-group/wb_income_groups.csv")


per_capita_columns <- grep("deaths", colnames(wb_export))

for(i in per_capita_columns){
  wb_export[, i] <- 100000*wb_export[, i]/wb_export[, "population"]
}
colnames(wb_export)[per_capita_columns] <- paste0(colnames(wb_export)[per_capita_columns], "_per_100k")

if(inspect){
  ggplot(wb_export[wb_export$World_Bank_income_group != "Unknown income group", ], 
         aes(x=date, 
             y=estimated_daily_excess_deaths_per_100k,
             col = World_Bank_income_group))+
    geom_line(aes(y=estimated_daily_excess_deaths_ci_95_top_per_100k))+
    geom_line(aes(y=estimated_daily_excess_deaths_ci_90_top_per_100k))+
    geom_line(aes(y=estimated_daily_excess_deaths_ci_90_bot_per_100k))+
    geom_line(aes(y=estimated_daily_excess_deaths_ci_95_bot_per_100k))+
    geom_line(col="black", linetype = "dashed")+
    facet_wrap(.~World_Bank_income_group)+theme_minimal()+
    theme(legend.position = "none")
}

# write to file:
write_csv(wb_export, "output-data/output-by-world-bank-income-group/wb_income_groups_per_100k.csv")


# Absolute, cumulative
wb_export <- confidence_intervals(new_col_names = "estimated_daily_excess_deaths",
                                  group = "World_Bank_income_group", 
                                  time = "date",
                                  covars = wb_export_covariates,
                                  return_cumulative = T,
                                  drop_ci_if_known_data = T,
                                  bootstrap_predictions = pred_matrix,
                                  known_data_column = "daily_excess_deaths",
                                  model_prediction = estimate,
                                  include_model_prediction_in_ci = F)

# Inspect:
if(inspect){
  ggplot(wb_export[wb_export$World_Bank_income_group != "Unknown income group", ], 
         aes(x=date, 
             y=cumulative_estimated_daily_excess_deaths,
             col = World_Bank_income_group))+
    geom_line(aes(y=cumulative_estimated_daily_excess_deaths_ci_95_top))+
    geom_line(aes(y=cumulative_estimated_daily_excess_deaths_ci_90_top))+
    geom_line(aes(y=cumulative_estimated_daily_excess_deaths_ci_90_bot))+
    geom_line(aes(y=cumulative_estimated_daily_excess_deaths_ci_95_bot))+
    geom_line(col="black", linetype = "dashed")+
    facet_wrap(.~World_Bank_income_group)+theme_minimal()+
    theme(legend.position = "none")
}

# write to file:
write_csv(wb_export, "output-data/output-by-world-bank-income-group/wb_income_groups_cumulative.csv")

per_capita_columns <- grep("deaths", colnames(wb_export))

for(i in per_capita_columns){
  wb_export[, i] <- 100000*wb_export[, i]/wb_export[, "population"]
}
colnames(wb_export)[per_capita_columns] <- paste0(colnames(wb_export)[per_capita_columns], "_per_100k")

if(inspect){
  ggplot(wb_export[wb_export$World_Bank_income_group != "Unknown income group", ], 
         aes(x=date, 
             y=cumulative_estimated_daily_excess_deaths_per_100k,
             col = World_Bank_income_group))+
    geom_line(aes(y=cumulative_estimated_daily_excess_deaths_ci_95_top_per_100k))+
    geom_line(aes(y=cumulative_estimated_daily_excess_deaths_ci_90_top_per_100k))+
    geom_line(aes(y=cumulative_estimated_daily_excess_deaths_ci_90_bot_per_100k))+
    geom_line(aes(y=cumulative_estimated_daily_excess_deaths_ci_95_bot_per_100k))+
    geom_line(col="black", linetype = "dashed")+
    facet_wrap(.~World_Bank_income_group)+theme_minimal()+
    theme(legend.position = "none")
}

# write to file:
write_csv(wb_export, "output-data/output-by-world-bank-income-group/wb_income_groups_per_100k_cumulative.csv")

rm(wb_export_covariates)

# Western Europe, North America, Australia, and New Zealand
export_covariates$continent_alt <- "Other"
export_covariates$continent_alt[export_covariates$iso3c %in% c("AUS", "AUT", "BEL", "BGR", "CAN", "CHE", "CHI", "CYP", "CZE", "DEU", "DNK", "ESP", "EST", "FIN", "FRA", "FRO", "GBR", "GIB", "GRC", "GRL", "HRV", "HUN", "IMN", "IRL", "ISL", "ITA", "LIE", "LTU", "LUX", "LVA", "MCO", "MEX", "MLT", "NLD", "NOR", "NZL", "POL", "PRT", "ROU", "SHN", "SMR", "SPM", "SVK", "SVN", "SWE", "USA", "VAT")] <- "W. Europe, NA, and Aus/NZ"

# We then use our export function to export summary statistics for these groups:
region_export <- confidence_intervals(new_col_names = "estimated_daily_excess_deaths",
                                      group = "continent_alt", 
                                      time = "date",
                                      covars = export_covariates,
                                      return_cumulative = F,
                                      drop_ci_if_known_data = T,
                                      bootstrap_predictions = pred_matrix,
                                      known_data_column = "daily_excess_deaths",
                                      model_prediction = estimate,
                                      include_model_prediction_in_ci = F)

# Inspect:
if(inspect){
  ggplot(region_export[region_export$continent_alt != "Other", ], 
         aes(x=date, 
             y=estimated_daily_excess_deaths,
             col = continent_alt))+
    geom_line(aes(y=estimated_daily_excess_deaths_ci_95_top))+
    geom_line(aes(y=estimated_daily_excess_deaths_ci_90_top))+
    geom_line(aes(y=estimated_daily_excess_deaths_ci_90_bot))+
    geom_line(aes(y=estimated_daily_excess_deaths_ci_95_bot))+
    geom_line(col="black", linetype = "dashed")+
    facet_wrap(.~continent_alt)+theme_minimal()+
    theme(legend.position = "none")
}

write_csv(region_export[region_export$continent_alt != "Other", ], "output-data/output-by-alternative-regions/export_regions_w_europe_aus_nz_north_america.csv")

# Western Europe
# Here, we use the "countrycode" package to place countries (defined by iso3c) into groups based on the continents they below to. 
export_covariates$continent_alt <- "Other"
export_covariates$continent_alt[export_covariates$iso3c %in% c( "AUT", "BEL", "BGR", "CHE", "CHI", "CYP", "CZE", "DEU", "DNK", "ESP", "EST", "FIN", "FRA", "FRO", "GBR", "GIB", "GRC", "GRL", "HRV", "HUN", "IMN", "IRL", "ISL", "ITA", "LIE", "LTU", "LUX", "LVA", "MCO", "MLT", "NLD", "NOR",  "POL", "PRT", "ROU", "SHN", "SMR", "SPM", "SVK", "SVN", "SWE",  "VAT")] <- "W. Europe"

# We then use our export function to export summary statistics for these groups:
region_export <- confidence_intervals(new_col_names = "estimated_daily_excess_deaths",
                                      group = "continent_alt", 
                                      time = "date",
                                      covars = export_covariates,
                                      return_cumulative = F,
                                      drop_ci_if_known_data = T,
                                      bootstrap_predictions = pred_matrix,
                                      known_data_column = "daily_excess_deaths",
                                      model_prediction = estimate,
                                      include_model_prediction_in_ci = F)

# Inspect:
if(inspect){
  ggplot(region_export[region_export$continent_alt != "Other", ], 
         aes(x=date, 
             y=estimated_daily_excess_deaths,
             col = continent_alt))+
    geom_line(aes(y=estimated_daily_excess_deaths_ci_95_top))+
    geom_line(aes(y=estimated_daily_excess_deaths_ci_90_top))+
    geom_line(aes(y=estimated_daily_excess_deaths_ci_90_bot))+
    geom_line(aes(y=estimated_daily_excess_deaths_ci_95_bot))+
    geom_line(col="black", linetype = "dashed")+
    facet_wrap(.~continent_alt)+theme_minimal()+
    theme(legend.position = "none")
}

# Write to file if desired
write_csv(region_export[region_export$continent_alt != "Other", ], "output-data/output-by-alternative-regions/export_regions_western_europe.csv")

### Short aux function to export for a custom region:
custom_region_export <- function(
  region, 
  name = "un_subregion", 
  data = export_covariates,
  folder_prefix = "output-data/output-by-alternative-regions/export_regions_"){
  
  data[, "region"] <- as.character(data[, region])
  data$region[is.na(data$region)] <- "Other"

  # Generate per day
  region_export <- confidence_intervals(new_col_names = "estimated_daily_excess_deaths",
                                        group = "region", 
                                        time = "date",
                                        covars = data,
                                        return_cumulative = F,
                                        drop_ci_if_known_data = T,
                                        bootstrap_predictions = pred_matrix,
                                        known_data_column = "daily_excess_deaths",
                                        model_prediction = estimate,
                                        include_model_prediction_in_ci = F)

  # Save per day
  write_csv(region_export, paste0(folder_prefix, name, ".csv"))
  
  # Generate per day per 100k
  per_capita_columns <- grep("deaths", colnames(region_export))
  
  for(i in per_capita_columns){
    region_export[, i] <- 100000*region_export[, i]/region_export[, "population"]
  }
  colnames(region_export)[per_capita_columns] <- paste0(colnames(region_export)[per_capita_columns], "_per_100k")
  
  # Save per day per 100k
  write_csv(region_export, paste0(folder_prefix, name, "_per_100k.csv"))
  
  # Generate cumulative
  region_export <- confidence_intervals(new_col_names = "estimated_daily_excess_deaths",
                                        group = "region", 
                                        time = "date",
                                        covars = data,
                                        return_cumulative = T,
                                        drop_ci_if_known_data = T,
                                        bootstrap_predictions = pred_matrix,
                                        known_data_column = "daily_excess_deaths",
                                        model_prediction = estimate,
                                        include_model_prediction_in_ci = F)
  # Save cumulative per day
  write_csv(region_export, paste0(folder_prefix, name, "_cumulative.csv"))
  
  # Generate absolute per day per 100k
  per_capita_columns <- grep("deaths", colnames(region_export))
  
  for(i in per_capita_columns){
    region_export[, i] <- 100000*region_export[, i]/region_export[, "population"]
  }
  colnames(region_export)[per_capita_columns] <- paste0(colnames(region_export)[per_capita_columns], "_per_100k")
  
  # Save per day per 100k
  write_csv(region_export, paste0(folder_prefix, name, "_per_100k_cumulative.csv"))
}

# UN subregion
export_covariates$un_subregion <- countrycode(export_covariates$iso3c, "iso3c", "un.regionsub.name")
custom_region_export(region = "un_subregion",
                     name = "un_subregion")

# World bank regions
export_covariates$wb_region <- countrycode(export_covariates$iso3c, "iso3c", "region")
custom_region_export(region = "wb_region",
                     name = "wb_region")

# WHO regions
who_groupings <- read_csv('source-data/who_groupings.csv')
who_groupings$iso3c <- who_groupings$iso3

export_covariates$WHO_region <- NA
for(i in 1:nrow(who_groupings)){
  export_covariates$WHO_region[export_covariates$iso3c == who_groupings$iso3c[i]] <- who_groupings$WHO_region[i]
}

custom_region_export(region = "WHO_region",
                     name = "WHO_region")

if(inspect){
  folder_prefix = "output-data/output-by-alternative-regions/export_regions_"
  for(name in c("un_subregion", "wb_region", 'WHO_region')){
    pdat <- read_csv(paste0(folder_prefix, name, ".csv"))
    ggplot(pdat, aes(x=date, y=estimated_daily_excess_deaths))+geom_line()+geom_line(aes(y=estimated_daily_excess_deaths_ci_95_top))+geom_line(aes(y=estimated_daily_excess_deaths_ci_95_bot))+facet_wrap(.~region)
    pdat <- read_csv(paste0(folder_prefix, name, "_per_100k.csv"))
    ggplot(pdat, aes(x=date, y=estimated_daily_excess_deaths_per_100k))+geom_line(aes(y=estimated_daily_excess_deaths_ci_95_top_per_100k))+geom_line(aes(y=estimated_daily_excess_deaths_ci_95_bot_per_100k))+facet_wrap(.~region)+geom_line()
    pdat <- read_csv(paste0(folder_prefix, name, "_cumulative.csv"))
    ggplot(pdat, aes(x=date, y=cumulative_estimated_daily_excess_deaths))+geom_line(aes(y=cumulative_estimated_daily_excess_deaths_ci_95_top))+geom_line(aes(y=cumulative_estimated_daily_excess_deaths_ci_95_bot))+facet_wrap(.~region)+geom_line()
    pdat <- read_csv(paste0(folder_prefix, name, "_per_100k_cumulative.csv"))
    ggplot(pdat, aes(x=date, y=cumulative_estimated_daily_excess_deaths_per_100k))+geom_line(aes(y=cumulative_estimated_daily_excess_deaths_ci_95_top_per_100k))+geom_line(aes(y=cumulative_estimated_daily_excess_deaths_ci_95_bot_per_100k))+facet_wrap(.~region)+geom_line()
  }
}


gc() # Free up memory

