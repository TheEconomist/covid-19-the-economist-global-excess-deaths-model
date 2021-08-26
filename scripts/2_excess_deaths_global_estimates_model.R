# Step 1: import libraries ---------------------------------------

# Import libraries
library(tidyverse)
library(data.table)
library(lubridate)
library(countrycode)
library(agtboost)
options(scipen=999)

set.seed(99010)

# Step 2: import excess deaths data frame with covariates ---------------------------------------
df <-  readRDS("output-data/country_daily_excess_deaths_with_covariates.Rds") %>%
  data.frame()
#df <- pred_frame <- data.frame(readRDS("output-data/country_daily_excess_deaths_with_covariates.RDS")) # <- to use pre-generated RDS

#remove countries with almost no information (more than 50% missing)
countries_to_remove <- df %>%
  group_by(iso3c) %>%
  summarise(
    across(
      !c(date, population, country),
      ~ sum(is.na(.x)) > 0
    )
  ) %>%
  mutate(
    missing_cols = rowMeans(across(where(is.logical)))
  ) %>% ungroup() %>%
  select(iso3c, missing_cols) %>%
  filter(missing_cols >= 0.55) %>%
  pull(iso3c)
countrycode::countrycode(countries_to_remove,
                         origin = "iso3c", destination = "country.name")
#All micro states, disputed territories or overseas territories +  Puerto Rico which
#is missing covid data other than vaccinations
pred_frame <- df <- df %>%
  filter(!(iso3c %in% countries_to_remove))


# Select DV
dv <- "daily_excess_deaths_per_100k"

# Step 3: define predictors ---------------------------------------

# Select features to exclude (as we don't have these for countries where we don't have excess deaths)
exclude <- c("daily_total_deaths",
             "daily_total_deaths_per_100k",
             "daily_expected_deaths",
             "daily_expected_deaths_per_100k",
             "daily_excess_deaths",
             "iso3c",
             "country",
             "is_subregion")

# Define predictors
predictors <- setdiff(colnames(df), c(dv, exclude))

for(i in predictors){
  if(class(df[[i]])[1] == "character"){ #won't work if a vector is logical
    df <- df %>%
      mutate(value = 1, !! i := if_else(
        !! rlang::sym(i) %in% c("", NA),
        "Unknown",
        !! rlang::sym(i)
      )) %>%
      pivot_wider(names_from = .data[[i]], values_from = value, values_fill = 0,
                  names_prefix = i)
  } else if(class(df[[i]])[1] == "logical"){
    df <- df %>%
      mutate(
        !! i := if_else(
          !! rlang::sym(i),
          1,
          0
        )
      )
  }
}
predictors <- setdiff(colnames(df), c(dv, exclude))

# Convert all columns to numeric
df <- mutate(df,
             across(
               any_of(predictors),
               as.numeric
             )
             )

# Step 4: impute missing data (using min-impute coupled with one-hot encoding of NA locations) ---------------------------------------

#group variables
#should have no missing data
noMissingDesign <- c("iso3c", "date", "weekday",  "country", "is_subregion")

timeInvariant <- c(
  "population",
  "median_age_region_average",
  "median_age_sub_region_average", "median_age_econ_region_average",
  "wdi_life_expectancy_at_birth_region_average", "wdi_life_expectancy_at_birth_sub_region_average",
  "wdi_life_expectancy_at_birth_econ_region_average",
  "median_age_dist_average", "median_age_contiguous_country_average",
  "wdi_life_expectancy_at_birth_dist_average", "wdi_life_expectancy_at_birth_contiguous_country_average",
  "demography_adjusted_ifr_dist_average", "demography_adjusted_ifr_contiguous_country_average",
  "regionLatin America & Caribbean", "regionSouth Asia",
  "regionSub-Saharan Africa", "regionEurope & Central Asia",
  "regionMiddle East & North Africa", "regionEast Asia & Pacific",
  "regionNorth America", "subregionLatin America and the Caribbean",
  "subregionSouthern Asia", "subregionSub-Saharan Africa",
  "subregionSouthern Europe", "subregionWestern Asia",
  "subregionAustralia and New Zealand", "subregionWestern Europe",
  "subregionEastern Europe", "subregionNorthern America",
  "subregionSouth-eastern Asia", "subregionEastern Asia",
  "subregionPolynesia", "subregionNorthern Europe",
  "subregionNorthern Africa", "subregionMelanesia",
  "subregionMicronesia", "subregionCentral Asia", 
  "wb_income_groupHigh income",
  "wb_income_groupLow income", "wb_income_groupLower middle income",
  "wb_income_groupUpper middle income",
  "imf_economyUnknown", "imf_economyEmerging",
  "imf_economyAdvanced", "continentAmericas",
  "continentAsia", "continentAfrica",
  "continentEurope", "continentOceania",
  "island", "lat_largest_city", "lng_largest_city", "lat_capital", 
  "lng_capital", "largest_city_pop_pct",
  "vdem_freedom_of_expression_score", "vdem_liberal_democracy_score",
  "boix_democracy_yes_no", "boix_democracy_duration_years", 
  "freedom_house_civil_liberties", "freedom_house_political_rights",
  "freedom_house_freedom_score", "polity_democracy_score",
  "wdi_prop_less_2_usd_day", "wdi_gdppc_nominal", "wdi_gdppc_ppp",
  "wdi_urban_population_pct", "wdi_urban_pop_1m_cities_pct", "wdi_gini_index",
  "wdi_life_expectancy_at_birth", "wdi_pop_over_65", "wdi_pop_under_15",
  "demography_adjusted_ifr", "tourist_arrivals_in_thousands_2019","population_density", "median_age",
  "aged_65_older", "aged_70_older", "life_expectancy", "hospital_beds_per_thousand",
  "demography_adjusted_ifr_region_average",
  "demography_adjusted_ifr_sub_region_average", "demography_adjusted_ifr_econ_region_average"
)

timeVariant <- c(
  "sero_nat_or_reg_delta_region_average", "sero_nat_or_reg_delta_sub_region_average", 
  "sero_nat_or_reg_delta_econ_region_average", "sero_nat_delta_region_average",
  "sero_nat_delta_sub_region_average", "sero_nat_delta_econ_region_average",
  "daily_excess_deaths_per_100k_region_average", 
  "daily_excess_deaths_per_100k_sub_region_average", 
  "daily_excess_deaths_per_100k_econ_region_average", 
  "daily_tests_per_100k_region_average",
  "daily_tests_per_100k_sub_region_average", 
  "daily_tests_per_100k_econ_region_average",
  "daily_covid_cases_per_100k_region_average", 
  "daily_covid_cases_per_100k_sub_region_average",
  "daily_covid_cases_per_100k_econ_region_average", 
  "daily_covid_deaths_per_100k_region_average",
  "daily_covid_deaths_per_100k_sub_region_average",
  "daily_covid_deaths_per_100k_econ_region_average",
  "daily_positive_rate_region_average", "daily_positive_rate_sub_region_average",
  "daily_positive_rate_econ_region_average",
  "sero_nat_or_reg_delta_dist_average", "sero_nat_or_reg_delta_contiguous_country_average",
  "sero_nat_delta_dist_average", "sero_nat_delta_contiguous_country_average",
  "daily_excess_deaths_per_100k_dist_average", "daily_excess_deaths_per_100k_contiguous_country_average",
  "daily_tests_per_100k_dist_average", "daily_tests_per_100k_contiguous_country_average",
  "daily_covid_cases_per_100k_dist_average", "daily_covid_cases_per_100k_contiguous_country_average",
  "daily_covid_deaths_per_100k_dist_average", "daily_covid_deaths_per_100k_contiguous_country_average",
  "daily_positive_rate_dist_average", "daily_positive_rate_contiguous_country_average",
  "daily_covid_deaths_per_100k", "daily_covid_deaths", "daily_covid_cases_per_100k",
  "daily_covid_cases", "daily_tests_per_100k", "daily_positive_rate",
  "daily_vaccinations", "daily_vaccinations_per_100k", "vaccinated_pct", 
  "fully_vaccinated_pct", "cumulative_daily_tests_per_100k", 
  "daily_tests",
  "cumulative_daily_covid_cases_per_100k", 
  "cumulative_daily_covid_deaths_per_100k", 
  "cumulative_daily_vaccinations_per_100k",
  "mobility_retail_rec_pct_of_baseline", 
  "mobility_grocery_and_pharma_pct_of_baseline",
  "mobility_parks_pct_of_baseline", "mobility_transit_rec_pct_of_baseline",
  "mobility_workplaces_rec_pct_of_baseline",
  "oxcgrt_schools_closed", "oxcgrt_workplaces_closed", 
  "oxcgrt_cancel_public_events", "oxcgrt_gathering_restrictions", 
  "oxcgrt_public_transport_closed", "oxcgrt_stay_at_home_required", 
  "oxcgrt_internal_movement_restrictions", 
  "oxcgrt_international_movement_restrictions", "oxcgrt_face_masks_required",
  "sero_nat_or_reg_delta", "sero_nat_delta"
)


#should match the predictors
setdiff(c(noMissingDesign, timeInvariant, timeVariant), c(predictors, exclude))
setdiff(predictors, c(noMissingDesign, timeInvariant, timeVariant))
#should have no overlaps
all(c(
  identical(setdiff(noMissingDesign, c(timeInvariant, timeVariant)), noMissingDesign),
  identical(setdiff(timeInvariant, c(noMissingDesign, timeVariant)), timeInvariant),
  identical(setdiff(timeVariant, c(timeInvariant, noMissingDesign)), timeVariant)
  ))
#correct

#first check what is actually missing
missingCounts <- df %>% 
  summarise(
    across(
      everything(),
      ~ sum(is.na(.x))
    )
  )%>% 
  select(
    where(function(x){x>0})
  )

#check that none that shouldn't be missing are not missing
missingCounts %>% 
  select(
    any_of(noMissingDesign)
  ) %>% 
  names(
  )
#correct

##check the time invariant variables:
missingCounts %>% 
  select(
    any_of(timeInvariant)
  ) %>% 
  names(
  )
#We don't expected averages to be missing, though continuous averages for island we would expect to be missing
df %>% 
  select(
    iso3c,
    island,
    any_of(timeInvariant)
  ) %>%
  select(
    iso3c,
    island,
    ends_with("contiguous_country_average")
  ) %>%
  filter(
    if_any(
      ends_with("contiguous_country_average"),
      is.na
    )
  ) %>%
  select(
    iso3c, island
  ) %>%
  unique() %>%
  table()
#This just leaves south Korea whose only neighbour (north Korea) has missing info, though since border crossings
#are rare we'll treat it as an island
#we will leave missing continuous_averages as NA
#updating the variables to impute to just those with missing data
timeInvariant_missing <- missingCounts %>% 
  select(
    any_of(timeInvariant)
  ) %>% #remove contiguous averages
  select(!ends_with("contiguous_country_average")) %>%
  pivot_longer(everything()) %>%  #sort into order of how many values are missing
  arrange(-value) %>%
  pull(name)

##Checking time variant variables:
missingCounts %>% 
  select(
    any_of(timeVariant)
  ) %>% 
  names(
  )
#averages being NA is to be expected if all countries included are NA on that day
#updating the variables to impute to just those with missing data
timeVariant_missing <- missingCounts %>% 
  select(
    any_of(timeVariant)
  ) %>% 
  names()

##Missing imputation:

#2: Perform single imputation with weighted means, weights will be calculated
#based upon the other time invariant variables
#function to perfom it
impute_missing_mean <- function(data, missing_vars, skip = c("iso3c")){
  
  #progress bar
  counter <- 1
  cat("\nImputing weighted mean values:\n\n")
  pb <- txtProgressBar(min=0, max=nrow(data) * length(missing_vars), style=3)
  
  rows <- 1:nrow(data)
  for(var in missing_vars){
    aux_vars <- setdiff(names(data), c(var, skip))
    for(row in rows){
      if(is.na(data[[var]][row])){
        aux_df <- data %>% select(all_of(aux_vars)) %>%
          scale() %>% as.data.frame()
        centralRow <- aux_df[row,]
        #calculate weights
        weights <- 1/( #inverse of the distances
          sweep( #subtract this countries data from every country
          as.matrix(aux_df),
          2,
          as.matrix(centralRow)
          )^2 %>% #square then sum then square root to get euclidean distance
          rowSums(na.rm = T) %>%
          sqrt()
        )
        #replace with weighted mean
        data[[var]][row] <- weighted.mean(data[[var]][-row], weights[-row],
                                          na.rm = T)
      }
      counter = counter + 1
      setTxtProgressBar(pb, value=counter)
    }
  }
  cat("\n")
  return(data)
}
X <- df %>%
  select(!all_of(timeInvariant_missing)) %>%
  left_join(
    df %>% #reduce to one entry per country
      select(iso3c, all_of(timeInvariant)) %>%
      unique() %>% #impute
      impute_missing_mean(timeInvariant_missing) %>%
      select(iso3c, all_of(timeInvariant_missing))
  )

#3:Linearly interpolate the time-varying variables for the countries that have
#some data
better_approx <- function(x, y, xout, rule){
  #a wrapper for approx to catch when there are no values at all
  if(sum(!is.na(y)) < 2){
    #if no values (or only one) just return them to be imputed else where
    return(
      y
    )
  } else{
    #before interpolating if a leading value is NA we set it to 0
    if(is.na(y[1])){
      y[1] <- 0
      #this means that our leading values are linear interpolated from 0 to whatever the first non NA value is
    }
    approx(x, y, xout, rule = rule)$y
    #by having rule = 2 any trailing NA get set to the last value
  }
}
X <- X %>%
  group_by(iso3c) %>%
  arrange(iso3c, date) %>% #interpolate
  mutate(
    across(
      all_of(timeVariant_missing),
      ~better_approx(date,.x, date, rule = 2)
    )
  ) %>%
  ungroup()

#4:Impute remaining time variant variables
remainingVars <- 
  X %>% summarise(
    across(
      everything(),
      ~ sum(is.na(.x))
    )
  )%>%
  select(all_of(timeVariant)) %>%
  select(!ends_with("contiguous_country_average")) %>%
  select(
    where(function(x){x>0})
  ) %>% names()
#deal with contiguous averages later
impute_missing_mean_timeVariant <- function(data, missing_vars, timeInvariant_vars, skip = c("iso3c")){
  #differs from the other function in that calculate the weights for the other countries then
  #give the country the weighted average of all existing values for the variables
  aux_df <- data %>% select(iso3c, all_of(timeInvariant_vars)) %>%
    unique()
  countrys <- aux_df %>% pull(iso3c)
  #scale df
  aux_df <- scale(aux_df %>% select(!iso3c)) %>% as.data.frame()
  aux_df$iso3c <- countrys
  
  #progress bar
  counter <- 1
  cat("\nImputing weighted mean values:\n\n")
  pb <- txtProgressBar(min=0, max=length(countrys), style=3)
  
  for(country in countrys){
    #determine which variables are missing
    missing_vars_spec <- data %>% 
      filter(iso3c == country) %>% 
      select(all_of(missing_vars)) %>%
      summarise(
        across(
          everything(),
          ~any(is.na(.x))
        )
      ) %>%
      select(
        where(function(x){x})
      ) %>% names()
    if(length(missing_vars_spec) > 0){
      country_df <- data %>%
        filter(iso3c == country)
      #calculate weight for country
      weights <- 1/( #inverse of the distances
        sweep( #subtract this countries data from every country
          as.matrix(aux_df %>% select(!iso3c)),
          2,
          as.matrix(aux_df %>% filter(iso3c == country) %>% select(!iso3c))
        )^2 %>% #square then sum then square root to get euclidean distance
          rowSums(na.rm = T) %>%
          sqrt()
      )
      weights[is.infinite(weights)] <- 0
      for(var in missing_vars_spec){
        #calculate weighted mean for the variables
        country_df <- country_df %>%
          select(!all_of(var)) %>%
          left_join(
            data %>% select(iso3c, date, all_of(var)) %>% pivot_wider(names_from = iso3c,
                                                    values_from = var) %>%
              rowwise() %>%
              transmute(
                date = date,
                !! var := weighted.mean(c_across(!date), weights, na.rm = TRUE)
              ),
            by = "date"
          )
      }
      #merge back into data
      data <- data %>%
        filter(iso3c != country) %>%
        rbind(
          country_df
        )
    }
    counter = counter + 1
    setTxtProgressBar(pb, value=counter)
  }
  cat("\n")
  return(data)
}
X <- X %>%
  select(!all_of(remainingVars)) %>%
  left_join(
    X %>%
      select(iso3c, date, all_of(unique(
        c(remainingVars, timeInvariant, timeVariant)
        ))) %>% #impute
      impute_missing_mean_timeVariant(remainingVars, timeInvariant) %>%
      select(iso3c, date, all_of(remainingVars))
  )

#5. same again for the contiguous averages but we leave out the islands and south Korea
remainingVars <- 
  X %>% summarise(
    across(
      everything(),
      ~ sum(is.na(.x))
    )
  )%>%
  select(all_of(timeVariant)) %>%
  select(
    where(function(x){x>0})
  ) %>% names()
X <- X %>% filter(island == 1 | iso3c == "KOR") %>%
  rbind(
    X %>% filter(island ==  0, iso3c != "KOR") %>%
      select(!all_of(remainingVars)) %>%
      left_join(
        X %>% filter(island ==  0, iso3c != "KOR") %>%
          select(iso3c, date, all_of(unique(
            c(remainingVars, timeInvariant, timeVariant)
          ))) %>% #impute
          impute_missing_mean_timeVariant(remainingVars, timeInvariant) %>%
          select(iso3c, date, all_of(remainingVars))
      )
  )

#6 Deal with continguous averages, set to 0 
X <- X %>%
  mutate(across(
    ends_with("contiguous_country_average"),
    ~if_else(is.na(.x), 0,
             .x)
  )
  )

predictors <- setdiff(names(X), c(exclude,dv))

#add regions back in (we use this for plotting later) and remove uneeded variables
X <- X %>%
  left_join(
    pred_frame %>% select(iso3c, region) %>%
      unique()
  )

# Step 5: collapse to weekly data to reduce noise and speed up calculations, save export covariates for future merge with model predictions  ---------------------------------------
X <- X %>%
  mutate(
    `week` = round(date/7, 0)
  ) %>%
  group_by(week, iso3c, region) %>%
  summarise(
    across(
      everything(),
      ~mean(.x, na.rm = T)
    )
  ) %>%
  ungroup() %>%
  arrange(iso3c, week) %>%
  select(!week)
#drop week day since that is meaningless now
X <- select(X, !weekday)

#get predictors
predictors <- setdiff(colnames(X), c(dv, exclude,
                                     "region" #this is only here for plotting
))

#Get weekly average for outcome since not every weekly correctly aligns with the week calculated
Y <- X %>%
  pull(dv)

#remove Y from X
X <- X %>%
  select(!all_of(dv))

#remove uneeded data etc
remove(missingCounts, countries_to_remove, i, noMissingDesign, remainingVars,
       timeInvariant, timeInvariant_missing, timeVariant, timeVariant_missing,
       better_approx, impute_missing_mean, impute_missing_mean_timeVariant)

# Save covariates (We won't actually use the covariates other than regions etc, so we'll attach Y and calculate the weekly average for reported covid deaths):
export <- cbind(X %>% select(iso3c, date), Y) %>%
  rename(!! dv := Y) %>% #attach the time invariant region data
  left_join(
    pred_frame %>% 
      select(
        c(iso3c, country, region, subregion, population)
      ) %>%
      unique()
  ) %>% #attach the weekly mean for the number of reported covid deaths
  left_join(
    pred_frame %>% 
      mutate(
        week = round(as.numeric(date)/7, 0)
      ) %>%
      group_by(iso3c, week) %>%
      summarise(
        daily_covid_deaths_per_100k = mean(daily_covid_deaths_per_100k, na.rm = T),
        daily_covid_deaths = mean(daily_covid_deaths, na.rm = T),
        daily_excess_deaths = mean(daily_excess_deaths, na.rm = T),
        date = mean(as.numeric(date))
      ) %>%
      ungroup() %>%
      select(!week)
  )
saveRDS(export, "output-data/export_covariates.RDS")

# Step 6: construct calibration plot ---------------------------------------

# Select data
X_cv <- X[!is.na(Y),]
Y_cv <- Y[!is.na(Y)]

# Specifiy basis for folds and weights
weights <- log(X_cv$population)
iso3c <- X_cv$iso3c

# Define folds
cv_folds <- function(x, n = 10){
  # Randomize order
  x <- sample(x)
  
  # Divide into equalish groups:
  split(x, cut(seq_along(x), n, labels = FALSE))} 


# We load a common set of folds here, to keep results comparable with other algorithms we test. Folds are stratified by country, i.e. no country is in the training and test set simultaneously
folds <- readRDS("output-data/folds.RDS")

#generate folds
#folds <- cv_folds(unique(iso3c), n = 10) 
#saveRDS(folds, "output-data/folds.RDS")

# Make container for results
results <- data.frame(target = Y_cv, 
                      preds = rep(NA, length(Y_cv)),
                      weights = weights,
                      iso3c = iso3c)

# This loops through the folds, using data not in fold to build a model, and then model to predict on fold data
vars <- list()
for(i in 1:length(folds)){
  
  # Define training data
  train_x <- X_cv[!iso3c %in% folds[[i]], ]
  train_y <- Y_cv[!iso3c %in% folds[[i]]]
  train_w <- weights[!iso3c %in% folds[[i]]]
  
  # Define testing data
  test_x <- X_cv[iso3c %in% folds[[i]], ]
  test_y <- Y_cv[iso3c %in% folds[[i]]]
  test_w <- weights[iso3c %in% folds[[i]]]
  
  # Fit agtboost - this function fits a gradient booster
  gbt_fit <- gbt.train(train_y,
                       as.matrix(train_x[, predictors]), 
                       learning_rate = 0.01,
                       nrounds = 1500,
                       verbose = 10,
                       algorithm = "vanilla",
                       weights = train_w/mean(train_w))
  
  print(i)
  print("cross-validation round completed.")
  
  # Predict on fold:
  results$preds[results$iso3c %in% folds[[i]]] <- predict(gbt_fit, 
                                                          newdata = as.matrix(test_x[, predictors]))
}

# Weighted mean-squared error:
mean((abs(results$target - results$preds)^2)*results$weights/mean(results$weights))

# Step 7: inspect predictions ---------------------------------------

# This creates a plotting data frame:
pdat <- cbind.data.frame(pred = results$preds, 
                         truth = results$target,
                         country = X_cv$iso3c,
                         region = X_cv$region,
                         w = results$weights)

#save so we can regenerate this plot else where
write_csv(pdat, "output-data/calibration_plot_gradient_booster.csv")


# This plots calibration, stratified by region
ggplot(pdat,
       aes(x=pred, y=truth,
           col=region))+
  geom_point()+
  geom_abline(aes(slope = 1, intercept = 0))+
  geom_smooth(aes(group = "1"), method = 'lm')+theme_minimal()+facet_wrap(.~region)

# This plots calibration (regions in different colors)
ggplot(pdat,
       aes(x=pred, y=truth,
           col=region))+
  geom_point()+
  geom_abline(aes(slope = 1, intercept = 0))+
  geom_smooth(mapping = aes(weight = weights, group = "1"), method = 'lm')+theme_minimal()


# Step 8: generate model and predictions with stratified bootstrap ---------------------------------------

# Define training set:
X_full <- X[!is.na(Y), ]
Y_full <- Y[!is.na(Y)]

# Create container matrix for predictions
pred_matrix <- data.frame()

# Generate model (= estimate) and bootstrap predictions 

# Define number of bootstrap iterations. We use 100.
B = 0 #100
counter = -1

# Loop over bootstrap iterations
for(i in 1:(B+1)){
  counter = counter + 1
  cat(paste("\n\nStarting B:", counter, "at : ", Sys.time(), "\n\n"))
  
  # Select observations for bootstrap (stratified)
  if(i == 1){
    # First fit is estimation (i.e. no random sampling of data)
    obs <- 1:nrow(X_full)
  } else {
    
    # Other fits use random sampling
    
    # Container for row indicies
    obs <- c()
    
    # Loop of to randomly sample observations
    while(length(obs) < nrow(X_full)){
      
      # First select a random country:
      new_obs <- which(X_full$iso3c == sample(unique(X_full$iso3c), 1))
      
      # Then randomly sample within country:
      new_obs <- sample(new_obs, length(new_obs), replace = T)
      obs <- c(obs, new_obs)
    }
  }
  
  # Define model weights - we use log(country population)
  temp_weights <- log(X_full$population[obs])/mean(log(X_full$population[obs]))
  
  # Fit model:
  gbt_model <- gbt.train(Y_full[obs], 
                         as.matrix(X_full[obs, predictors]), 
                         learning_rate = ifelse(i == 1, 0.001, 0.01),
                         verbose = 10,
                         algorithm = "vanilla",
                         weights = temp_weights)
  
  # Save model objects
  gbt.save(gbt_model, paste0("output-data/models/gbt_model_B_", i, ".agtb"))
  
  #print feature importance
  if(i == 1){
    importance <- gbt.importance(feature_names=predictors, object=gbt_model) %>%
      as.data.frame() %>%
      arrange(.)
    importance$names <- row.names(importance) 
    print(
      ggplot(importance[1:10,], aes(x = fct_reorder(names, .), y = .)) + geom_col() + 
        coord_flip() + 
        labs(x = "10 most importance features")
    )
  }
  
  cat(paste("\nCompleted B:", counter, "at : ", Sys.time(), "\n\n"))
  
  # Save model predictions
  preds <- predict(gbt_model, as.matrix(X[, predictors]))
  pred_matrix <- rbind(pred_matrix, preds)
}

# Clean up bootstrap prediction matrix:
pred_matrix <- t(pred_matrix)
if(B == 0){
  colnames(pred_matrix) <- c("estimate")
} else{
  colnames(pred_matrix) <- c("estimate", paste0("B", 1:B)) 
}
rownames(pred_matrix) <- 1:nrow(pred_matrix)

saveRDS(pred_matrix, "output-data/pred_matrix.RDS")

# See next script for continuation (including extracting daily and cumulative data with confidence intervals)