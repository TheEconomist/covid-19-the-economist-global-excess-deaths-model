# Step 1: import libraries ---------------------------------------

# Import libraries
library(tidyverse)
library(data.table)
library(lubridate)
library(countrycode)
options(scipen=999)

# Step 2: import excess deaths data frame with covariates ---------------------------------------
df <- pred_frame <- data.frame(fread("output-data/country_daily_excess_deaths_with_covariates.csv"))

# Select DV
dv <- "daily_excess_deaths_per_100k"

# Step 3: define predictors ---------------------------------------

# Select features to exclude
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

# Expand categorical variables
categorical <- c()
df <- data.frame(df)
for(i in predictors){
  if(class(df[, i]) == "character"){
    categorical <- c(categorical, i)
  }
}
for(i in categorical){
  df[is.na(df[, i]), i] <- paste0("Unknown ", i)
  temp <- model.matrix(~ . - 1, data = df[, i, drop = F])
  predictors <- setdiff(c(predictors, colnames(temp)), i)
  df <- cbind(df, temp)
  df[, i] <- NULL
}

# Convert all columns to numeric
for(i in predictors){
  df[, i] <- as.numeric(df[, i])
}

# Step 4: impute missing data (using min-impute coupled with one-hot encoding of NA locations) ---------------------------------------

# Do one-hot-encoding to deal with missing data
impute_missing <- function(X = df_wide,
                           method = "min-impute",
                           replace.inf = T){
  X <- data.table(X)
  invisible(lapply(names(X),function(.name) set(X, which(is.infinite(X[[.name]])), j = .name,value =NA)))
  X <- data.frame(X)
  
  # Find columns with missing values
  na_cols <- unlist(lapply(1:ncol(X), FUN = function(i){any(is.na(X[, i]))}))
  n <- nrow(X)

  # exit if no missing
  if(sum(na_cols) == 0){
    return(X)
  }
  
  # Generate matrix of zeroes
  XNA <- matrix(0, nrow=n, ncol=sum(na_cols))
  
  counter <- 1
  cat("\nImputing min values and adding missing data matrix:\n\n")
  pb <- txtProgressBar(min=0, max=sum(na_cols), style=3)
  
  for(j in which(na_cols)){
    #print(colnames(X)[j])
    if(method == "min-impute"){
      min_val <- min(X[,j], na.rm = T)
    }
    if(method == "mean-impute"){
      min_val <- mean(X[,j], na.rm = T) 
    }
    na_ind <- is.na(X[,j])
    XNA[na_ind, counter] <- 1
    X[na_ind,j] <- min_val - 1
    counter = counter + 1
    setTxtProgressBar(pb, value=counter)
  }
  cat("\n")
  
  XNA <- t(unique(t(XNA)))
  
  # reduce XNA
  ind <- 1
  while(ind < ncol(XNA))
  {
    equal <- rep(FALSE, ncol(XNA)-ind)
    for(j in (ind+1):ncol(XNA))
    {
      equal[j-ind] <- all(XNA[,ind] == XNA[,j])
    }
    sum(equal)
    
    XNA <- XNA[,!c( rep(F, ind), equal), drop=F]
    ind = ind + 1
  }
  
  colnames(XNA) <- paste0("NA_matrix_col", 1:ncol(XNA))
  
  class(XNA) <- "numeric"
  
  # Bind X and 1-hot of XNA
  dX <- cbind(X, XNA)
  
  return(dX)}

# predictors <- c("population_density", "daily_covid_deaths_per_100k",
#                 "daily_covid_cases_per_100k", "daily_positive_rate",
#                 "boix_democracy_yes_no", "island",
#                 "date",
#                 "population",
#                 "life_expectancy",
#                 "population_density",
#                 "wdi_gdppc_ppp", "wdi_urban_pop_1m_cities_pct",
#                 "lat_largest_city", "lng_largest_city",
#                 "demography_adjusted_ifr",
#                 "sero_nat_or_reg_delta_region_average",
#                 "sero_nat_delta_region_average",
#                 "daily_excess_deaths_per_100k_region_average",
#                 "daily_covid_deaths_per_100k_region_average",
#                 "daily_covid_cases_per_100k_region_average",
#                 "daily_positive_rate_sub_region_average")

X <- df[, c(predictors, "iso3c")]
X$region <- pred_frame$region # use this for plotting later
Y <- df[, dv]

# Step 5: collapse to weekly data to reduce noise and speed up calculations ---------------------------------------
ids <- paste0(X$iso3c, "_", round(X$date/7, 0))
for(i in setdiff(colnames(X), c("iso3c", "region"))){
  X[, i] <- ave(X[, i], ids, FUN = function(x){mean(x, na.rm = T)})
}
X <- X[!duplicated(ids), ]
Y <- Y[!duplicated(ids)]

# Step 6: reduce dimensionality through PCA ---------------------------------------

pca_simplify <- function(X, 
                         name = "healthcare",
                         vars = c("hospital_beds_per_thousand", 
                                       "life_expectancy", 
                                       "wdi_life_expectancy_at_birth",
                                       "wdi_life_expectancy_at_birth_dist_average",
                                       "wdi_life_expectancy_at_birth_contiguous_country_average"),
                         threshold = 0.9){
  
  vars <- vars[vars %in% colnames(X)]
  
  temp_X <- X[, vars]
  temp_X <- impute_missing(temp_X, method = "mean-impute")
  
  pca <- prcomp(temp_X, center = TRUE, scale = TRUE)
  keep <- 1:min(which(summary(pca)$importance[3, ] > threshold))
  
  components <- pca$x[, keep]
  if(ncol(components) > ncol(temp_X)){
    components <- temp_X
  }
  
  colnames(components) <- paste0(name, "_", colnames(components))
  return(components)
}

# Baseline groupings
if(TRUE){
groups = list("healthcare" =  c("hospital_beds_per_thousand",
                                "life_expectancy",
                                "wdi_life_expectancy_at_birth_region_average",
                                "wdi_life_expectancy_at_birth_sub_region_average",
                                "wdi_life_expectancy_at_birth_econ_region_average",
              "wdi_life_expectancy_at_birth_dist_average",
              "wdi_life_expectancy_at_birth_contiguous_country_average",
              "wdi_life_expectancy_at_birth"),
              
              "demography" =  c("population",
                                "population_density",
                                "median_age",
                                "aged_65_older",
                                "aged_70_older",
                                "median_age_region_average",
                                "median_age_sub_region_average",
                                "median_age_econ_region_average",
                                "demography_adjusted_ifr_region_average",
                                "demography_adjusted_ifr_sub_region_average",
                                "demography_adjusted_ifr_econ_region_average",
                                "median_age_dist_average",
                                "median_age_contiguous_country_average",
                                "demography_adjusted_ifr_dist_average",
                                "demography_adjusted_ifr_contiguous_country_average",
                                "largest_city_pop_pct",
                                "demography_adjusted_ifr",
                                "wdi_pop_over_65",
                                "wdi_pop_under_15"),
              
              "vaccinations" = c("daily_vaccinations",
                                 "daily_vaccinations_per_100k",
                                 "vaccinated_pct",
                                 "fully_vaccinated_pct",
                                 "cumulative_daily_vaccinations_per_100k"),
              
              "geography" = c("lat_largest_city",
                              "lng_largest_city",
                              "lat_capital",
                              "lng_capital",
                              "island"),
              
              "gov_type" = c("vdem_freedom_of_expression_score" ,                      
                              "vdem_liberal_democracy_score"     ,                      
                              "boix_democracy_yes_no"            ,                      
                              "boix_democracy_duration_years"    ,                      
                              "freedom_house_civil_liberties"    ,                      
                              "freedom_house_political_rights"   ,                      
                              "freedom_house_freedom_score"      ,                      
                              "polity_democracy_score"),
              
              "economy" = c("wdi_prop_less_2_usd_day"         ,                       
                            "wdi_gdppc_nominal"               ,                       
                            "wdi_gdppc_ppp"                   ,                       
                            "wdi_urban_population_pct"        ,                       
                            "wdi_urban_pop_1m_cities_pct"     ,                       
                            "wdi_gini_index")                  ,                       
              
              "mobility" = c("tourist_arrivals_in_thousands_2019",                    
              "mobility_retail_rec_pct_of_baseline"              ,     
              "mobility_grocery_and_pharma_pct_of_baseline"      ,     
              "mobility_parks_pct_of_baseline"                   ,     
              "mobility_transit_rec_pct_of_baseline"             ,     
              "mobility_workplaces_rec_pct_of_baseline")          ,     
              
              "gov_policy" = c("oxcgrt_schools_closed"                       ,           
                                "oxcgrt_workplaces_closed"                    ,           
                                "oxcgrt_cancel_public_events"                 ,           
                                "oxcgrt_gathering_restrictions"               ,           
                                "oxcgrt_public_transport_closed"              ,           
                                "oxcgrt_stay_at_home_required"                ,           
                                "oxcgrt_internal_movement_restrictions"       ,           
                                "oxcgrt_international_movement_restrictions"  ,           
                                "oxcgrt_face_masks_required")        ,
            
              "local_excess_deaths" = c("daily_excess_deaths_per_100k_region_average",       "daily_excess_deaths_per_100k_sub_region_average",        
                              "daily_excess_deaths_per_100k_econ_region_average",     
                               "daily_excess_deaths_per_100k_dist_average"        ,      
                               "daily_excess_deaths_per_100k_contiguous_country_average"),
              
              "covid_cases_and_tests" = c(
                          "daily_tests_per_100k_region_average"                    ,
                          "daily_tests_per_100k_sub_region_average"                ,
                          "daily_tests_per_100k_econ_region_average"          ,
                           "daily_tests_per_100k_dist_average"                     ,
                           "daily_tests_per_100k_contiguous_country_average"       ,
                           "daily_tests_per_100k_dist_average"                     ,
                           "daily_tests_per_100k_contiguous_country_average"       ,
                          "cumulative_daily_tests_per_100k"                       ,
                          "daily_tests"                                           ,
                          "daily_tests_per_100k"                                  ,
                          "daily_covid_cases"                                     ,
                          "daily_covid_cases_per_100k"                            ,
                          "daily_positive_rate"                                   ,
                          "daily_covid_cases_per_100k_region_average"             ,
                          "daily_covid_cases_per_100k_sub_region_average"         ,
                          "daily_covid_cases_per_100k_econ_region_average"        ,
                          "daily_positive_rate_region_average"                    ,
                          "daily_positive_rate_sub_region_average"                ,
                          "daily_positive_rate_econ_region_average"               ,
                           "daily_covid_cases_per_100k_dist_average"               ,
                           "daily_covid_cases_per_100k_contiguous_country_average" ,
                          "cumulative_daily_covid_cases_per_100k"  ),                
              
              "covid_deaths" = c("daily_covid_deaths_per_100k_region_average"      ,       
                                 "daily_covid_deaths_per_100k_sub_region_average"  ,       
                                 "daily_covid_deaths_per_100k_econ_region_average" ,       
                                 "cumulative_daily_covid_deaths_per_100k"          ,       
                                "daily_covid_deaths"                               ,      
                                 "daily_covid_deaths_per_100k")    ,
              
              "seroprevalence" = c(
              "sero_nat_or_reg_delta_dist_average"                   ,  
              "sero_nat_or_reg_delta_contiguous_country_average"     ,  
              "sero_nat_delta_dist_average"                          ,  
              "sero_nat_delta_contiguous_country_average"           ,   
              "sero_nat_or_reg_delta"                                ,  
              "sero_nat_delta"                                       ,  
              "sero_nat_or_reg_delta_region_average"                 ,  
              "sero_nat_or_reg_delta_sub_region_average"             ,  
              "sero_nat_or_reg_delta_econ_region_average"            ,  
              "sero_nat_delta_region_average"                        ,  
              "sero_nat_delta_sub_region_average"                    ,  
              "sero_nat_delta_econ_region_average")                   ,  
              
              "econ_development" = c(
              "wb_income_groupHigh.income"                  ,         
              "wb_income_groupLow.income"                   ,         
              "wb_income_groupLower.middle.income"          ,         
              "wb_income_groupUpper.middle.income"          ,         
              "imf_economyAdvanced"                         ,         
              "imf_economyEmerging"                        ),         
            
              "region" = c(
              "continentAfrica"                                 ,       
              "continentAmericas"                               ,       
              "continentAsia"                                   ,       
              "continentEurope"                                 ,       
              "continentOceania"           ,
              "regionEast.Asia...Pacific"                       ,       
              "regionEurope...Central.Asia"                     ,       
              "regionLatin.America...Caribbean"                 ,       
              "regionMiddle.East...North.Africa"                ,       
              "regionNorth.America"                             ,       
              "regionSouth.Asia"                                ,       
              "regionSub.Saharan.Africa"                        ,       
              "subregionAustralia.and.New.Zealand"              ,       
              "subregionCentral.Asia"                           ,       
              "subregionEastern.Asia"                           ,       
              "subregionEastern.Europe"                         ,       
              "subregionLatin.America.and.the.Caribbean"        ,       
              "subregionMelanesia"                              ,       
              "subregionMicronesia"                             ,       
              "subregionNorthern.Africa"                        ,       
              "subregionNorthern.America"                       ,       
              "subregionNorthern.Europe"                        ,       
              "subregionPolynesia"                              ,       
              "subregionSouth.eastern.Asia"                     ,       
              "subregionSouthern.Asia"                          ,       
              "subregionSouthern.Europe"                        ,       
              "subregionSub.Saharan.Africa"                     ,       
              "subregionWestern.Asia"                           ,       
              "subregionWestern.Europe")           
              )  
}              

# These resulted in two many variables - so I cut it down even further by skipping more of them:
# Reducing dimensionality manually:
if(TRUE){
  groups = list("healthcare" =  c("hospital_beds_per_thousand",
                                  "life_expectancy"#,
                                  # "wdi_life_expectancy_at_birth_region_average",
                                  # "wdi_life_expectancy_at_birth_sub_region_average",
                                  # "wdi_life_expectancy_at_birth_econ_region_average",
                                  # "wdi_life_expectancy_at_birth_dist_average",
                                  # "wdi_life_expectancy_at_birth_contiguous_country_average",
                                  # "wdi_life_expectancy_at_birth"
                                  ),
                
                "demography" =  c("population",
                                  #"population_density",
                                  "median_age",
                                  "aged_65_older",
                                  #"aged_70_older",
                                  # "median_age_region_average",
                                  # "median_age_sub_region_average",
                                  # "median_age_econ_region_average",
                                  # "demography_adjusted_ifr_region_average",
                                  # "demography_adjusted_ifr_sub_region_average",
                                  # "demography_adjusted_ifr_econ_region_average",
                                  # "median_age_dist_average",
                                  # "median_age_contiguous_country_average",
                                  # "demography_adjusted_ifr_dist_average",
                                  # "demography_adjusted_ifr_contiguous_country_average",
                                  # "largest_city_pop_pct",
                                  "demography_adjusted_ifr"#,
                                  #"wdi_pop_over_65",
                                  #"wdi_pop_under_15"
                                  ),
                
                #"vaccinations" = c("daily_vaccinations",
                #                   "daily_vaccinations_per_100k",
                #                   "vaccinated_pct",
                #                   "fully_vaccinated_pct",
                #                   "cumulative_daily_vaccinations_per_100k"),
                
                "geography" = c("lat_largest_city",
                                "lng_largest_city",
                                #"lat_capital",
                                #"lng_capital",
                                "island"),
                
                "gov_type" = c(#"vdem_freedom_of_expression_score" ,                      
                               "vdem_liberal_democracy_score"     ,                      
                               "boix_democracy_yes_no"            ,                      
                               "boix_democracy_duration_years"    ,                      
                               #"freedom_house_civil_liberties"    ,                      
                               #"freedom_house_political_rights"   ,                      
                               #"freedom_house_freedom_score"      ,                      
                               "polity_democracy_score"
                               ),
                
                "economy" = c(#"wdi_prop_less_2_usd_day"         ,                       
                              #"wdi_gdppc_nominal"               ,                       
                              "wdi_gdppc_ppp"                   ,                       
                              #"wdi_urban_population_pct"        ,                       
                              #"wdi_urban_pop_1m_cities_pct"     ,                       
                              "wdi_gini_index")                  ,                       
                
                "mobility" = c(#"tourist_arrivals_in_thousands_2019",                    
                               "mobility_retail_rec_pct_of_baseline"              ,     
                               "mobility_grocery_and_pharma_pct_of_baseline"      ,     
                               #"mobility_parks_pct_of_baseline"                   ,     
                               #"mobility_transit_rec_pct_of_baseline"             ,     
                               "mobility_workplaces_rec_pct_of_baseline")          ,     
                
                "gov_policy" = c(#"oxcgrt_schools_closed"                       ,           
                                 "oxcgrt_workplaces_closed"                    ,           
                                 #"oxcgrt_cancel_public_events"                 ,           
                                 #"oxcgrt_gathering_restrictions"               ,           
                                 #"oxcgrt_public_transport_closed"              ,           
                                 "oxcgrt_stay_at_home_required"      #          ,           
                                 #"oxcgrt_internal_movement_restrictions"       ,           
                                 #"oxcgrt_international_movement_restrictions"  ,           
                                 #"oxcgrt_face_masks_required"
                                 )        ,
                
                "local_excess_deaths" = c("daily_excess_deaths_per_100k_region_average",      # "daily_excess_deaths_per_100k_sub_region_average",        
                                          # "daily_excess_deaths_per_100k_econ_region_average",     
                                          "daily_excess_deaths_per_100k_dist_average"        ,      
                                          "daily_excess_deaths_per_100k_contiguous_country_average"),
                
                "covid_cases_and_tests" = c(
                  # "daily_tests_per_100k_region_average"                    ,
                  # "daily_tests_per_100k_sub_region_average"                ,
                  # "daily_tests_per_100k_econ_region_average"          ,
                  # "daily_tests_per_100k_dist_average"                     ,
                  # "daily_tests_per_100k_contiguous_country_average"       ,
                  # "daily_tests_per_100k_dist_average"                     ,
                  "daily_tests_per_100k_contiguous_country_average"       ,
                 # "cumulative_daily_tests_per_100k"                       ,
                #  "daily_tests"                                           ,
                  "daily_tests_per_100k"                                  ,
                #  "daily_covid_cases"                                     ,
                  "daily_covid_cases_per_100k"                            ,
                  "daily_positive_rate"                                #   ,
                  # "daily_covid_cases_per_100k_region_average"             ,
                  # "daily_covid_cases_per_100k_sub_region_average"         ,
                  # "daily_covid_cases_per_100k_econ_region_average"        ,
                  # "daily_positive_rate_region_average"                    ,
                #   "daily_positive_rate_sub_region_average"                ,
                #   "daily_positive_rate_econ_region_average"               ,
                #   "daily_covid_cases_per_100k_dist_average"               ,
                #   "daily_covid_cases_per_100k_contiguous_country_average" ,
                #   "cumulative_daily_covid_cases_per_100k"  
                # 
                ),                
                
                "covid_deaths" = c("daily_covid_deaths_per_100k_region_average"      ,       
                                  # "daily_covid_deaths_per_100k_sub_region_average"  ,       
                                  # "daily_covid_deaths_per_100k_econ_region_average" ,       
                                  # "cumulative_daily_covid_deaths_per_100k"          ,       
                                  # "daily_covid_deaths"                               ,      
                                   "daily_covid_deaths_per_100k")    ,
                
                "seroprevalence" = c(
                  "sero_nat_or_reg_delta_dist_average"                  ,  
                 # "sero_nat_or_reg_delta_contiguous_country_average"     ,  
                 # "sero_nat_delta_dist_average"                          ,  
                #  "sero_nat_delta_contiguous_country_average"           ,   
                  "sero_nat_or_reg_delta"                           #     ,  
                #  "sero_nat_delta"                                       ,  
                #  "sero_nat_or_reg_delta_region_average"                 ,  
                #  "sero_nat_or_reg_delta_sub_region_average"             ,  
                #  "sero_nat_or_reg_delta_econ_region_average"            ,  
                #  "sero_nat_delta_region_average"                        ,  
                #  "sero_nat_delta_sub_region_average"                    ,  
                #  "sero_nat_delta_econ_region_average"
                )     #              ,  
                
              #  "econ_development" = c(
              #    "wb_income_groupHigh.income"                  ,         
              #    "wb_income_groupLow.income"                   ,         
              #    "wb_income_groupLower.middle.income"          ,         
              #    "wb_income_groupUpper.middle.income"          ,         
              #    "imf_economyAdvanced"                         ,         
              #    "imf_economyEmerging"                        ),         
                
                # "region" = c(
                #   "continentAfrica"                                 ,       
                #   "continentAmericas"                               ,       
                #   "continentAsia"                                   ,       
                #   "continentEurope"                                 ,       
                #   "continentOceania"           ,
                #   "regionEast.Asia...Pacific"                       ,       
                #   "regionEurope...Central.Asia"                     ,       
                #   "regionLatin.America...Caribbean"                 ,       
                #   "regionMiddle.East...North.Africa"                ,       
                #   "regionNorth.America"                             ,       
                #   "regionSouth.Asia"                                ,       
                #   "regionSub.Saharan.Africa"                        ,       
                #   "subregionAustralia.and.New.Zealand"              ,       
                #   "subregionCentral.Asia"                           ,       
                #   "subregionEastern.Asia"                           ,       
                #   "subregionEastern.Europe"                         ,       
                #   "subregionLatin.America.and.the.Caribbean"        ,       
                #   "subregionMelanesia"                              ,       
                #   "subregionMicronesia"                             ,       
                #   "subregionNorthern.Africa"                        ,       
                #   "subregionNorthern.America"                       ,       
                #   "subregionNorthern.Europe"                        ,       
                #   "subregionPolynesia"                              ,       
                #   "subregionSouth.eastern.Asia"                     ,       
                #   "subregionSouthern.Asia"                          ,       
                #   "subregionSouthern.Europe"                        ,       
                #   "subregionSub.Saharan.Africa"                     ,       
                #   "subregionWestern.Asia"                           ,       
                #   "subregionWestern.Europe")           
  )  
}              


new_X <- data.frame(date = as.numeric(X$date))
pca_summary <- data.frame("group"= "none", "input_ncol" = NA, "output_ncol" = NA)
for(i in 1:length(groups)){
  temp <- pca_simplify(X,
                       name = names(groups[i]),
                       vars = groups[[i]],
                       threshold = 0.9)
  new_X <- cbind(new_X, temp)
  pca_summary <- rbind(pca_summary,
                   c(names(groups[i]),
                     length(groups[[i]]),
                     ncol(temp)))
  
}

pca_summary[1, ] <- c("all", ncol(X),
                      ncol(new_X))
pca_summary


# Step 7: Construct model and predict on hold-out ---------------------------------------

# Add square term:
new_X$iso3c <- NULL

for(i in colnames(new_X)){
  new_X[, paste0(i, "_sqrd")] <- new_X[, i]^2
}

# Next run glmnet:
library(glmnet)

# Add pairwise interactions:
glmnet_x <- model.matrix(~(.)^2, data = new_X[])
glmnet_y <- Y

# Drop NA
glmnet_x <- glmnet_x[!is.na(glmnet_y), ] 
glmnet_y <- glmnet_y[!is.na(glmnet_y)] 

# Define cv-folds
iso3c <- X$iso3c[!is.na(Y)]

cv_folds <- function(x, n = 10){
  # Randomize order
  x <- sample(x)
  
  # Divide into equalish groups:
  split(x, cut(seq_along(x), n, labels = FALSE))} 

folds <- cv_folds(unique(iso3c), n = 10)

#### Loop over cv-folds, predicting on hold-out set:
weights <- log(X$population[!is.na(Y)])
iso3c <- X$iso3c[!is.na(Y)]

# Get fold-id numeric:
folds_by_obs <- rep(NA, length(glmnet_y))
for(i in 1:length(folds)){
  folds_by_obs[iso3c %in% folds[[i]]] <- i
}

# Saving these so we can replicate the procedure for agtboost:
saveRDS(folds, "output-data/folds.RDS")

# Create container variable:
results <- data.frame(target = glmnet_y, 
                      preds = rep(NA, length(glmnet_y)),
                      weights = weights,
                      iso3c = iso3c)

vars <- list()
for(i in 1:length(folds)){
  
  # Define training data
  train_x <- glmnet_x[!iso3c %in% folds[[i]], ]
  train_y <- glmnet_y[!iso3c %in% folds[[i]]]
  train_w <- weights[!iso3c %in% folds[[i]]]
  training_folds <- folds_by_obs[!iso3c %in% folds[[i]]]
  training_folds <- as.numeric(as.factor(training_folds))
  
  # Define testing data
  test_x <- glmnet_x[iso3c %in% folds[[i]], ]
  test_y <- glmnet_y[iso3c %in% folds[[i]]]
  test_w <- weights[iso3c %in% folds[[i]]]
  
  # Fit cv-glmnet
  glm_fit <- cv.glmnet(train_x,
                       as.matrix(train_y),
                       weights = train_w,
                       foldid = training_folds,
                       trace.it = 1)
  
  print("Selected variables:")
  print(LASSO_vars <- rownames(coef(glm_fit, s = 'lambda.1se'))[coef(glm_fit, s = 'lambda.1se')[,1]!= 0])
  
  vars <- c(vars, "vars" = list(LASSO_vars))

  # Predict:
  results$preds[results$iso3c %in% folds[[i]]] <- predict(glm_fit, 
                                                          newx = test_x, 
                                                          s = "lambda.1se")

  }

# Weighted mean-squared error:
mean((abs(results$target - results$preds)^2)*results$weights/mean(results$weights))

write_csv(results, "output-data/results_glmnet.csv")

