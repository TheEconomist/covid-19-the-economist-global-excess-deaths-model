# Step 1: import libraries ---------------------------------------

# Import libraries
library(tidyverse)
library(data.table)
library(lubridate)
library(countrycode)
options(scipen=999)
inspect <- FALSE

# Step 2: import excess deaths data frame with covariates ---------------------------------------
df <- pred_frame <- data.frame(readRDS("output-data/country_daily_excess_deaths_with_covariates.RDS"))
df <- df[order(df$date), ]
pred_frame <- pred_frame[order(pred_frame$date), ]

# Select DV
dv <- "daily_excess_deaths_per_100k"

# Step 3: define predictors --------------------  -------------------

# Select features to exclude (as we don't have these for countries where we don't have excess deaths or they are subsumed by other covariates or are country-specific, perfectly co-linear with region, or are not per capita)
exclude <- c("daily_total_deaths",
             "daily_total_deaths_per_100k",
             "daily_expected_deaths",
             "daily_expected_deaths_per_100k",
             "daily_excess_deaths",
             "daily_tests",
             "daily_covid_cases",
             "daily_covid_deaths",
             "daily_vaccinations",
             "iso3c",
             "country",
             "continent",
             "imf_economy",
             "wdi_life_expectancy_at_birth_dist_average",
             "wdi_life_expectancy_at_birth_contiguous_country_average")

# Define predictors
predictors <- setdiff(colnames(df), c(dv, exclude))

# Save these predictor names
saveRDS(predictors, "output-data/model-objects/predictors.RDS")

# Expand categorical variables and update list of predictors
source("scripts/shared-functions/expand_categorical.R")
temp <- expand_categorical(df, predictors)
df <- temp[[1]]
predictors <- temp[[2]]

X <- df
Y <- df[, dv]

# Step 4: Remove observations plausibly affected by reporting lag issues ---------------------------------------

#### Excess deaths figures were inspected manually anomolous drops in excess deaths for very recent dates, which could be due to reporting lags (at the time of model estimation). For detected cases, more investigation was initiated.

if(inspect){
  library(ggplot2)
  pdat <- df
  ggplot(pdat[pdat$date >= as.Date('2021-10-01'), ], 
         aes(x=as.Date(date, origin = '1970-01-01'), y=daily_excess_deaths_per_100k, col = iso3c))+
    geom_line()+
    theme(legend.pos = 'none')+
    geom_vline(aes(xintercept = as.Date('2021-12-01')))+ # 
    geom_vline(aes(xintercept = as.Date('2021-12-31')))+
    geom_line(data = pdat[pdat$date >= as.Date('2021-10-01') & pdat$iso3c == 'USA', ], size = 2)+
    geom_vline(aes(xintercept = Sys.Date()-28), size = 2)
}


# Drop very recent observations (<28 days):
Y <- Y[!X$date > Sys.Date()-21]
X <- X[!X$date > Sys.Date()-21, ]

# An anamolous drop was found in Mumbai at the very tail end of its observations. A source confirms that registration for that month was still ongoing (https://timesofindia.indiatimes.com/city/mumbai/excess-deaths-in-city-call-for-scientific-survey-tiss/articleshow/84001199.cms):
Y <- Y[!(X$iso3c == "IND_Mumbai_City" & X$date > 18744)]
X <- X[!(X$iso3c == "IND_Mumbai_City" & X$date > 18744),]

# Equador and Peru have backward adjusted their covid-19 deaths, incorporating excess deaths information. These therefore had to be dropped, as current covid deaths differ greatly from those backward adjusted:
Y <- Y[!X$iso3c %in% c("PER", "ECU")]
X <- X[!X$iso3c %in% c("PER", "ECU"), ]

# Step 5: collapse to weekly data to reduce noise and speed up calculations ---------------------------------------

ids <- paste0(X$iso3c, "_", round(X$date/7, 0))

# Loop through country-weeks, calculate mean of predictors:
for(i in setdiff(colnames(X), c("iso3c", "region"))){
  X[, i] <- ave(X[, i], ids, FUN = function(x){mean(x, na.rm = T)})
}

# Reduce dimensionality by only using one observation for each week
X <- X[!duplicated(ids), ]
Y <- Y[!duplicated(ids)]

# Save covariates:
export <- pred_frame[!duplicated(ids), c("iso3c", "country", "date", "region", "subregion", "population", "median_age", "aged_65_older", "life_expectancy", "daily_covid_deaths_per_100k", "daily_covid_cases_per_100k", "daily_tests_per_100k", "cumulative_daily_covid_cases_per_100k", 
                                         "cumulative_daily_covid_deaths_per_100k",
                                         "cumulative_daily_tests_per_100k", "demography_adjusted_ifr",
                                         "daily_covid_cases",
                                         "daily_tests",
                                         "daily_covid_deaths",
                                         "daily_excess_deaths",
                                         dv)]
# saveRDS(export, "output-data/export_covariates.RDS")

# Step 6: impute missing data (using min-impute coupled with one-hot encoding of NA locations) ---------------------------------------

# Do one-hot-encoding to deal with missing data
source("scripts/shared-functions/impute_missing.R")
X <- impute_missing(X[, c(predictors, "iso3c")])

# Remove columns with sd = 0 or Inf observations:
ncol(X)
for(i in colnames(X)){
  if(is.numeric(X[, i])){
    if(is.nan(sd(X[, i]))){
      print(i)
      X[,i] <- NULL
    } else if(sd(X[, i]) == 0){
      print(i)
      X[,i] <- NULL
    }
  }
}
ncol(X)

# Reduce dimensionality of NA matrix: 
# (It is important that this is non-dynamic, as we otherwise would risk the column names and order messing up predictions):
NAm <- X[, grep("NA_matrix", colnames(X))]

library(DescTools)
# This function loads finds all variables with pairwise correlations over 0.99, then for each pair, selects the one with most correlation with other variables to remove.
NA_dimension_reduction <- FindCorr(cor(NAm), cutoff = 0.99)

# X <- X[, setdiff(colnames(X), colnames(NAm)[NA_dimension_reduction])]


# Make NA columns either 0 or 1
for(i in grep("NA_matrix", colnames(X))){
  X[X[, i] != 1, i] <- 0  
}

# Step 7: generate model and predictions with stratified bootstrap ---------------------------------------

# We first load the model-generation function:
source('scripts/aux_generate_model_loop.R')

# Define number of models with different seeds to combine via median for main estimate
main_estimate_models <- 10
saveRDS(main_estimate_models, "output-data/model-objects/main_estimate_models_n.RDS")

# We then use this to generate our main estimate (median of 10 models with different seeds) and 200 bootstrap samples
set.seed(112358)
generate_model_loop(
  X_full = X[!is.na(Y), ], # Defines training set
  Y_full = Y[!is.na(Y)],   # Defines outcome variable
  B = 200, # Defines number of bootstrap iterations. We use 200.
  include_main_estimate = T,
  main_estimate_model_n = main_estimate_models,
  main_estimate_learning_rate = 0.001,
  bootstrap_learning_rate = 0.003)

calibration = F
if(calibration){
# Step 7: construct calibration plot ---------------------------------------

# Select data
X_cv <- X[!is.na(Y), ]
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

folds <- cv_folds(unique(iso3c), n = 10) 

# We save a common set of folds here, to keep results comparable with other algorithms we test. Folds are stratified by country, i.e. no country is in the training and test set simultaneously
saveRDS(folds, "output-data/model-objects/folds.RDS")

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
  library(agtboost)
  gbt_fit <- gbt.train(train_y,
                       as.matrix(train_x[, setdiff(colnames(X_cv), c("iso3c", "region"))]), 
                       learning_rate = 0.01,
                       nrounds = 1500,
                       verbose = 10,
                       algorithm = "vanilla",
                       weights = train_w/mean(train_w))
  
  print(i)
  print("cross-validation round completed.")
  
  # Predict on fold:
  results$preds[results$iso3c %in% folds[[i]]] <- predict(gbt_fit, 
                                                          newdata = as.matrix(test_x[, setdiff(colnames(X_cv), c("iso3c", "region"))]))
}

# Weighted mean-squared error:
mean((abs(results$target - results$preds)^2)*results$weights/mean(results$weights))

# Save fold results (so we can compare with other folds)
write_csv(results, "output-data/results_gradient_booster.csv")


# Step 8: inspect predictions ---------------------------------------

# This creates a plotting data frame:
pdat <- cbind.data.frame(pred = results$preds, 
                         truth = results$target,
                         country = X_cv$iso3c,
                         region = X_cv$region,
                         w = results$weights)

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

}