# Step 1: import libraries ---------------------------------------

# Import libraries
library(tidyverse)
library(data.table)
library(lubridate)
library(countrycode)
options(scipen=999)

# Step 2: import excess deaths data frame with covariates ---------------------------------------
df <- pred_frame <- data.frame(readRDS("output-data/country_daily_excess_deaths_with_covariates.RDS"))
df <- df[order(df$date), ]
pred_frame <- pred_frame[order(pred_frame$date), ]

# Select DV
dv <- "daily_excess_deaths_per_100k"

# Step 3: define predictors ---------------------------------------

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

# In this case a recent drop in US excess deaths meant checking the underlying CDC data. In line with their estimates of when reporting lags were likely to be influential at the time the model was fitted, some then very recent observations were dropped:
Y <- Y[!(X$iso3c == "USA" & X$date > 18839)]
X <- X[!(X$iso3c == "USA" & X$date > 18839), ]

# A similar anamolous drop was found in Mumbai at the very tail end of its observations:
Y <- Y[!(X$iso3c == "IND_Mumbai_City" & X$date > 18744)]
X <- X[!(X$iso3c == "IND_Mumbai_City" & X$date > 18744),]

# And in Malaysia:
Y <- Y[!(X$iso3c == "MYS" & X$date > 18748)]
X <- X[!(X$iso3c == "MYS" & X$date > 18748),]

# Furthermore, the drop-off of almost all series after July 10th suggests region averages will be skewed beyond this point, these very few weeks from just a few countries were dropped. This also reduced probability of bias from reporting lags influencing the model.  
Y <- Y[!(X$date > 18841)]
X <- X[!(X$date > 18841), ]

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

# Define training set:
X_full <- X[!is.na(Y), ]
Y_full <- Y[!is.na(Y)]

# Define weights, using half-values for subnational units to reflect greater uncertainty in their covariates and estimates. That value was selected based on correlations between covid-deaths and excess deaths in these units (which was much weaker - these also had fewer non-NA observations):
X_full$weights <- log(X_full$population)
X_full$weights[nchar(X_full$iso3c) > 3] <- X_full$weights[nchar(X_full$iso3c) > 3]/2

#  Dividing weights if multiple subunits for one country (this matters because observations are weighted by log population rather than absolute population):
for(i in unique(substr(df$iso3c, 1, 3))){
  n_units <- length(unique(df$iso3c[substr(df$iso3c, 1, 3) == i & !is.na(df$daily_excess_deaths_per_100k)]))
  if(n_units > 1){
    X_full$weights[substr(X_full$iso3c, 1, 3) == i] <-     X_full$weights[substr(X_full$iso3c, 1, 3) == i]/n_units
  }
}

# Create container matrix for predictions
pred_matrix <- data.frame()

# Define predictors
m_predictors <- setdiff(colnames(X_full), c("iso3c", "region", "weights", "date"))

# Save these predictor names
saveRDS(m_predictors, "output-data/model-objects/m_predictors.RDS")

# Generate model (= estimate) and bootstrap predictions 
library(agtboost)

# Define number of bootstrap iterations. We use 200.
B = 200

set.seed(112358)

# Loop over bootstrap iterations
for(i in 1:(B+1)){
  cat(paste("\n\nStarting B:", i, "at : ", Sys.time(), "\n\n"))

  # Container for row indicies
  obs <- c()
  
  # Select observations for bootstrap (stratified)
  if(i == 1){
    # First fit is estimation (i.e. no random sampling of data)
    obs <- 1:nrow(X_full)
  } else {
    
    # Other fits use stratified bootstrap
    iso3cs <- sample(unique(X_full$iso3c), length(unique(X_full$iso3c)), replace = T)
    
    for(j in 1:length(iso3cs)){
      obs <- c(obs, sample(which(X_full$iso3c == iso3cs[j]), length(which(X_full$iso3c == iso3cs[j])), replace = T))
    }
  }
  
  # Define model weights - we use log(country population)
  weights_temp <- X_full$weights[obs]/mean(X_full$weights[obs])

  Y_temp <- Y_full[obs]
  X_temp <- as.matrix(X_full[obs, m_predictors])
  
  lr_temp <- ifelse(i == 1, 0.001, 0.003)

  # Fit model:
  gbt_model <- gbt.train(Y_temp, 
                         X_temp, 
                         learning_rate = lr_temp,
                         nrounds = 35000,
                         verbose = 200,
                         algorithm = "global_subset",
                         weights = weights_temp)
  
  # Save model objects
  gbt.save(gbt_model, paste0("output-data/model-objects/gbt_model_B_", i, ".agtb"))
  
  cat(paste("\nCompleted B:", i, "at : ", Sys.time(), "\n\n"))
  
}

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