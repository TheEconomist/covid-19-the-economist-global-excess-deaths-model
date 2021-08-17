# Step 1: import libraries ---------------------------------------

# Import libraries
library(tidyverse)
library(data.table)
library(lubridate)
library(countrycode)
library(agtboost)
options(scipen=999)

# Step 2: import excess deaths data frame with covariates ---------------------------------------
df <- pred_frame <- readRDS("output-data/country_daily_excess_deaths_with_covariates.Rds") %>%
  data.frame()
#df <- pred_frame <- data.frame(readRDS("output-data/country_daily_excess_deaths_with_covariates.RDS")) # <- to use pre-generated RDS

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

# Do one-hot-encoding to deal with missing data
impute_missing <- function(X = df_wide,
                           method = "min-impute",
                           replace.inf = T){
  X <- data.table(X)
  invisible(
    lapply(
      names(X),
      function(.name){set(X, which(is.infinite(X[[.name]])), j = .name,value =NA)}
      )
    )
  X <- data.frame(X)
  
  # Find columns with missing values
  na_cols <- X %>%
    summarise(
      across(
        everything(),
        ~any(is.na(.x))
      )
    ) %>%
    unlist()
  n <- nrow(X)
  
  # Generate matrix of zeroes
  XNA <- matrix(0, nrow=n, ncol=sum(na_cols))
  
  counter <- 1
  cat("\nImputing min values and adding missing data matrix:\n\n")
  pb <- txtProgressBar(min=0, max=sum(na_cols), style=3)
  
  for(j in names(which(na_cols))){
    #print(colnames(X)[j])
    if(method == "min-impute"){
      min_val <- min(X[[j]], na.rm = T)
    }
    if(method == "mean-impute"){
      min_val <- mean(X[[j]], na.rm = T) 
    }
    na_ind <- is.na(X[[j]])
    XNA[na_ind, counter] <- 1
    X[na_ind,j] <- min_val# - 1 removed this because it doesn't make any sense and gives non-sense values
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

X <- impute_missing(df[, c(predictors, "iso3c")])
X$region <- pred_frame$region # we use this for plotting later

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

#Get weekly average for outcome since not every weekly correctly aligns with the week calculated
Y <- df %>%
  mutate(
    `week` = round(date/7, 0)
  ) %>%
  group_by(week, iso3c) %>%
  summarise(
    across(
      all_of(dv),
      ~mean(.x, na.rm = T)
    )
  ) %>%
  ungroup() %>%
  arrange(iso3c, week) %>%
  pull(dv)

# Save covariates:
export <- pred_frame %>%
  filter(date %in% X$date) %>%
  select(
    any_of(
      c("iso3c", "country", "date", "region", "subregion", "population", "median_age", "aged_65_older", "life_expectancy", "daily_covid_deaths_per_100k", "daily_covid_cases_per_100k", "daily_tests_per_100k", "cumulative_daily_covid_cases_per_100k", 
        "cumulative_daily_covid_deaths_per_100k",
        "cumulative_daily_tests_per_100k", "demography_adjusted_ifr",
        "daily_covid_cases",
        "daily_tests",
        "daily_covid_deaths",
        "daily_excess_deaths",
        dv)
    )
  )#these aren't even the real covariates??
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

folds <- cv_folds(unique(iso3c), n = 10) # We load a common set of folds here, to keep results comparable with other algorithms we test. Folds are stratified by country, i.e. no country is in the training and test set simultaneously
#folds <- readRDS("output-data/folds.RDS")


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


# Step 7: inspect predictions ---------------------------------------

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


# Step 8: generate model and predictions with stratified bootstrap ---------------------------------------

# Define training set:
X_full <- X[!is.na(Y), ]
Y_full <- Y[!is.na(Y)]

# Create container matrix for predictions
pred_matrix <- data.frame()

# Define predictors
m_predictors <- setdiff(colnames(X_full), c("iso3c", "region"))

# Generate model (= estimate) and bootstrap predictions 

# Define number of bootstrap iterations. We use 100.
B = 10 #100
counter = -1

# Loop over bootstrap iterations
for(i in 1:(B+1)){
  counter = counter + 1
  cat(paste("\n\nStarting B:", counter, "at : ", Sys.time(), "\n\n"))
  
  # Select observations for bootstrap (stratified)
  if(B == 1){
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
                         as.matrix(X_full[obs, m_predictors]), 
                         learning_rate = ifelse(i == 1, 0.001, 0.01),
                         nrounds = 10000,
                         verbose = 10,
                         algorithm = "vanilla",
                         weights = temp_weights)
  
  # Save model objects
  gbt.save(gbt_model, paste0("output-data/gbt_model_B_", i, ".agtb"))
  
  cat(paste("\nCompleted B:", counter, "at : ", Sys.time(), "\n\n"))
  
  # Save model predictions
  preds <- predict(gbt_model, as.matrix(X[, m_predictors]))
  pred_matrix <- rbind(pred_matrix, preds)
  saveRDS(pred_matrix, "temp.RDS")
}

# Clean up bootstrap prediction matrix:
pred_matrix <- t(pred_matrix)
colnames(pred_matrix) <- c("estimate", paste0("B", 1:B))
rownames(pred_matrix) <- 1:nrow(pred_matrix)

estimate <- pred_matrix[, 1]

# Sort in case one wants to quickly extract bootstrap CI:
pred_matrix_sorted <- pred_matrix
for(i in 1:nrow(pred_matrix)){
  pred_matrix_sorted[i, ] <- sort(pred_matrix[i, ])
}

saveRDS(pred_matrix, "output-data/pred_matrix.RDS")
saveRDS(pred_matrix_sorted, "pred_matrix_sorted.RDS")

# See next script for continuation (including extracting daily and cumulative data with confidence intervals)