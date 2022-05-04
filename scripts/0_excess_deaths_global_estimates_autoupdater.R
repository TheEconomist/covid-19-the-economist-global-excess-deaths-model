#### Master Script  ---------------------------------------

# Import libraries
library(ggplot2)
library(tidyverse)
library(data.table)

options(scipen=999)

# Set warning log container
log <- c()

start_time <- Sys.time()

# 1. Update data ---------------------------------------

# For memory efficiency, this is executed within a temporary local environment created by the update_data function:
update_data <- function(){source("scripts/1_excess_deaths_global_estimates_data_generation.R", local = TRUE)}
update_data()  

# 2. Load updated data ---------------------------------------
dat <- readRDS("output-data/country_daily_excess_deaths_with_covariates.RDS")
dat <- dat[order(dat$date), ] 

# 3. Run diagnostics ---------------------------------------

# Check dataset balanced (all dates have equal number of observations)
if(length(unique(table(dat$date))) != 1){
  log <- c(log, "-- Not all dates have equal number of observations --")
}

# Check dataset balanced (all iso3c have equal number of observations)
if(length(unique(table(dat$iso3c))) != 1){
  log <- c(log, "-- Not all iso3c have equal number of observations --")
}

# Check dataset up to current day
if(max(dat$date) >= Sys.Date()-1){
  log <- c(log, "-- Dataset not updated to present day (or yesterday) --")
}

diagnostics <- FALSE
if(diagnostics){
  # Check data coverage by date and country:
  pdat <- dat
  pdat$'Missing Values' <- apply(pdat, 1, function(x) sum(is.na(x)))
  pdat$'Missing Values' <- 100*pdat$'Missing Values'/ncol(pdat)
  
  ggplot(pdat, aes(x = date, y = iso3c, fill= `Missing Values`)) + 
    geom_tile()+
    scale_fill_gradient(low="white", 
                        high="deepskyblue",
                        breaks = c("0%" = 0, "50%"= 50, "100%" = 100), 
                        limits = c(0, 100))+
    geom_vline(aes(xintercept = as.Date("2021-05-10")))+xlab("")+ylab("")
  ggsave("plots/diagnostic_1_NA_by_iso3c_and_day.png", height = 14, width = 10)
  
  # Check data coverage by date and country:
  pdat <- dat
  pdat[, setdiff(colnames(pdat), c("date", "iso3c"))] <- as.numeric(is.na(pdat[, setdiff(colnames(pdat), c("date", "iso3c"))]))
  pdat <- pivot_longer(data = pdat,
                       cols = setdiff(colnames(pdat), c("date", "iso3c")))
  pdat[, "Missing Values"] <- ave(pdat$value, paste0(pdat$date, "_", pdat$name), FUN = sum)
  pdat$`Missing Values` <- 100*as.numeric(pdat$`Missing Values`)/length(unique(pdat$iso3c))
  pdat <- pdat[!duplicated(paste0(pdat$date, "_", pdat$name)), ]
  pdat$iso3c <- NULL
  
  ggplot(pdat[, ], aes(x = date, y = name, fill= `Missing Values`)) + 
    geom_tile()+
    scale_fill_gradient(low="white", 
                        high="deepskyblue",
                        breaks = c("0%" = 0, "50%"= 50, "100%" = 100), 
                        limits = c(0, 100))+
    geom_vline(aes(xintercept = as.Date("2021-05-10")))+
    xlab("")+ylab("")
  ggsave("plots/diagnostic_2_NA_by_variable_and_day.png", height = 14, width = 10)
  
  rm(pdat)
}

# 4. Generate data matrix ------------------------------------------------------ 

# Load set of predictors used
predictors <- readRDS("output-data/model-objects/predictors.RDS")

# Expand categorical variables and update list of predictors
source("scripts/shared-functions/expand_categorical.R")
temp <- expand_categorical(dat, predictors)
df <- temp[[1]]
predictors <- temp[[2]]
X <- df

# Collapse to weekly data through means
# 1. Ensure current date will be included as prediction target:
X_latest <- X[X$date %in% max(X$date):(max(X$date)-7), ]

# 2. Loop columns to compute country-week averages (by country-week):
ids <- paste0(X$iso3c, "_", round(X$date/7, 0))
for(i in setdiff(colnames(X), c("iso3c"))){
  X[, i] <- ave(X[, i], ids, FUN = function(x){mean(x, na.rm = T)})
  X_latest[, i] <- ave(X_latest[, i], X_latest$iso3c, 
                       FUN = function(x){mean(x, na.rm = T)})
}

# 3. Combine the latest day with the rest of the data:
X <- rbind(X[!duplicated(ids), ],
           X_latest[!duplicated(X_latest$iso3c), ])
rm(X_latest)

# Impute missing data (using min-impute coupled with one-hot encoding of NA locations) 

# 1. Impute missing:
source("scripts/shared-functions/impute_missing.R")
X <- impute_missing(X[order(X$date), c(predictors, "iso3c")], cached_NA_cols = T)

# 2. Make NA indicators either 0 or 1
for(i in grep("NA_matrix", colnames(X))){
  X[X[, i] != 1, i] <- 0  
}

# Extract DV vector (with the same length and order as the X matrix)
Y <- X[, c('iso3c', 'date')]
Y$order <- 1:nrow(Y)
Y <- merge(Y[, c('iso3c', 'date', 'order')], 
           df[, c('iso3c', 'date', 'daily_excess_deaths_per_100k')], all.x = T)
Y <- Y[order(Y$order), 'daily_excess_deaths_per_100k']

# Clean workspace for memory efficiency
saveRDS(X, 'output-data/model-objects/X_train.RDS')
saveRDS(Y, 'output-data/model-objects/Y_train.RDS')
saveRDS(dat, "output-data/model-objects/dat.RDS")
rm(Y)
rm(dat)
rm(temp)
rm(df)
gc()

# 5. Load models and populate prediction matrix --------------------------------------- 

# Create container matrix for predictions
pred_matrix <- data.frame()

# Load model (= estimate) and bootstrap predictions 

# Load list of model predictors
m_predictors <- readRDS("output-data/model-objects/m_predictors.RDS")

# Define number of bootstrap iterations. We use 200.
B <- 200
counter <- 0

# Define ensemble size for central estimate
main_estimate_models <- readRDS("output-data/model-objects/main_estimate_models_n.RDS")

# Select predictors and create predictor matrix
X <- as.matrix(X[, m_predictors])

# Load machine learning library
library(agtboost)

# The update now happens in two runs, updating twice daily. The first loads predictions from models 1:110. The second refines these by re-calculating models 1:10, and adding 110:210. We ensure that a day of updates always start with update run A.
current_update_run <- readRDS('output-data/model-objects/current_update_run.RDS') 
if(readRDS('output-data/model-objects/latest_update.RDS') < Sys.Date()){
  current_update_run <- "A"
}

if(current_update_run == "A"){
  load_predictions_model_set <- 1:(floor(B/2)+main_estimate_models)
} else {
  load_predictions_model_set <- c(1:main_estimate_models, main_estimate_models+(floor(B/2)+1):B)
}

# Loop over bootstrap iterations
for(i in load_predictions_model_set){
  counter <- counter + 1
  cat(paste("\n\nStarting prediction by model:", counter, "of", ifelse(current_update_run == "A", floor(B/2), B)+main_estimate_models, "at : ", Sys.time(), "\n"))
  
  # Load model object
  cat("\n -- loading model -- ")
  gbt_model <- gbt.load(paste0("output-data/model-objects/gbt_model_B_", i, ".agtb"))
  
  # Save model predictions
  cat("generating predictions -- ")
  preds <- rep(NA, nrow(X))
  preds <- predict(gbt_model, newdata = X)
  rm(gbt_model)
  cat("saving prediction --\n")
  saveRDS(preds, paste0('output-data/model-objects/model-predictions/model_', i , '_prediction.RDS'))
  rm(preds)
  
  cat(paste("\nCompleted:", counter, "at : ", Sys.time(), "\n\n"))
  gc()
}

# Re-import base libraries post-loop
library(ggplot2)
library(tidyverse)
library(data.table)

# Load and combine predictions from individual models:
for(i in 1:(ifelse(current_update_run == "A", floor(B/2), B)+main_estimate_models)){
  pred_matrix <- rbind(pred_matrix, readRDS(paste0('output-data/model-objects/model-predictions/model_', i , '_prediction.RDS')))
}

# Fix column and row names of prediction matrix:
pred_matrix <- t(pred_matrix)

# Combine main estimate models (with different seeds) via median
if(main_estimate_models > 1){
  pred_matrix[, 1] <- apply(pred_matrix[, 1:main_estimate_models], 1, median, na.rm=T)
  pred_matrix <- pred_matrix[, c(1, (main_estimate_models+1):ncol(pred_matrix))]
}

colnames(pred_matrix) <- c("estimate", paste0("B", 1:ifelse(current_update_run == "A", floor(B/2), B)))
rownames(pred_matrix) <- 1:nrow(pred_matrix)

saveRDS(pred_matrix, "output-data/pred_matrix.RDS")

# 6. Generate exports ---------------------------------------

# Save covariates at weekly level:
dat <- readRDS("output-data/model-objects/dat.RDS")
covars_for_export_cols <- c("iso3c", "country", "date", "region", "subregion", "population", "median_age", "aged_65_older", "life_expectancy", "daily_covid_deaths_per_100k", "daily_covid_cases_per_100k", "daily_tests_per_100k", "cumulative_daily_covid_cases_per_100k", 
                            "cumulative_daily_covid_deaths_per_100k",
                            "cumulative_daily_tests_per_100k", "demography_adjusted_ifr",
                            "daily_covid_cases",
                            "daily_tests",
                            "daily_covid_deaths",
                            "daily_excess_deaths",
                            "daily_excess_deaths_per_100k")
covars_for_export <- dat[!duplicated(ids), covars_for_export_cols]
covars_for_export_latest <- dat[dat$date == max(dat$date), covars_for_export_cols]
covars_for_export_latest <- covars_for_export_latest[!duplicated(covars_for_export_latest$iso3c), ]

saveRDS(rbind(covars_for_export, covars_for_export_latest), "output-data/export_covariates.RDS")

# Clean workspace:
rm(covars_for_export)
rm(covars_for_export_latest)
rm(pred_matrix)
rm(dat)
rm(X)
gc()

# Get pre-update cumulative world total:
pre_updated_world_total <- read.csv('output-data/export_world_cumulative.csv')
pre_updated_world_total <- pre_updated_world_total[order(pre_updated_world_total$date, decreasing = T), c("cumulative_estimated_daily_excess_deaths", "cumulative_estimated_daily_excess_deaths_ci_95_top", "cumulative_estimated_daily_excess_deaths_ci_95_bot")][1, ]

# Run export script:
# For memory efficiency, this is also executed within a temporary local environment created by the update_export_1 and update_export_2 functions)
cat('\n\n Generate main exports.\n\n')
update_export_1 <- function(){source("scripts/3_excess_deaths_global_estimates_export.R", local = TRUE)}
update_export_1()

cat('\n\n Generate exports for interactive.\n\n')
update_export_2 <- function(){source("scripts/4_excess_deaths_global_estimates_export_for_interactive.R", local = TRUE)}
update_export_2()
cat('\n\n Exports completed, testing before push.\n\n')

# Compare pre and post-update world total:
post_updated_world_total <- read.csv('output-data/export_world_cumulative.csv')
post_updated_world_total <- post_updated_world_total[order(post_updated_world_total$date, decreasing = T), c("cumulative_estimated_daily_excess_deaths", "cumulative_estimated_daily_excess_deaths_ci_95_top", "cumulative_estimated_daily_excess_deaths_ci_95_bot")][1, ]

# If day-to-day difference is over 0.25m, throw an error to stop the automatic update. This notifies the maintainers, who can then ensure such large jumps are inspected manually before they are pushed to the live page.
if(abs(post_updated_world_total[1] - pre_updated_world_total[1]) > 500000 |
   abs(post_updated_world_total[2] - pre_updated_world_total[2]) > 350000 |
   abs(post_updated_world_total[3] - pre_updated_world_total[3]) > 500000){
  print("pre_updated_world_total")
  print(pre_updated_world_total)
  print("post_updated_world_total")
  print(post_updated_world_total)
  stop("Large change in cumulative world total, please inspect manually.")
}
                                 
# 7. Train a new bootstrap model ---------------------------------------
X <- readRDS('output-data/model-objects/X_train.RDS')
Y <- readRDS('output-data/model-objects/Y_train.RDS')
X$daily_excess_deaths_per_100k <- Y

# We first drop very recent observations (<21 days):
Y <- Y[!X$date > Sys.Date()-21]
X <- X[!X$date > Sys.Date()-21, ]

# We then load the model-generation loop function:
source('scripts/aux_generate_model_loop.R')

# Load list of previously updated models. This ensures that all models are eventually replaced:
recently_updated_models <- readRDS('output-data/model-objects/recently_updated_models.RDS')

# Select model to overwrite:
update <- sample(setdiff(1:(B+main_estimate_models), unlist(recently_updated_models)), 1)
recently_updated_models <- c(recently_updated_models, update)

# We then use this to generate one new bootstrap model, overwriting a random prior model:
cat('\n\n Re-training and replacing 1 model based on latest data.\n\n')
generate_model_loop(
  X_full = X[!is.na(Y), ], # Defines training set
  Y_full = Y[!is.na(Y)],   # Defines outcome variable
  B = 1, 
  include_main_estimate = T,
  main_estimate_learning_rate = 0.001,
  bootstrap_learning_rate = 0.003,
  custom_model_index = update,
  new_predictor_set = F
)
cat('\n\n One of 210 models successfully re-trained.\n\n')

# Save list of updated models, resetting to null if all updated:
if(length(recently_updated_models) < B+main_estimate_models){
    saveRDS(recently_updated_models, 'output-data/model-objects/recently_updated_models.RDS')
  } else {
    saveRDS(c(), 'output-data/model-objects/recently_updated_models.RDS')
  }

# Update output run:
if(current_update_run == "A"){ 
       saveRDS('B', 'output-data/model-objects/current_update_run.RDS')
  } else {
       saveRDS('A', 'output-data/model-objects/current_update_run.RDS')
  }
saveRDS(Sys.Date(), 'output-data/model-objects/latest_update.RDS')

end_time <- Sys.time()

print(paste("Total time:", end_time - start_time))
