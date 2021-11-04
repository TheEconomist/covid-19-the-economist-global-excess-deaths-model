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

# Impute missing data (using min-impute coupled with one-hot encoding of NA locations) 

# 1. Impute missing:
source("scripts/shared-functions/impute_missing.R")
X <- impute_missing(X[order(X$date), c(predictors, "iso3c")], cached_NA_cols = T)

# 2. Make NA indicators either 0 or 1
for(i in grep("NA_matrix", colnames(X))){
  X[X[, i] != 1, i] <- 0  
}

# 5. Load models and populate prediction matrix --------------------------------------- 

# Clean workspace for memory efficiency
rm(pdat)
rm(temp)
rm(df)

# Create container matrix for predictions
pred_matrix <- data.frame()

# Load model (= estimate) and bootstrap predictions 
library(agtboost)

# Load list of model predictors
m_predictors <- readRDS("output-data/model-objects/m_predictors.RDS")

# Define number of bootstrap iterations. We use 200.
B = 200
counter = -1

# Select predictors and create predictor matrix
X <- as.matrix(X[, m_predictors])

# Loop over bootstrap iterations
for(i in 1:(B+1)){
  counter = counter + 1
  cat(paste("\n\nStarting B:", counter, "at : ", Sys.time(), "\n\n"))
  
  # Load model object
  gbt_model <- gbt.load(paste0("output-data/model-objects/gbt_model_B_", i, ".agtb"))
  
  # Save model predictions
  preds <- predict(gbt_model, newdata = X)
  pred_matrix <- rbind(pred_matrix, preds)
  rm(gbt_model)
  rm(preds)

  cat(paste("\nCompleted B:", counter, "at : ", Sys.time(), "\n\n"))
}

# Fix column and row names of prediction matrix:
pred_matrix <- t(pred_matrix)
colnames(pred_matrix) <- c("estimate", paste0("B", 1:B))
rownames(pred_matrix) <- 1:nrow(pred_matrix)

saveRDS(pred_matrix, "output-data/pred_matrix.RDS")

# 6. Generate exports ---------------------------------------

# Save covariates at weekly level:
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

# Get pre-update cumulative world total:
pre_updated_world_total <- read.csv('output-data/export_world_cumulative.csv')
pre_updated_world_total <- pre_updated_world_total[order(pre_updated_world_total$date, decreasing = T), "cumulative_estimated_daily_excess_deaths"][1]

# Run export script:
source("scripts/3_excess_deaths_global_estimates_export.R")
source("scripts/4_excess_deaths_global_estimates_export_for_interactive.R")

# Compare pre and post-update world total:
post_updated_world_total <- read.csv('output-data/export_world_cumulative.csv')
post_updated_world_total <- post_updated_world_total[order(post_updated_world_total$date, decreasing = T), "cumulative_estimated_daily_excess_deaths"][1]

# If day-to-day difference is over 0.25m, throw an error to stop the automatic update. This notifies the maintainers, who can then ensure such large jumps are inspected manually before they are pushed to the live page.
if(abs(post_updated_world_total - pre_updated_world_total) > 250000){
  stop("Large change in cumulative world total, please inspect manually.")
}

end_time <- Sys.time()

print(paste("Total time:", end_time - start_time))
