#### Master Script  ---------------------------------------

# Import libraries
library(ggplot2)
library(tidyverse)
library(data.table)

options(scipen=999)

# Set warning log container
log <- c()

start_time <- Sys.time()
saveRDS(start_time, 'output-data/model-objects/start.RDS')

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
saveRDS(ids, 'output-data/model-objects/ids.RDS')
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

# Save outputs
saveRDS(X, 'output-data/model-objects/X_train.RDS')
saveRDS(Y, 'output-data/model-objects/Y_train.RDS')
saveRDS(dat, "output-data/model-objects/dat.RDS")
