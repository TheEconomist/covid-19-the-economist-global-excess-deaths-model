# 6. Generate exports ---------------------------------------

# Save covariates at weekly level:
ids <- readRDS("output-data/model-objects/ids.RDS")
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

# Get pre-update cumulative world total:
pre_updated_world_total <- read.csv('output-data/export_world_cumulative.csv')
pre_updated_world_total <- pre_updated_world_total[order(pre_updated_world_total$date, decreasing = T), c("cumulative_estimated_daily_excess_deaths", "cumulative_estimated_daily_excess_deaths_ci_95_top", "cumulative_estimated_daily_excess_deaths_ci_95_bot")][1, ]

# Run export script:
# For memory efficiency, this is also executed within a temporary local environment created by the update_export_1 and update_export_2 functions)
library(ggplot2)
cat('\n\n Generate main exports.\n\n')
update_export_1 <- function(){source("scripts/3_excess_deaths_global_estimates_export.R", local = TRUE)}
update_export_1()
gc()

cat('\n\n Generate exports for interactive.\n\n')
update_export_2 <- function(){source("scripts/4_excess_deaths_global_estimates_export_for_interactive.R", local = TRUE)}
update_export_2()
gc()
cat('\n\n Exports completed, testing before push.\n\n')

# Compare pre and post-update world total:
post_updated_world_total <- read.csv('output-data/export_world_cumulative.csv')
post_updated_world_total <- post_updated_world_total[order(post_updated_world_total$date, decreasing = T), c("cumulative_estimated_daily_excess_deaths", "cumulative_estimated_daily_excess_deaths_ci_95_top", "cumulative_estimated_daily_excess_deaths_ci_95_bot")][1, ]

# If day-to-day difference is over 0.25m, throw an error to stop the automatic update. This notifies the maintainers, who can then ensure such large jumps are inspected manually before they are pushed to the live page.
if(abs(post_updated_world_total[1] - pre_updated_world_total[1]) > 350000 |
   abs(post_updated_world_total[2] - pre_updated_world_total[2]) > 1500000 |
   abs(post_updated_world_total[3] - pre_updated_world_total[3]) > 1500000){
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
B <- readRDS("output-data/model-objects/B.RDS")
main_estimate_models <- readRDS("output-data/model-objects/main_estimate_models_n.RDS")

update <- sample(setdiff(1:(B+main_estimate_models), unlist(recently_updated_models)), 1)

# Temporary: prioritize re-fit of main-estimate models:
update <- sample(setdiff(1:main_estimate_models, unlist(recently_updated_models)), 1)

recently_updated_models <- c(recently_updated_models, update)

# We then use this to generate one new bootstrap model, overwriting a random prior model:
cat('\n\n Re-training and replacing 1 model based on latest data.\n\n')
generate_model_loop(
  X_full = X[!is.na(Y), ], # Defines training set
  Y_full = Y[!is.na(Y)],   # Defines outcome variable
  B = 1, 
  include_main_estimate = T,
  main_estimate_learning_rate = 0.0009,
  bootstrap_learning_rate = 0.003,
  custom_model_index = update,
  new_predictor_set = F
)
cat('\n\n One of 210 models successfully re-trained.\n\n')

# If desired, uncomment line below to force additional update of main estimate models:
# recently_updated_models <- setdiff(recently_updated_models, 1:10)

# Save list of updated models, resetting to null if all updated:

if(length(recently_updated_models) < B+main_estimate_models){
  saveRDS(recently_updated_models, 'output-data/model-objects/recently_updated_models.RDS')
} else {
  saveRDS(c(), 'output-data/model-objects/recently_updated_models.RDS')
}

# Update output run:
if(readRDS('output-data/model-objects/current_update_run.RDS') == "A"){ 
  saveRDS('B', 'output-data/model-objects/current_update_run.RDS')
} else {
  saveRDS('A', 'output-data/model-objects/current_update_run.RDS')
}
saveRDS(Sys.Date(), 'output-data/model-objects/latest_update.RDS')

end_time <- Sys.time()

print(paste("Total time:", end_time - readRDS('output-data/model-objects/start.RDS')))
