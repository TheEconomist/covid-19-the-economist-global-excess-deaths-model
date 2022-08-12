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
X <- readRDS('output-data/model-objects/X_train.RDS')
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
for(i in load_predictions_model_set[(1+floor(length(load_predictions_model_set)/2)):length(load_predictions_model_set)]){
  counter <- counter + 1
  cat(paste("\n\nStarting prediction by model:", counter, "of", ifelse(current_update_run == "A", floor(B/2), B)+main_estimate_models, "at :\n", Sys.time(), "\n"))
  
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
  cat(paste("\nCompleted:", counter, "at : ", Sys.time(), "\n\n"))
  gc()
}
rm(X)

# Load and combine predictions from individual models:
for(i in 1:(ifelse(current_update_run == "A", floor(B/2), B)+main_estimate_models)){
  pred_matrix <- rbind(pred_matrix, readRDS(paste0('output-data/model-objects/model-predictions/model_', i , '_prediction.RDS')))
}

# Fix column and row names of prediction matrix:
pred_matrix <- t(pred_matrix)

# Combine main estimate models (with different seeds) via median
if(main_estimate_models > 1){
  pred_matrix[, 1] <- apply(pred_matrix[, 1:main_estimate_models], 1, mean, na.rm=T)
  pred_matrix <- pred_matrix[, c(1, (main_estimate_models+1):ncol(pred_matrix))]
}

colnames(pred_matrix) <- c("estimate", paste0("B", 1:ifelse(current_update_run == "A", floor(B/2), B)))
rownames(pred_matrix) <- 1:nrow(pred_matrix)

saveRDS(pred_matrix, "output-data/pred_matrix.RDS")
