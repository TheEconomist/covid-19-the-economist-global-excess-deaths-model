# This script defines a function to generate population-weighted gradient boosted tree models for a given X training matrix and Y outcome vector. By default, it generates a main estimate using all the data, and 200 models based on bootstrap samples. The custom_model_index allows specification of the name used when saving the model. The new_predictor_set parameter decided if the current run should update the list of predictors (saved/loaded at "output-data/model-objects/m_predictors.RDS").

generate_model_loop <- function(X_full = X[!is.na(Y), ],
                                Y_full = Y[!is.na(Y)], 
                                 B = 200, 
                                 include_main_estimate = T,
                                 main_estimate_model_n = 10,
                                 main_estimate_learning_rate = 0.001,
                                 bootstrap_learning_rate = 0.003,
                                 custom_model_index,
                                new_predictor_set = T){

  # Define weights, using half-values for subnational units to reflect greater uncertainty in their covariates and estimates. That value was selected based on correlations between covid-deaths and excess deaths in these units (which was much weaker - these also had fewer non-NA observations):
  X_full$weights <- log(X_full$population)
  X_full$weights[nchar(X_full$iso3c) > 3] <- X_full$weights[nchar(X_full$iso3c) > 3]/2
  
  #  Dividing weights if multiple subunits for one country (this matters because observations are weighted by log population rather than absolute population):
  for(i in unique(substr(X_full$iso3c, 1, 3))){
    n_units <- length(unique(X_full$iso3c[substr(X_full$iso3c, 1, 3) == i & !is.na(X_full$daily_excess_deaths_per_100k)]))
    if(n_units > 1){
      X_full$weights[substr(X_full$iso3c, 1, 3) == i] <-     X_full$weights[substr(X_full$iso3c, 1, 3) == i]/n_units
    }
  }

  # Create container matrix for predictions
  pred_matrix <- data.frame()
  
  if(new_predictor_set){
    # Define predictors
    m_predictors <- setdiff(colnames(X_full), c("iso3c", "region", "weights", "date"))
    
    # Save these predictor names
    saveRDS(m_predictors, "output-data/model-objects/m_predictors.RDS")
  } else {
    m_predictors <- readRDS("output-data/model-objects/m_predictors.RDS")
  }
  
  # Generate model (= estimate) and bootstrap predictions 
  library(agtboost)
  
  # Loop over bootstrap iterations (and main estimate if requested)
  for(i in 1:(B+include_main_estimate*main_estimate_model_n)){
    cat(paste("\n\nStarting B:", i, "at : ", Sys.time(), "\n\n"))
    
    # Container for row indicies
    obs <- c()
    
    # Select observations for bootstrap (stratified)
    if(i %in% 1:main_estimate_model_n & include_main_estimate){
      # First fit is estimation (i.e. no random sampling of data) if main estimate requested
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
    
    lr_temp <- ifelse(i %in% 1:main_estimate_model_n & include_main_estimate, 
                      main_estimate_learning_rate, 
                      bootstrap_learning_rate)
    
    # Fit model:
    gbt_model <- gbt.train(Y_temp, 
                           X_temp, 
                           learning_rate = lr_temp,
                           nrounds = 35000,
                           verbose = 200,
                           weights = weights_temp,
                           algorithm = "global_subset")
    
    # Save model objects
    if(!missing(custom_model_index)){
      gbt.save(gbt_model, paste0("output-data/model-objects/gbt_model_B_", custom_model_index, ".agtb"))
    } else {
    gbt.save(gbt_model, paste0("output-data/model-objects/gbt_model_B_", ifelse(include_main_estimate, i, i+1), ".agtb"))
    }
    cat(paste("\nCompleted B:", i, "at : ", Sys.time(), "\n\n"))
    
  }
}
