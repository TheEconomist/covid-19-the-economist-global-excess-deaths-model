# This function takes a data frame as its input, and uses min-impute or mean-impute to replace NAs.  
impute_missing <- function(X = dat,
                           method = "min-impute",
                           replace.inf = T){
  
  # Transform to data table
  library(data.table)
  X <- data.table(X)
  
  # Find and replace Inf values
  invisible(lapply(names(X),function(.name) set(X, which(is.infinite(X[[.name]])), j = .name,value =NA)))
  X <- data.frame(X)
  
  # Find columns with missing values
  na_cols <- unlist(lapply(1:ncol(X), FUN = function(i){any(is.na(X[, i]))}))
  n <- nrow(X)
  
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

  # This section had to be commented out. It reduces the dimensionality of the NA matrix by removing 1-to-1 correlated columns. The reason was that we otherwise ended up with the possibility of new missingness patterns changing the NA_matrix_col column order, making the prediction matrix have a different column order and dimensionality than the model training matrix. This would only happen if a column in dynamically updated data had a missing observation but in the training data did not have any missing data, so a low probability event, but we do not want to take that risk. It will affect model fitting time, but should not affect predictions themselves (as these matrices are correlated 1 to 1 unless the aforementioned happens). 
  #
  # XNA <- t(unique(t(XNA)))
  #
  # 
  # reduce XNA
  # ind <- 1
  # while(ind < ncol(XNA))
  # {
  #   equal <- rep(FALSE, ncol(XNA)-ind)
  #   for(j in (ind+1):ncol(XNA))
  #   {
  #     equal[j-ind] <- all(XNA[,ind] == XNA[,j])
  #   }
  #   sum(equal)
  #   
  #   XNA <- XNA[,!c( rep(F, ind), equal), drop=F]
  #   ind = ind + 1
  # }
  # 
  colnames(XNA) <- paste0("NA_matrix_col", 1:ncol(XNA))
  
  class(XNA) <- "numeric"
  
  # Bind X and 1-hot of XNA
  dX <- cbind(X, XNA)
  
  return(dX)}
