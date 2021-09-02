# This function takes a data frame and vector of column names as its input, expanding categorical categories among these column names and converting them numerical columns:
expand_categorical <- function(dat,
                               column_names){
  
  # Expand categorical variables 
  categorical <- c()
  dat <- data.frame(dat)
  for(i in column_names){
    if(class(dat[, i]) == "character"){
      categorical <- c(categorical, i)
    }
  }
  for(i in categorical){
    dat[is.na(dat[, i]), i] <- paste0("Unknown ", i)
    temp <- model.matrix(~ . - 1, data = dat[, i, drop = F])
    column_names <- setdiff(c(column_names, colnames(temp)), i)
    dat <- cbind(dat, temp)
    dat[, i] <- NULL
  }
  
  # Convert all columns to numeric 
  for(i in column_names){
    dat[, i] <- as.numeric(dat[, i])
  }
  return(list(dat, column_names))}