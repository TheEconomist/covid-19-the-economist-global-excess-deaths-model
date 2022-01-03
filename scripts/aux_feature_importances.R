# Analyze feature importance

# 1. Generate CSV export
# Import files:
m <- readRDS('output-data/model-objects/importance_matrix.RDS')
m_predictors <- readRDS('output-data/model-objects/m_predictors.RDS')

# Generate container matrix
imp <- data.frame("feature" = m_predictors, "models_used_by_n" = rep(NA, length(m_predictors)))

# Combine features into matrix from exported list (which extracted feature importance from the main model in 1, then the 200 bootstraps in 2:200) 
for(i in 1:length(m)){
  temp <- m[[i]]
  temp <- data.frame(feature = names(temp),
                     value = as.numeric(temp))
  colnames(temp) <- c('feature', paste0('value_', i))
  rownames(temp) <- 1:nrow(temp)
  imp <- merge(imp, temp, by = 'feature', all.x= T)
}

# Set NAs to 0
imp[imp == NA] <- 0

# Create containers for mean, median and standard deviations of feature importance
imp$mean_imp <- NA
imp$median_imp <- NA
imp$sd_imp <- NA

# Calculate these
for(i in 1:nrow(imp)){
  imp$models_used_by_n[i] <- sum(!as.numeric(imp[i, 3:(ncol(imp)-3)]) == 0, na.rm = T)
  imp$mean_imp[i] <- mean(as.numeric(imp[i, 3:(ncol(imp)-3)]), na.rm = T)
  imp$median_imp[i] <- median(as.numeric(imp[i, 3:(ncol(imp)-3)]), na.rm = T)
  imp$sd_imp <- sd(as.numeric(imp[i, 3:(ncol(imp)-3)]))
}

# Rename columns
colnames(imp)[3:203] <- c('main estimate model', paste0('bootstrap_', 1:200))

# Inspect if desired
# View(imp[, c(1:3, 204:206)])

# Export to file
write.csv(imp, 'output-data/model-objects/importance_matrix.csv')

# 2. Plot importances
library(tidyverse)
# Reshape to long format
pdat <- imp %>% pivot_longer(!colnames(imp)[c(1,2,204, 205, 206)])

# Set factor levels
pdat <- pdat[order(pdat$mean_imp, decreasing = F), ]
pdat$feature <- factor(pdat$feature, levels = unique(pdat$feature))

# Plot using ggplot
library(ggplot2)
ggplot(pdat[pdat$mean_imp > 0.1, ], aes(x=value, y=feature))+geom_violin()+geom_point(data = pdat[pdat$mean_imp > 1 & pdat$name == 'main estimate model', ], aes(col = 'central estimate model'))+theme(legend.pos = 'bot')
