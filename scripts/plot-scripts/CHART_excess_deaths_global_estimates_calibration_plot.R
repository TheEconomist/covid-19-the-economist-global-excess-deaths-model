# Calibration plot
library(readr)
library(ggplot2)

# Load results of 10-fold cross-validation (with predictions on hold-out set)
results <- read_csv("output-data/calibration_plot_gradient_booster.csv")
# Note: see model script for replication code and details.

# This plots calibration (regions in different colors)
ggplot(results,
       aes(x=pred, y=truth))+
  geom_point()+
  geom_abline(aes(slope = 1, intercept = 0))+
  geom_smooth(mapping = aes(weight = results$w, group = "1"), method = 'lm')+theme_minimal()+ggtitle("Excess deaths per 100k population, actual vs model prediction*\n45 degree line = optimal calibration")+xlab("Predicted excess deaths, per 100k population\n\n*On 'unseen countries' - models created without data on the countries for which they were asked to predict")+ylab("Observed excess deaths, per 100k population")

