# Calibration plot
library(readr)
library(ggplot2)
library(dplyr)

# Load results of 10-fold cross-validation (with predictions on hold-out set)
results <- read_csv("output-data/calibration_plot_gradient_booster.csv")
# Note: see model script for replication code and details.

# This plots calibration (regions in different colors)
cal_plot <- ggplot(results,
       aes(x=pred, y=truth, alpha = w))+
  geom_point()+
  geom_abline(aes(slope = 1, intercept = 0))+
  geom_smooth(mapping = aes(weight = results$w, group = "1"), method = 'lm')+
  theme_minimal()+
  ggtitle("Excess deaths per 100k population, actual vs model prediction*\n45 degree line = optimal calibration")+
  xlab("Predicted excess deaths, per 100k population\n\n*On 'unseen countries' - models created without data on the countries for which they were asked to predict")+
  ylab("Observed excess deaths, per 100k population") + 
  theme(legend.position = "none")

#Generate a large pdf to check fits of all plots

results <- cbind(
  readRDS("output-data/export_covariates.RDS"),
  readRDS("output-data/pred_matrix.RDS")
  )

#plot by region

#Latin America & Caribbean
cal_plot_1 <- ggplot(results %>% filter(region == "Latin America & Caribbean")) + 
  geom_line(aes(
    x = date,
    y = estimate
  ),
  colour = "black",
  linetype = "dashed") +
  geom_line(aes(
    x = date,
    y = daily_excess_deaths_per_100k
  ),
  colour = "black") +
  geom_line(aes(
    x = date,
    y = daily_covid_deaths_per_100k
  ),
  colour = "red") + 
  facet_wrap(facets = vars(iso3c), scales = "free_y")

#South Asia
cal_plot_2 <- ggplot(results %>% filter(region == "South Asia")) + 
  geom_line(aes(
    x = date,
    y = estimate
  ),
  colour = "black",
  linetype = "dashed") +
  geom_line(aes(
    x = date,
    y = daily_excess_deaths_per_100k
  ),
  colour = "black") +
  geom_line(aes(
    x = date,
    y = daily_covid_deaths_per_100k
  ),
  colour = "red") + 
  facet_wrap(facets = vars(iso3c), scales = "free_y")

#Sub-Saharan Africa
cal_plot_3 <- ggplot(results %>% filter(region == "Sub-Saharan Africa")) + 
  geom_line(aes(
    x = date,
    y = estimate
  ),
  colour = "black",
  linetype = "dashed") +
  geom_line(aes(
    x = date,
    y = daily_excess_deaths_per_100k
  ),
  colour = "black") +
  geom_line(aes(
    x = date,
    y = daily_covid_deaths_per_100k
  ),
  colour = "red") + 
  facet_wrap(facets = vars(iso3c), scales = "free_y")

#Europe & Central Asia
cal_plot_4 <- ggplot(results %>% filter(region == "Europe & Central Asia")) + 
  geom_line(aes(
    x = date,
    y = estimate
  ),
  colour = "black",
  linetype = "dashed") +
  geom_line(aes(
    x = date,
    y = daily_excess_deaths_per_100k
  ),
  colour = "black") +
  geom_line(aes(
    x = date,
    y = daily_covid_deaths_per_100k
  ),
  colour = "red") + 
  facet_wrap(facets = vars(iso3c), scales = "free_y")

#Middle East & North Africa
cal_plot_5 <- ggplot(results %>% filter(region == "Middle East & North Africa")) + 
  geom_line(aes(
    x = date,
    y = estimate
  ),
  colour = "black",
  linetype = "dashed") +
  geom_line(aes(
    x = date,
    y = daily_excess_deaths_per_100k
  ),
  colour = "black") +
  geom_line(aes(
    x = date,
    y = daily_covid_deaths_per_100k
  ),
  colour = "red") + 
  facet_wrap(facets = vars(iso3c), scales = "free_y")

#East Asia & Pacific
cal_plot_6 <- ggplot(results %>% filter(region == "East Asia & Pacific")) + 
  geom_line(aes(
    x = date,
    y = estimate
  ),
  colour = "black",
  linetype = "dashed") +
  geom_line(aes(
    x = date,
    y = daily_excess_deaths_per_100k
  ),
  colour = "black") +
  geom_line(aes(
    x = date,
    y = daily_covid_deaths_per_100k
  ),
  colour = "red") + 
  facet_wrap(facets = vars(iso3c), scales = "free_y")

#North America
cal_plot_7 <- ggplot(results %>% filter(region == "North America")) + 
  geom_line(aes(
    x = date,
    y = estimate
  ),
  colour = "black",
  linetype = "dashed") +
  geom_line(aes(
    x = date,
    y = daily_excess_deaths_per_100k
  ),
  colour = "black") +
  geom_line(aes(
    x = date,
    y = daily_covid_deaths_per_100k
  ),
  colour = "red") + 
  facet_wrap(facets = vars(iso3c), scales = "free_y")

#save to pdf
pdf("output-data/calibration/plots.pdf")
cal_plot
cal_plot_1
cal_plot_2
cal_plot_3
cal_plot_4
cal_plot_5
cal_plot_6
cal_plot_7
dev.off()