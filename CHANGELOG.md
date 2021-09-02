## Sep 2nd 2021
- Changed all data sources to update daily where applicable.
- Tweaked dimensionality reduction of missingness indicators, removing possibility of the column order and dimensionality changing between training and prediction steps as a result of previously complete data ceasing to be so.
- Greatly expanded serosurveys featured, added split to last two months of seroprevalence estimates to account for sero-survey to publication lag. Added 295 new seroprevalence estimates, expanding the sample to 420 surveys in 51 countries (previously 32).
- Added cumulative regional and national seroprevalence indicators.
- Greatly expanded subnational data, adding in all areas with reported total mortality figures for the last 3 years, and populations over 1m present in the Local Mortality dataset as of July 2021. These were all manually matched to subnational figures on covid deaths, cases, figures, mobility data, and geography.
- Added mean elevation, percent of population in the tropics and other geographical country-level variables (Source: John L. Gallup; Andrew D. Mellinger; Jeffrey D. Sachs, 2010, "Geography Datasets").
- Added tuberculosis, HIV/AIDS, malaria, and projected total death burden data (Source: WHO).
- Added temperature data based on population-weighted average by month and country 2015-2019 (Source: Copernicus Climate Service; Oikalabs).
- Set distance-weighted averages to be log-population-weighted.
- Adjusted Chinese reported excess deaths for mortality increases over time based on UN pre-pandemic projections.
- Manually inspected all excess deaths series for reporting lag-driven declines in mortality, censoring as applicable based on reporting source (this meant removing very recent American excess deaths data from the model fitting stage, based on CDC estimates of likely reporting lags). All excess deaths data remain reported and part of estimates, this only affected the model-fitting stage.
- Removed countries (e.g. Peru) who have back-ward adjusted their covid-19 death figures to match excess mortality estimates from the model-fitting stage (as current covid deaths there are not based on excess deaths). Also removed these countries covid-19 death tallies from relevant regional and distance-weighted averages.
- Feature-engineering to include covid deaths interacted with vaccination data and population over 65 to facilitate model learning. Also added two-week lagged variables of vaccination indicators to account for time-lag in their effectiveness.
- Adjusted bootstrapping step to sample strata then observations within them, rather than drawing one strata then observations within it iteratively until sample size approached original data. Increased bootstrap iterations to 200.

## May 2020

Initial release.