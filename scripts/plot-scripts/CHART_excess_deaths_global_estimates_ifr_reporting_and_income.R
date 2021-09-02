# Load IFR data:

ifr <- read_csv("https://raw.githubusercontent.com/TheEconomist/covid-19-age-adjusted-ifr/main/ifr_by_iso2c.csv")
ifr$demography_adjusted_ifr <- ifr$area_ifr
library(countrycode)
ifr$iso3c <- countrycode(ifr$iso2c, "iso2c", "iso3c")


# Load income data:
library(WDI)
wdi <- WDI(country = 'all',
           indicator = c('wdi_prop_less_2_usd_day' = 'SI.POV.DDAY',
                         'wdi_gdppc_nominal' = 'NY.GDP.PCAP.CD',
                         'wdi_gdppc_ppp' = 'NY.GDP.PCAP.PP.CD',
                         'wdi_urban_population_pct' = 'SP.URB.TOTL.IN.ZS',
                         'wdi_urban_pop_1m_cities_pct' = 'EN.URB.MCTY.TL.ZS',
                         'wdi_gini_index' = 'SI.POV.GINI',
                         'wdi_life_expectancy_at_birth' = 'SP.DYN.LE00.IN',
                         'wdi_pop_over_65' = 'SP.POP.65UP.TO.ZS',
                         'wdi_pop_under_15' = 'SP.POP.0014.TO.ZS'))
wdi$iso3c <- countrycode(wdi$iso2c, "iso2c", "iso3c")
wdi$iso2c <- NULL
wdi$country <- NULL

# Only latest observation
wdi <- wdi[order(wdi$year), ]
wdi <- wdi[!is.na(wdi$iso3c), ]

for(i in setdiff(colnames(wdi), c("year", "iso3c"))){
  wdi[, i] <- ave(wdi[, i], wdi$iso3c, 
                  FUN = function(x){
                    if(max(which(!is.na(x))) == -Inf){
                      NA
                    } else {
                      x[max(which(!is.na(x)))]
                    }
                  })
}

# Collapse rows to one per country (multiple happens when data comes from different years)
for(i in setdiff(colnames(wdi), "iso3c")){
  wdi[, i] <- ave(wdi[, i], wdi$iso3c, FUN = function(x) mean(x, na.rm = T))
}
wdi <- unique(wdi)

# Merge the two
pdat <- merge(wdi, ifr, by = "iso3c")

# Load excess deaths data
export_covariates <- readRDS("output-data/export_covariates.RDS")
has_excess_deaths <- unique(export_covariates$iso3c[!is.na(export_covariates$daily_excess_deaths) & export_covariates$date >= as.Date("2021-01-01")])

pdat$has_excess_deaths_data_up_to_2021 <- pdat$iso3c %in% has_excess_deaths

has_excess_deaths <- unique(export_covariates$iso3c[!is.na(export_covariates$daily_excess_deaths)])

pdat$has_any_excess_deaths_data <- pdat$iso3c %in% has_excess_deaths


# Plot:
library(ggplot2)
ggplot(pdat, aes(x=wdi_gdppc_ppp, y=demography_adjusted_ifr, col = has_any_excess_deaths_data))+
  geom_point()+scale_x_continuous(trans = "log10")+
  ylim(c(0, 1.5))+theme_minimal()

write_csv(pdat, "ifr_vs_gdp_vs_excess_deaths_data.csv")
