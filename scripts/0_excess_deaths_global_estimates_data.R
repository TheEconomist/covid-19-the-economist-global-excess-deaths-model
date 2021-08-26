# Step 1: import libraries ---------------------------------------

# Import libraries
library(tidyverse)
library(data.table)
library(readr)
library(readxl)
library(lubridate)
options(scipen=999)

##For the sake of repoducibility this file will save copies of the datasets used
#from the web in the source-data\raw folder. The data will be pre-processed here
file_path <- file.path(here::here(), "source-data")


## Import excess deaths from economist:
all_weekly_excess_deaths <- fread("https://raw.githubusercontent.com/TheEconomist/covid-19-excess-deaths-tracker/master/output-data/excess-deaths/all_weekly_excess_deaths.csv")
all_monthly_excess_deaths <- fread("https://raw.githubusercontent.com/TheEconomist/covid-19-excess-deaths-tracker/master/output-data/excess-deaths/all_monthly_excess_deaths.csv")
all_quarterly_excess_deaths <- fread("https://raw.githubusercontent.com/TheEconomist/covid-19-excess-deaths-tracker/master/output-data/excess-deaths/all_quarterly_excess_deaths.csv")
#save:
write_csv(all_weekly_excess_deaths,
          file.path(
            file_path, "all_weekly_excess_deaths.csv"
          ))
write_csv(all_monthly_excess_deaths,
          file.path(
            file_path, "all_monthly_excess_deaths.csv"
          ))
write_csv(all_quarterly_excess_deaths,
          file.path(
            file_path, "all_quarterly_excess_deaths.csv"
          ))

##Download OWID data:
country_daily_data <- fread("https://raw.githubusercontent.com/owid/covid-19-data/master/public/data/owid-covid-data.csv")
#save
write_csv(country_daily_data,
          file.path(
            file_path, "country_daily_data.csv"
          ))

##Vdem data:
#load from package
library(vdemdata)
vdem <- vdemdata::vdem
#limit to last 10 years to save space
vdem <- filter(vdem, year > 2010)
#save
write_csv(vdem,
          file.path(
            file_path, "vdem.csv"
          ))

##Democracy Binary data:
#from: https://dataverse.harvard.edu/dataset.xhtml?persistentId=doi:10.7910/DVN/FJLMKT
democracy_binary <- fread("https://dataverse.harvard.edu/api/access/datafile/:persistentId?persistentId=doi:10.7910/DVN/FJLMKT/UQPATJ")
#save
write_csv(democracy_binary,
          file.path(
            file_path, "democracy_binary.csv"
          ))

##Import freedom house:
#(source: https://freedomhouse.org/report/freedom-world)
#download xlsx files
download.file("https://freedomhouse.org/sites/default/files/2021-02/All_data_FIW_2013-2021.xlsx",
              file.path(file_path, "freedom_house.xlsx"),
              mode="wb")
#open file
freedom_house <- read_xlsx("source-data/freedom_house.xlsx", sheet = 2, skip = 1)
#save
write_csv(freedom_house,
          file.path(
            file_path, "freedom_house.csv"
          ))

##Import centre for systemic peace data:
#source: https://www.systemicpeace.org/polityproject.html
#download
download.file("http://www.systemicpeace.org/inscr/p5v2018.xls",
              file.path(here::here(), "source-data/polity.xls"),
              mode="wb")
#read
polity <- read_xls("source-data/polity.xls")
#save:
write_csv(polity,
          file.path(
            file_path, "polity.csv"
          ))

##Get WDI data from package:
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
#save
write_csv(wdi,
          file.path(
            file_path, "wdi.csv"
          ))

#Import coordinates of capital cities and largest cities:
#source:https://simplemaps.com/data/world-cities
download.file("https://simplemaps.com/static/data/world-cities/basic/simplemaps_worldcities_basicv1.74.zip",
              file.path(here::here(), "source-data/world.cities.zip"),
              mode="wb")
unzip(file.path(here::here(), "source-data/world.cities.zip"),
      files = "worldcities.csv",
      exdir = file.path(here::here(), "source-data/"))
#already csv so we'll leave 

##Import Economist IFR data
#source:https://www.economist.com/graphic-detail/2020/11/16/why-rich-countries-are-so-vulnerable-to-covid-19 and github: https://github.com/TheEconomist/covid-19-age-adjusted-ifr
ifr <- read_csv("https://raw.githubusercontent.com/TheEconomist/covid-19-age-adjusted-ifr/main/ifr_by_iso2c.csv")
#save
write_csv(ifr,
          file.path(
            file_path, "ifr.csv"
          ))

##Tourism Arrivals
#source: https://www.unwto.org/statistic/basic-tourism-statistics
download.file("https://webunwto.s3.eu-west-1.amazonaws.com/s3fs-public/2020-10/Arrivals-1995-2019.xlsx",
              file.path(here::here(), "source-data/unwto.xlsx"),
              mode="wb")
unwto <- read_xlsx(file.path(here::here(), "source-data/unwto.xlsx"), skip =2)
#save
write_csv(unwto,
          file.path(
            file_path, "unwto.csv"
          ))

###Time Varying Data:
##Google Mobility reports:
mob <- fread("https://www.gstatic.com/covid19/mobility/Global_Mobility_Report.csv")
#Restrict to country-level data to save on file size
mob <- mob %>%
  filter(
    sub_region_1 == "",
    sub_region_2 == "",
    metro_area == ""
  ) %>%
  unique()
#save
write_csv(mob,
          file.path(
            file_path, "mob.csv"
          ))

##Policy response data
ox <- data.frame(fread("https://raw.githubusercontent.com/OxCGRT/covid-policy-tracker/master/data/OxCGRT_latest.csv"))
#save
write_csv(ox,
          file.path(
            file_path, "ox.csv"
          ))

##Import seroprevalence data:
#source: https://serotracker.com/en/Explore
#predownloaded atm, working on programmatic access

##Import economist standard names
#Source: Don't know came with repo

###Additional Non-repo Excess Deaths
##China:
#source: https://www.bmj.com/content/372/bmj.n415
library(tabulizer)
#scrap from pdf, requires rJava
china <- extract_tables("https://www.bmj.com/highwire/filestream/1043710/field_highwire_adjunct_files/0/liuj061031.ww.pdf",
                 pages = 10, method = "stream", output = "data.frame")[[1]] %>%
  #drop the first rows because they are the titles
  filter(!(X %in% c("", "Week"))) %>%
  #split up large characters
  rowwise() %>%
  mutate(
    `Mean No. of reported Deaths, 2015-19` = as.numeric(str_split(Wuhan.DSP..n.3., " ")[[1]][1]),
    `Delay adjustment ratio (%)*` = as.numeric(str_split(Wuhan.DSP..n.3., " ")[[1]][2]),
    `No. of reported deaths in 2020` = as.numeric(str_split(Wuhan.DSP..n.3., " ")[[1]][3]),
    
    `Mean No. of reported Deaths, 2015-19_1` = as.numeric(str_split(Hubei.without.Wuhan..n.19., " ")[[1]][1]),
    `Delay adjustment ratio (%) *` = as.numeric(str_split(Hubei.without.Wuhan..n.19., " ")[[1]][2]),
    `No. of reported deaths in 2020_1` = as.numeric(str_split(Hubei.without.Wuhan..n.19., " ")[[1]][3]),
    
    `Mean No. of reported Deaths, 2015-19_2` = as.numeric(str_split(China.without.Hubei..n.583., " ")[[1]][1]),
    `Delay adjustment ratio (%)* *` = as.numeric(str_split(China.without.Hubei..n.583., " ")[[1]][2]),
    `No. of reported deaths in 2020_2` = as.numeric(str_split(China.without.Hubei..n.583., " ")[[1]][3])
  ) %>%
  rename(Week = X,
         `Starting date of the week` = X.1) %>%
  select(!c(X.2, Wuhan.DSP..n.3., Hubei.without.Wuhan..n.19., China.without.Hubei..n.583.))
#save
write_csv(china,
          file.path(
            file_path, "china.csv"
          ))
##India
#source: from https://portal.mcgm.gov.in/irj/portal/anonymous/qlvitalstatsreport
# and https://health.kerala.gov.in/pdf/Technical-paper-All-Cause-Mortality-Kerala.pdf
#which won't be updated til mid year no just leave as csv as originally

##Distance dyads
#source:http://www.cepii.fr/cepii/en/bdd_modele/presentation.asp?id=6
download.file("http://www.cepii.fr/distance/dist_cepii.zip",
              file.path(
                file_path, "dist_dy.zip"
              ),
              mode="wb"
              )
unzip(file.path(
  file_path, "dist_dy.zip"
),
files = "dist_cepii.xls",
exdir = file_path)
#load
dist_dy <- read_xls(
  file.path(
    file_path, "dist_cepii.xls"
  )
)
#save
write_csv(dist_dy,
          file.path(
            file_path, "dist_dy.csv"
          ))
