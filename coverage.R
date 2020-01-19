# This script reads kaggle files and runs the machine learning

rm(list=ls(all=TRUE)) 
cat("\014")  
#dev.off()

library(tidyverse)
library(readxl)

getwd()
setwd("C:/Users/workhome/Dropbox (Analysys Mason AS)/2020-QGIS/data/nkom-dekning")



# Read in files & fix colums
# Read in NKOM Dekning
setwd("C:/Users/workhome/Dropbox (Analysys Mason AS)/2020-QGIS/data/nkom-dekning")
dat_nkom <- read_delim("nkom_med_tett.csv",delim = ",", locale = locale(decimal_mark = "."))

# Fix parsing error and translate variables
dat_nkom <- dat_nkom %>%
  mutate(tettsted2 = coalesce(tettsted, FALSE)) %>%
  select(- tettsted) %>%
  rename(urban = tettsted2, muni_nr = kommune_nr, building_nr = bygnings_nr,
         homes = boliger)

# read municipal information, translate variables, remove unneeded columns
setwd("C:/Users/workhome/Dropbox (Analysys Mason AS)/2020-QGIS/data/kommuneinfo/2018")
dat_muni <- read_delim("2018_muni_all.csv",delim = ",", locale = locale(decimal_mark = "."))

dat_muni <- dat_muni %>% 
  rename(muni_nr = kommune_nr, muni_homes = edr_homes) %>% 
  subset(select = -c(fylke, kommune))


# merge into one dataset
dat_all <- left_join(dat_nkom, dat_muni, by = "muni_nr")

## data wrangling - check - test for outliers

# make training set



# train model





# test model








