library(tidyverse)
library(feather)

####pre-loading in data####

#census
income_bands <- read_feather("income_bands.feather")
med_mean_income <- read_feather("med_mean_income.feather")
race_eth <- read_feather("race_eth.feather")
hh_char <- read_feather("hh_char.feather")

valid_zips <- unique(med_mean_income$Zipcode)

#Zillow 
home_val <- read_feather("home_val.feather")
growth_home <- read_feather("growth_home.feather")