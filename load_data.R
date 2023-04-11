library(tidyverse)
library(feather)

####pre-loading in data####

#census
income_bands <- read_feather("income_bands.feather")
income_bands_place <- read_feather("acs_income_bands_place.feather")

race_income_bands_zip <- read_feather("race_income_bands_zip.feather")
race_income_bands_place <- read_feather("race_income_bands_place.feather")

med_mean_income <- read_feather("med_mean_income.feather")
med_mean_income_place <- read_feather("med_mean_income_place.feather")

race_eth <- read_feather("race_eth.feather")
race_eth_place <- read_feather("race_eth_place.feather")

hh_char <- read_feather("hh_char.feather")
hh_char_place <- read_feather("hh_char_place.feather")

rent_own_zip <- read_feather("rent_own_zip.feather")
rent_own_place <- read_feather("rent_own_zip_place.feather")

#valid geographies 
valid_zips <- unique(med_mean_income$Zipcode)
city_state_list <- read_csv("city_state_list.csv")

#Zillow 
home_val <- read_feather("home_val.feather")
growth_home <- read_feather("growth_home.feather")
home_val_five <- read_feather("home_val_year_five.feather")
home_avg_five <- read_feather("home_avg_five.feather")