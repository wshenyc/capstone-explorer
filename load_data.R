library(tidyverse)
library(feather)

####pre-loading in data####
# income_bands <- read_csv("acs_income_bands_2017_2021.csv")
# med_mean_income <- read_csv("med_mean_income_acs_2017_2021.csv")
#race_eth <- read_csv("race_table_acs_2017_2021.csv")
# hh_char <- read_csv("hh_char_acs_2017_2021.csv")

#write_feather(income_bands, "income_bands.feather")
income_bands <- read_feather("income_bands.feather")

#write_feather(med_mean_income, "med_mean_income.feather")
med_mean_income <- read_feather("med_mean_income.feather")

#write_feather(race_eth, "race_eth.feather")
race_eth <- read_feather("race_eth.feather")

#write_feather(hh_char, "hh_char.feather")
hh_char <- read_feather("hh_char.feather")

valid_zips <- unique(med_mean_income$Zipcode)