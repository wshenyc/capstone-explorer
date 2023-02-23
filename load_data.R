library(tidyverse)

####pre-loading in data####
income_bands <- read_csv("acs_income_bands_2017_2021.csv")
med_mean_income <- read_csv("med_mean_income_acs_2017_2021.csv")

race_eth <- read_csv("race_table_acs_2017_2021.csv")

hh_char <- read_csv("hh_char_acs_2017_2021.csv")

valid_zips <- unique(med_mean_income$Zipcode)