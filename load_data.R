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

#Zillow#### 
#zipcode geo####
#all single family
home_val <- read_feather("home_val.feather")
home_val_five <- read_feather("home_val_year_five.feather")
home_avg_five <- read_feather("home_avg_five.feather")

#by bedroom sizes
onebd_zhvi <- read_feather("zhvi_onebd.feather")
twod_zhvi <- read_feather("zhvi_twobd.feather")
threebd_zhvi <- read_feather("zhvi_threebd.feather")
fourbd_zhvi <- read_feather("zhvi_fourbd.feather")
fivebd_zhvi <- read_feather("zhvi_fivebd.feather")

#annual pct growth
onebd_five_year_growth <- read_feather("onebd_five_year_growth.feather")
twobd_five_year_growth <- read_feather("twobd_five_year_growth.feather")
threebd_five_year_growth <- read_feather("threebd_five_year_growth.feather")
fourbd_five_year_growth <- read_feather("fourbd_five_year_growth.feather")
fivebd_five_year_growth <-  read_feather("fivebd_five_year_growth.feather")

#avg growth by 5 year bands
onebd_avg_growth <-  read_feather("onebd_avg_growth.feather")
twobd_avg_growth <-  read_feather("twobd_avg_growth.feather")
threebd_avg_growth <-  read_feather("threebd_avg_growth.feather")
fourbd_avg_growth <-  read_feather("fourbd_avg_growth.feather")
fivebd_avg_growth <-  read_feather("fivebd_avg_growth.feather")


#zhvf
growth_home <- read_feather("growth_home.feather")

#MSA geo####
#all single family
home_val_metro <- read_feather("zhvi_metro.feather")
home_val_five_metro <- read_feather("five_year_growth_metro.feather")
home_avg_five_metro <- read_feather("avg_growth_metro.feather")

#by bedroom sizes
onebd_zhvi_metro <- read_feather("zhvi_onebd_metro.feather")
twobd_zhvi_metro <- read_feather("zhvi_twobd_metro.feather")
threebd_zhvi_metro <- read_feather("zhvi_threebd_metro.feather")
fourbd_zhvi_metro <- read_feather("zhvi_fourbd_metro.feather")
fivebd_zhvi_metro <- read_feather("zhvi_fivebd_metro.feather")

#annual pct growth
onebd_five_year_growth_metro <- read_feather("onebd_five_year_growth_metro.feather")
twobd_five_year_growth_metro <- read_feather("twobd_five_year_growth_metro.feather")
threebd_five_year_growth_metro <- read_feather("threebd_five_year_growth_metro.feather")
fourbd_five_year_growth_metro <- read_feather("fourbd_five_year_growth_metro.feather")
fivebd_five_year_growth_metro <-  read_feather("fivebd_five_year_growth_metro.feather")

#avg growth by 5 year bands
onebd_avg_growth_metro <-  read_feather("onebd_avg_growth_metro.feather")
twobd_avg_growth_metro <-  read_feather("twobd_avg_growth_metro.feather")
threebd_avg_growth_metro <-  read_feather("threebd_avg_growth_metro.feather")
fourbd_avg_growth_metro <-  read_feather("fourbd_avg_growth_metro.feather")
fivebd_avg_growth_metro <-  read_feather("fivebd_avg_growth_metro.feather")


#zhvf
growth_metro <- read_feather("growth_metro.feather")

