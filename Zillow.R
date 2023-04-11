library(tidyverse)
library(feather)

#home_val <- read_csv("Zip_zhvi_uc_sfr_tier_0.33_0.67_sm_sa_month.csv")
#write_feather(home_val, "home_val.feather")
home_val <- read_feather("home_val.feather")

#growth_home <- read_csv("Zip_zhvf_growth_uc_sfrcondo_tier_0.33_0.67_sm_sa_month.csv")
#write_feather(growth_home, "growth_home.feather")
growth_home <- read_feather("home_val.feather")

###percent growth over year
#let's do most recent 5 years % growth so like 2020-2021 
#2022-2021, 2021-2020, 2020-2019, 2019-2018, 2018-2017 
home_val_year <- home_val %>% 
  select(-c(RegionID, SizeRank, RegionType, StateName, State, City, Metro, CountyName)) %>% 
  pivot_longer(cols = starts_with("20"),
               names_to = "date",
               values_to = "home_val") %>% 
  filter(grepl("12-31", date)) 

home_val_year_five <- home_val_year %>% 
  filter(grepl("2022", date)|
           grepl("2021", date)|
           grepl("2020", date)|
           grepl("2019", date)|
           grepl("2018", date)|
           grepl("2017", date)) %>% 
  group_by(RegionName) %>% 
  mutate(pct_change = round((home_val-lag(home_val))/lag(home_val) * 100,1)) %>% 
  ungroup() %>% 
  filter(!grepl("2017", date)) %>% 
  mutate(date = str_sub(date, end = 4))
  

#write_feather(home_val_year_five, "home_val_year_five.feather")

#then average pct growth in 5 year periods?
#2000-2004, 2004-2008, 2008-2012, 2012-2016, 2016-2020
home_avg_five <- home_val_year %>% 
  filter(grepl("2000", date)|
           grepl("2004", date)|
           grepl("2008", date)|
           grepl("2012", date)|
           grepl("2016", date)|
           grepl("2020", date)) %>% 
  group_by(RegionName) %>% 
  mutate(pct_change = round((home_val-lag(home_val))/lag(home_val)/5 * 100,1)) %>% 
  ungroup() %>% 
  filter(!grepl("2000", date)) %>% 
  mutate(year_range = case_when(grepl("2004", date) ~ "2000-2004",
                                grepl("2008", date) ~ "2004-2008",
                                grepl("2012", date) ~ "2008-2012",
                                grepl("2016", date) ~ "2012-2016",
                                grepl("2020", date) ~ "2016-2020")) %>% 
  select(RegionName, year_range, pct_change)

#write_feather(home_avg_five, "home_avg_five.feather")


