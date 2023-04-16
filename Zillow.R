library(tidyverse)
library(feather)


###Zillow zipcode####
#home_val <- read_csv("Zip_zhvi_uc_sfr_tier_0.33_0.67_sm_sa_month.csv")
#write_feather(home_val, "home_val.feather")
home_val <- read_feather("home_val.feather")

growth_home <- read_csv("Zip_zhvf_growth_uc_sfrcondo_tier_0.33_0.67_sm_sa_month.csv")
#growth_home %>% write_feather("growth_home.feather")

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


#bedroom sizes
# zhvi_onebd <- read_csv("Zip_zhvi_bdrmcnt_1_uc_sfrcondo_tier_0.33_0.67_sm_sa_month.csv")
# zhvi_twobd <- read_csv("Zip_zhvi_bdrmcnt_2_uc_sfrcondo_tier_0.33_0.67_sm_sa_month.csv")
# zhvi_threebd <- read_csv("Zip_zhvi_bdrmcnt_3_uc_sfrcondo_tier_0.33_0.67_sm_sa_month.csv")
# zhvi_fourbd <- read_csv("Zip_zhvi_bdrmcnt_4_uc_sfrcondo_tier_0.33_0.67_sm_sa_month.csv")
# zhvi_fivebd <- read_csv("Zip_zhvi_bdrmcnt_5_uc_sfrcondo_tier_0.33_0.67_sm_sa_month.csv")

#line graph value of just the values
# zhvi_onebd %>% write_feather("zhvi_onebd.feather")
# zhvi_twobd %>% write_feather("zhvi_twobd.feather")
# zhvi_threebd %>% write_feather("zhvi_threebd.feather")
# zhvi_fourbd %>% write_feather("zhvi_fourbd.feather")
# zhvi_fivebd %>% write_feather("zhvi_fivebd.feather")


###percent growth over year by bedroom size####
pct_growth_annual <- function(df) {
  df %>% 
    select(-c(RegionID, SizeRank, RegionType, StateName, State, City, Metro, CountyName)) %>% 
    pivot_longer(cols = starts_with("20"),
                 names_to = "date",
                 values_to = "home_val") %>% 
    filter(grepl("12-31", date)) %>% 
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
}

onebd_five_year_growth <- pct_growth_annual(zhvi_onebd)
twobd_five_year_growth <- pct_growth_annual(zhvi_twobd)
threebd_five_year_growth <- pct_growth_annual(zhvi_threebd)
fourbd_five_year_growth <- pct_growth_annual(zhvi_fourbd)
fivebd_five_year_growth <- pct_growth_annual(zhvi_fivebd)

#feathers
# onebd_five_year_growth %>% write_feather("onebd_five_year_growth.feather")
# twobd_five_year_growth %>% write_feather("twobd_five_year_growth.feather")
# threebd_five_year_growth %>%  write_feather("threebd_five_year_growth.feather")
# fourbd_five_year_growth %>%  write_feather("fourbd_five_year_growth.feather")
# fivebd_five_year_growth %>%  write_feather("fivebd_five_year_growth.feather")


###avg percent growth over 5 years by bedroom size####
home_avg_five <- function(df) {
  df %>% 
    select(-c(RegionID, SizeRank, RegionType, StateName, State, City, Metro, CountyName)) %>% 
    pivot_longer(cols = starts_with("20"),
                 names_to = "date",
                 values_to = "home_val") %>% 
    filter(grepl("12-31", date)) %>% 
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
} 

onebd_avg_growth <- home_avg_five(zhvi_onebd)
twobd_avg_growth <- home_avg_five(zhvi_twobd)
threebd_avg_growth <- home_avg_five(zhvi_threebd)
fourbd_avg_growth <- home_avg_five(zhvi_fourbd)
fivebd_avg_growth <- home_avg_five(zhvi_fivebd)

#feathers
# onebd_avg_growth %>%  write_feather("onebd_avg_growth.feather")
# twobd_avg_growth %>%  write_feather("twobd_avg_growth.feather")
# threebd_avg_growth %>%  write_feather("threebd_avg_growth.feather")
# fourbd_avg_growth %>%  write_feather("fourbd_avg_growth.feather")
# fivebd_avg_growth %>%  write_feather("fivebd_avg_growth.feather")



###Zillow MSA geography####
#ZHVI for SFH
zhvi_metro <- read_csv("Metro_zhvi_uc_sfr_tier_0.33_0.67_sm_sa_month.csv")

#ZHVI diff bedroom sizes
zhvi_onebd_metro <- read_csv("Metro_zhvi_bdrmcnt_1_uc_sfrcondo_tier_0.33_0.67_sm_sa_month.csv")
zhvi_twobd_metro <- read_csv("Metro_zhvi_bdrmcnt_2_uc_sfrcondo_tier_0.33_0.67_sm_sa_month.csv")
zhvi_threebd_metro <- read_csv("Metro_zhvi_bdrmcnt_3_uc_sfrcondo_tier_0.33_0.67_sm_sa_month.csv")
zhvi_fourbd_metro <- read_csv("Metro_zhvi_bdrmcnt_4_uc_sfrcondo_tier_0.33_0.67_sm_sa_month.csv")
zhvi_fivebd_metro <- read_csv("Metro_zhvi_bdrmcnt_5_uc_sfrcondo_tier_0.33_0.67_sm_sa_month.csv")

#wriing feathers for the general line graphs for MSA 
# zhvi_metro %>% write_feather("zhvi_metro.feather")
# zhvi_onebd_metro %>% write_feather("zhvi_onebd_metro.feather")
# zhvi_twobd_metro %>% write_feather("zhvi_twobd_metro.feather")
# zhvi_threebd_metro %>% write_feather("zhvi_threebd_metro.feather")
# zhvi_fourbd_metro %>% write_feather("zhvi_fourbd_metro.feather")
# zhvi_fivebd_metro %>% write_feather("zhvi_fivebd_metro.feather")

###MSA annual pct change####
pct_growth_annual_metro <- function(df) {
  df %>% 
    select(-c(RegionID, SizeRank, RegionType, StateName)) %>% 
    pivot_longer(cols = starts_with("20"),
                 names_to = "date",
                 values_to = "home_val") %>% 
    filter(grepl("12-31", date)) %>% 
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
}


five_year_growth_metro <- pct_growth_annual_metro(zhvi_metro)
onebd_five_year_growth_metro <- pct_growth_annual_metro(zhvi_onebd_metro)
twobd_five_year_growth_metro <- pct_growth_annual_metro(zhvi_twobd_metro)
threebd_five_year_growth_metro <- pct_growth_annual_metro(zhvi_threebd_metro)
fourbd_five_year_growth_metro <- pct_growth_annual_metro(zhvi_fourbd_metro)
fivebd_five_year_growth_metro <- pct_growth_annual_metro(zhvi_fivebd_metro)

#feathers
# five_year_growth_metro %>% write_feather("five_year_growth_metro.feather")
# onebd_five_year_growth_metro %>% write_feather("onebd_five_year_growth_metro.feather")
# twobd_five_year_growth_metro %>% write_feather("twobd_five_year_growth_metro.feather")
# threebd_five_year_growth_metro %>%  write_feather("threebd_five_year_growth_metro.feather")
# fourbd_five_year_growth_metro %>%  write_feather("fourbd_five_year_growth_metro.feather")
# fivebd_five_year_growth_metro %>%  write_feather("fivebd_five_year_growth_metro.feather")

###MSA average pct change by 5-year bands####
home_avg_five_metro <- function(df) {
  df %>% 
    select(-c(RegionID, SizeRank, RegionType, StateName)) %>% 
    pivot_longer(cols = starts_with("20"),
                 names_to = "date",
                 values_to = "home_val") %>% 
    filter(grepl("12-31", date)) %>% 
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
} 

avg_growth_metro <- home_avg_five_metro(zhvi_metro)
onebd_avg_growth_metro <- home_avg_five_metro(zhvi_onebd_metro)
twobd_avg_growth_metro <- home_avg_five_metro(zhvi_twobd_metro)
threebd_avg_growth_metro <- home_avg_five_metro(zhvi_threebd_metro)
fourbd_avg_growth_metro <- home_avg_five_metro(zhvi_fourbd_metro)
fivebd_avg_growth_metro <- home_avg_five_metro(zhvi_fivebd_metro)

#feathers
# avg_growth_metro %>% write_feather("avg_growth_metro.feather")
# onebd_avg_growth_metro %>%  write_feather("onebd_avg_growth_metro.feather")
# twobd_avg_growth_metro %>%  write_feather("twobd_avg_growth_metro.feather")
# threebd_avg_growth_metro %>%  write_feather("threebd_avg_growth_metro.feather")
# fourbd_avg_growth_metro %>%  write_feather("fourbd_avg_growth_metro.feather")
# fivebd_avg_growth_metro %>%  write_feather("fivebd_avg_growth_metro.feather")


#ZHVF 
zhvf_metro <- read_csv("Metro_zhvf_growth_uc_sfrcondo_tier_0.33_0.67_sm_sa_month.csv")
#zhvf_metro %>% write_feather("growth_metro.feather")


