library(tidycensus)
library(feather)

#home_val <- read_csv("Zip_zhvi_uc_sfr_tier_0.33_0.67_sm_sa_month.csv")
#write_feather(home_val, "home_val.feather")
home_val <- read_feather("home_val.feather")

#growth_home <- read_csv("Zip_zhvf_growth_uc_sfrcondo_tier_0.33_0.67_sm_sa_month.csv")
#write_feather(growth_home, "growth_home.feather")
growth_home <- read_feather("home_val.feather")