library(tidycensus)
library(tidyverse)

"%notin%" <- Negate("%in%")

#Purpose: Pre-process ACS 2017-2021 data for race & ethnicity, median household income,

#census_api_key("0cb3ea8ea0c4a8cdd2bdbb55582d9c1e400093bc", install = T)


####Median Household Income#####

vars <- load_variables(
  2021,
  dataset = c("acs5/subject")
)

vars_medinc <- vars %>% 
  filter(grepl("S1901", name)) %>% 
  select(-c("concept")) %>% 
  head(13) %>% 
  separate(label, c("pre","label", "desc","extra"), "!!") %>% 
  mutate(extra = ifelse(is.na(extra), desc, extra)) %>% 
  select(name, extra)


income_table <- get_acs(
  geography = "zcta",
  table = "S1901", #median household income 
  year = 2021 #2017-2021 is the most recent 5-year ACS, I'm not crazy lol 
)

income_bands_full <- income_table %>% 
  filter(grepl("S1901_C01", variable)) %>% 
  filter(!grepl("S1901_C01_014", variable)) %>% 
  filter(!grepl("S1901_C01_015", variable)) %>% 
  filter(!grepl("S1901_C01_016", variable)) %>% 
  left_join(vars_medinc, by = c("variable"="name")) %>% 
  filter(!grepl("Total", extra)) %>% 
  filter(!grepl("(dollars)", extra)) %>% 
  select(GEOID, extra, estimate) %>% 
  rename(Label = extra,
         Estimate = estimate,
         Zipcode = GEOID) %>% 
  groupby(Zipcode) %>% 
  

#writing to csv 
#income_bands_full %>% write_csv("acs_income_bands_2017_2021.csv")

med_mean_income <- income_table %>% 
  filter(grepl("S1901_C01_001", variable) |
           grepl("S1901_C01_012", variable) |
           grepl("S1901_C01_013", variable)) %>% 
  left_join(vars_medinc, by = c("variable"="name")) %>% 
  select(GEOID, extra, estimate) %>% 
  rename(Label = extra,
         Estimate = estimate,
         Zipcode = GEOID)

#med_mean_income %>% write_csv("med_mean_income_acs_2017_2021.csv")


####household income by race/ethnicity####
#this is going to take more time than I anticipated lol

vars_inc_rr <- load_variables(
  2021,
  dataset = c("acs5")
)

inc_rr_b <- get_acs(
  geography = "zcta",
  table = "B19001B", #household income by race/ethnicity
  year = 2021 
)

inc_rr_b_formatted <- inc_rr_b %>% 
  select(GEOID, estimate) %>% 
  rename(Zipcode = GEOID,
         estimate_black = estimate) 

# inc_rr_b_labelled <- inc_rr %>% 
#   left_join(vars_inc_rr, by = c("variable"="name"))

inc_rr_d <- get_acs(
  geography = "zcta",
  table = "B19001D", #household income by race/ethnicity
  year = 2021 
)

inc_rr_d_formatted <- inc_rr_d %>% 
  select(GEOID, estimate) %>% 
  rename(Zipcode = GEOID,
         estimate_asian = estimate) 

inc_rr_h <- get_acs(
  geography = "zcta",
  table = "B19001H", #household income by race/ethnicity
  year = 2021 
)

inc_rr_h_formatted <- inc_rr_h %>% 
  select(GEOID, estimate) %>% 
  rename(Zipcode = GEOID,
         estimate_white = estimate)

inc_rr_i <- get_acs(
  geography = "zcta",
  table = "B19001I", #household income by race/ethnicity
  year = 2021 
)

inc_rr_i_formatted <- inc_rr_i %>% 
  select(GEOID, estimate) %>% 
  rename(Zipcode = GEOID,
         estimate_his = estimate) 

master_inc_race <- inc_rr_b_formatted %>% 
  left_join(inc_rr_d_formatted, by = "Zipcode") %>% 
  left_join(inc_rr_h_formatted, by = "Zipcode") %>% 
  left_join(inc_rr_i_formatted, by = "Zipcode")


# B black alone
## C American Indian and Alaska Native alone
# D asian alone
## E native hawaiian and other pacific islander
## F some other race alone
## G two or more races 
# H white alone, not hispanic or latino
# I hispanic or latino 




####household characteristics####
vars_dp  <- load_variables(
  2021, 
  dataset = "acs5/profile"
)

hh_char <- get_acs(
  geography = "zcta",
  table = "DP04", #selected housing characteristics 
  year = 2021
)

hh_char_labelled <- hh_char %>% 
  left_join(vars_dp, by = c("variable"="name")) %>% 
  filter(str_sub(variable,start=-1) != "P" &
           (grepl("HOUSING OCCUPANCY", label) |
              grepl("HOUSING TENURE", label) |
              grepl("VALUE", label) |
              grepl("SELECTED MONTHLY OWNER COSTS AS A PERCENTAGE", label) |
              grepl("GROSS RENT!!", label) |
              grepl("GROSS RENT AS A PERCENTAGE", label))
  )


hh_char_formatted <- hh_char_labelled %>% 
  select(GEOID, variable, estimate, label) %>% 
  separate(label, c("drop","category","subcat","desc"), "!!") %>% #breaks up by !!
  mutate(desc = if_else(is.na(desc), subcat, desc),
         temp_sort = case_when(paste(category, subcat) == "SELECTED MONTHLY OWNER COSTS AS A PERCENTAGE OF HOUSEHOLD INCOME (SMOCAPI) Housing units with a mortgage (excluding units where SMOCAPI cannot be computed)" ~
                             "SMOCAPI with",
                             paste(category, subcat) == "SELECTED MONTHLY OWNER COSTS AS A PERCENTAGE OF HOUSEHOLD INCOME (SMOCAPI) Housing units without a mortgage (excluding units where SMOCAPI cannot be computed)" ~
                               "SMOCAPI without",
                             !grepl("SMOCAPI", subcat) ~ category)) %>% 
  group_by(GEOID, temp_sort) %>% 
  mutate(total_val = first(estimate),
    Percentage = case_when(desc %notin% c("Total housing units",
                                               "Homeowner vacancy rate",
                                               "Rental vacancy rate", 
                                               "Average household size of owner-occupied unit",
                                               "Average household size of renter-occupied unit",
                                               "Owner-occupied units",
                                               "Median (dollars)",
                                               "Housing units with a mortgage (excluding units where SMOCAPI cannot be computed)",
                                               "Housing unit without a mortgage (excluding units where SMOCAPI cannot be computed)",
                                               "Occupied units paying rent",
                                               "Occupied units paying rent (excluding units where GRAPI cannot be computed)",
                                               "Not computed", 
                                               "No rent paid")
                           &  (paste(subcat, desc) != "Occupied housing units Occupied housing units") ~ 
                                  round(100 * (estimate/total_val),1)
                       
         ),
    Estimate = if_else(estimate %%1 == 0, 
                       format(round(as.numeric(estimate), 0), big.mark=",", trim = TRUE),
                       as.character(estimate))) %>% 
  ungroup() %>% 
  select(GEOID, desc, category, Estimate, Percentage) %>%
  rename(Zipcode = GEOID,
         Label = desc) 


#hh_char_formatted %>% write_csv("hh_char_acs_2017_2021.csv")

####Race and Ethnicity####

vars_race <- load_variables(
  2021,
  dataset = c("acs5")
)

race_table <- get_acs(
  geography = "zcta",
  table = "B03002", #hispanic or latino origin by race
  year = 2021 
)


race_table_labelled <- race_table %>% 
  left_join(vars_race , by =c("variable"="name")) %>% 
  filter(label %in% c("Estimate!!Total:!!Hispanic or Latino:",
                      "Estimate!!Total:!!Not Hispanic or Latino:!!American Indian and Alaska Native alone",
                      "Estimate!!Total:!!Not Hispanic or Latino:!!White alone",
                      "Estimate!!Total:!!Not Hispanic or Latino:!!Asian alone",
                      "Estimate!!Total:!!Not Hispanic or Latino:!!Black or African American alone",
                      "Estimate!!Total:!!Not Hispanic or Latino:!!Native Hawaiian and Other Pacific Islander alone",
                      "Estimate!!Total:!!Not Hispanic or Latino:!!Some other race alone",
                      "Estimate!!Total:!!Not Hispanic or Latino:!!Two or more races:")) %>% 
  separate(label, c("drop","category","subcat","desc"), "!!") %>% #breaks up by !!
  mutate(desc = ifelse(is.na(desc), subcat, desc),
         Estimate =  format(round(as.numeric(estimate), 1), big.mark=","),
         Label = desc,
         Zipcode = GEOID,
         Label = gsub(':', '', Label)) %>% 
  group_by(Zipcode) %>%
  mutate(Percentage = round(100 * (estimate / sum(estimate)),1)) %>%
  ungroup() %>% 
  select(Zipcode, Label, Estimate, Percentage)
  

#race_table_labelled %>% write_csv("race_table_acs_2017_2021.csv")

####Feathers####
# income_bands <- read_csv("acs_income_bands_2017_2021.csv")
# med_mean_income <- read_csv("med_mean_income_acs_2017_2021.csv")
#race_eth <- read_csv("race_table_acs_2017_2021.csv")
# hh_char <- read_csv("hh_char_acs_2017_2021.csv")

#write_feather(income_bands, "income_bands.feather")
#write_feather(med_mean_income, "med_mean_income.feather")
#write_feather(race_eth, "race_eth.feather")
#write_feather(hh_char, "hh_char.feather")

