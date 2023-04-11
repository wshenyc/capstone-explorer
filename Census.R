library(tidycensus)
library(tidyverse)
library(feather)


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

###med income zip code####
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
         Zipcode = GEOID) 
  

  

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

####med income place#####
income_table_place <- get_acs(
  geography = "place",
  table = "S1901", #median household income 
  year = 2021 #2017-2021 is the most recent 5-year ACS, I'm not crazy lol 
)


#I should probably do filter by state then place I guess? 

income_bands_full_place <- income_table_place %>% 
  filter(grepl("S1901_C01", variable)) %>% 
  filter(!grepl("S1901_C01_014", variable)) %>% 
  filter(!grepl("S1901_C01_015", variable)) %>% 
  filter(!grepl("S1901_C01_016", variable)) %>% 
  left_join(vars_medinc, by = c("variable"="name")) %>% 
  filter(!grepl("Total", extra)) %>% 
  filter(!grepl("(dollars)", extra)) %>% 
  select(NAME, extra, estimate) %>% 
  rename(Label = extra,
         Estimate = estimate) %>% 
  separate(NAME, c("City","State","Extra State"), sep = ",")

income_bands_place_clean <- income_bands_full_place %>% 
  mutate(State = if_else(!is.na(`Extra State`), `Extra State`, State)) %>% 
  select(-c(`Extra State`))

#income_bands_place_clean %>% write_csv("income_bands_place.csv")

###med/avg income by place####
med_mean_income_place <- income_table_place %>% 
  filter(grepl("S1901_C01_001", variable) |
           grepl("S1901_C01_012", variable) |
           grepl("S1901_C01_013", variable)) %>% 
  left_join(vars_medinc, by = c("variable"="name")) %>% 
  select(NAME, extra, estimate) %>% 
  rename(Label = extra,
         Estimate = estimate) %>% 
  separate(NAME, c("City","State","Extra State"), sep = ",") %>% 
  mutate(State = if_else(!is.na(`Extra State`), `Extra State`, State)) %>% 
  select(-c(`Extra State`))


####household income by race/ethnicity####
#this is going to take more time than I anticipated lol

vars_inc_rr <- load_variables(
  2021,
  dataset = c("acs5")
)


###household income by race by zipcode#### 
inc_rr_b <- get_acs(
  geography = "zcta",
  table = "B19001B", #black household income by race/ethnicity
  year = 2021 
)

inc_rr_b_formatted <- inc_rr_b %>% 
  select(GEOID, estimate, variable) %>% 
  rename(Zipcode = GEOID,
         estimate_black = estimate) %>% 
  left_join(vars_inc_rr, by = c("variable"="name")) %>% 
  separate(label, c("drop","category","subcat","desc"), "!!") %>%  #breaks up by !!
  filter(!is.na(subcat)) %>% #dropping total # of households
  select(Zipcode, estimate_black, subcat) %>% 
  mutate(temp_join = paste(Zipcode, subcat, sep = "_")) #creating a unique col to join all the race df

inc_rr_d <- get_acs(
  geography = "zcta",
  table = "B19001D", #asian household income by race/ethnicity
  year = 2021 
)

inc_rr_d_formatted <- inc_rr_d %>% 
  select(GEOID, estimate, variable) %>% 
  rename(Zipcode = GEOID,
         estimate_asian = estimate) %>% 
  left_join(vars_inc_rr, by = c("variable"="name")) %>% 
  separate(label, c("drop","category","subcat","desc"), "!!") %>%  #breaks up by !!
  filter(!is.na(subcat)) %>% #dropping total # of households
  select(Zipcode, estimate_asian, subcat) %>% 
  mutate(temp_join = paste(Zipcode, subcat, sep = "_")) #creating a unique col to join all the race df

inc_rr_h <- get_acs(
  geography = "zcta",
  table = "B19001H", #white alone household income by race/ethnicity
  year = 2021 
)

inc_rr_h_formatted <- inc_rr_h %>% 
  select(GEOID, estimate, variable) %>% 
  rename(Zipcode = GEOID,
         estimate_white = estimate) %>% 
  left_join(vars_inc_rr, by = c("variable"="name")) %>% 
  separate(label, c("drop","category","subcat","desc"), "!!") %>%  #breaks up by !!
  filter(!is.na(subcat)) %>% #dropping total # of households
  select(Zipcode, estimate_white, subcat) %>% 
  mutate(temp_join = paste(Zipcode, subcat, sep = "_")) #creating a unique col to join all the race df

inc_rr_i <- get_acs(
  geography = "zcta",
  table = "B19001I", #hispanic household income by race/ethnicity
  year = 2021 
)

inc_rr_i_formatted <- inc_rr_i %>% 
  select(GEOID, estimate, variable) %>% 
  rename(Zipcode = GEOID,
         estimate_his = estimate) %>% 
  left_join(vars_inc_rr, by = c("variable"="name")) %>% 
  separate(label, c("drop","category","subcat","desc"), "!!") %>%  #breaks up by !!
  filter(!is.na(subcat)) %>% #dropping total # of households
  select(Zipcode, estimate_his, subcat) %>% 
  mutate(temp_join = paste(Zipcode, subcat, sep = "_")) #creating a unique col to join all the race df


master_inc_race <- inc_rr_b_formatted %>% 
  left_join(select(inc_rr_d_formatted, c(estimate_asian, temp_join)), by = "temp_join") %>% 
  left_join(select(inc_rr_h_formatted, c(estimate_white, temp_join)), by = "temp_join") %>% 
  left_join(select(inc_rr_i_formatted, c(estimate_his, temp_join)), by = "temp_join") %>% 
  select(Zipcode, subcat, estimate_black, estimate_asian, estimate_white, estimate_his)

####household income by race by place####
inc_rr_b_place <- get_acs(
  geography = "place",
  table = "B19001B", #black household income by race/ethnicity
  year = 2021 
)

inc_rr_b_formatted_place <- inc_rr_b_place %>% 
  select(NAME, estimate, variable) %>% 
  rename(estimate_black = estimate) %>% 
  left_join(vars_inc_rr, by = c("variable"="name")) %>% 
  separate(label, c("drop","category","subcat","desc"), "!!") %>%  #breaks up by !!
  filter(!is.na(subcat)) %>% #dropping total # of households
  select(NAME, estimate_black, subcat) %>% 
  mutate(temp_join = paste(NAME, subcat, sep = "_")) %>%  #creating a unique col to join all the race df
  separate(NAME, c("City","State","Extra State"), sep = ",") %>% 
  mutate(State = if_else(!is.na(`Extra State`), `Extra State`, State)) %>% 
  select(-c(`Extra State`))

inc_rr_d_place <- get_acs(
  geography = "place",
  table = "B19001D", #asian household income by race/ethnicity
  year = 2021 
)

inc_rr_d_formatted_place <- inc_rr_d_place %>% 
  select(NAME, estimate, variable) %>% 
  rename(estimate_asian = estimate) %>% 
  left_join(vars_inc_rr, by = c("variable"="name")) %>% 
  separate(label, c("drop","category","subcat","desc"), "!!") %>%  #breaks up by !!
  filter(!is.na(subcat)) %>% #dropping total # of households
  select(NAME, estimate_asian, subcat) %>% 
  mutate(temp_join = paste(NAME, subcat, sep = "_")) %>%  #creating a unique col to join all the race df
  separate(NAME, c("City","State","Extra State"), sep = ",") %>% 
  mutate(State = if_else(!is.na(`Extra State`), `Extra State`, State)) %>% 
  select(-c(`Extra State`))

inc_rr_h <- get_acs(
  geography = "place",
  table = "B19001H", #white alone household income by race/ethnicity
  year = 2021 
)

inc_rr_h_formatted_place <- inc_rr_h %>% 
  select(NAME, estimate, variable) %>% 
  rename(estimate_white = estimate) %>% 
  left_join(vars_inc_rr, by = c("variable"="name")) %>% 
  separate(label, c("drop","category","subcat","desc"), "!!") %>%  #breaks up by !!
  filter(!is.na(subcat)) %>% #dropping total # of households
  select(NAME, estimate_white, subcat) %>% 
  mutate(temp_join = paste(NAME, subcat, sep = "_")) %>%  #creating a unique col to join all the race df
  separate(NAME, c("City","State","Extra State"), sep = ",") %>% 
  mutate(State = if_else(!is.na(`Extra State`), `Extra State`, State)) %>% 
  select(-c(`Extra State`))

inc_rr_i <- get_acs(
  geography = "place",
  table = "B19001I", #hispanic household income by race/ethnicity
  year = 2021 
)

inc_rr_i_formatted_place <- inc_rr_i %>% 
  select(NAME, estimate, variable) %>% 
  rename(estimate_his = estimate) %>% 
  left_join(vars_inc_rr, by = c("variable"="name")) %>% 
  separate(label, c("drop","category","subcat","desc"), "!!") %>%  #breaks up by !!
  filter(!is.na(subcat)) %>% #dropping total # of households
  select(NAME, estimate_his, subcat) %>% 
  mutate(temp_join = paste(NAME, subcat, sep = "_")) %>%  #creating a unique col to join all the race df
  separate(NAME, c("City","State","Extra State"), sep = ",") %>% 
  mutate(State = if_else(!is.na(`Extra State`), `Extra State`, State)) %>% 
  select(-c(`Extra State`))


master_inc_race_place <- inc_rr_b_formatted_place %>% 
  left_join(select(inc_rr_d_formatted_place, c(estimate_asian, temp_join)), by = "temp_join") %>% 
  left_join(select(inc_rr_h_formatted_place, c(estimate_white, temp_join)), by = "temp_join") %>% 
  left_join(select(inc_rr_i_formatted_place, c(estimate_his, temp_join)), by = "temp_join") %>% 
  select(City, State, subcat, estimate_black, estimate_asian, estimate_white, estimate_his)


# B black alone
## C American Indian and Alaska Native alone
# D asian alone
## E native hawaiian and other pacific islander
## F some other race alone
## G two or more races 
# H white alone, not hispanic or latino
# I hispanic or latino 

###doing percentages instead




####household characteristics####
vars_dp  <- load_variables(
  2021, 
  dataset = "acs5/profile"
)

###hh char by zipcode####

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


###hh char by place####
hh_char_place <- get_acs(
  geography = "place",
  table = "DP04", #selected housing characteristics 
  year = 2021
)

hh_char_labelled_place <- hh_char_place %>% 
  left_join(vars_dp, by = c("variable"="name")) %>% 
  filter(str_sub(variable,start=-1) != "P" &
           (grepl("HOUSING OCCUPANCY", label) |
              grepl("HOUSING TENURE", label) |
              grepl("VALUE", label) |
              grepl("SELECTED MONTHLY OWNER COSTS AS A PERCENTAGE", label) |
              grepl("GROSS RENT!!", label) |
              grepl("GROSS RENT AS A PERCENTAGE", label))
  )


hh_char_formatted_place <- hh_char_labelled_place %>% 
  select(NAME, variable, estimate, label) %>% 
  separate(label, c("drop","category","subcat","desc"), "!!") %>% #breaks up by !!
  mutate(desc = if_else(is.na(desc), subcat, desc),
         temp_sort = case_when(paste(category, subcat) == "SELECTED MONTHLY OWNER COSTS AS A PERCENTAGE OF HOUSEHOLD INCOME (SMOCAPI) Housing units with a mortgage (excluding units where SMOCAPI cannot be computed)" ~
                                 "SMOCAPI with",
                               paste(category, subcat) == "SELECTED MONTHLY OWNER COSTS AS A PERCENTAGE OF HOUSEHOLD INCOME (SMOCAPI) Housing units without a mortgage (excluding units where SMOCAPI cannot be computed)" ~
                                 "SMOCAPI without",
                               !grepl("SMOCAPI", subcat) ~ category)) %>% 
  group_by(NAME, temp_sort) %>% 
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
  select(NAME, desc, category, Estimate, Percentage) %>%
  rename(Label = desc) %>% 
  separate(NAME, c("City","State","Extra State"), sep = ",") %>% 
  mutate(State = if_else(!is.na(`Extra State`), `Extra State`, State)) %>% 
  select(-c(`Extra State`))

####Race and Ethnicity####

vars_race <- load_variables(
  2021,
  dataset = c("acs5")
)

###race and eth by zipcode####

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

###race eth by place#####
race_table_place <- get_acs(
  geography = "place",
  table = "B03002", #hispanic or latino origin by race
  year = 2021 
)

race_table_labelled_place <- race_table_place %>% 
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
         Label = gsub(':', '', Label)) %>% 
  group_by(NAME) %>%
  mutate(Percentage = round(100 * (estimate / sum(estimate)),1)) %>%
  ungroup() %>% 
  select(NAME, Label, Estimate, Percentage) %>% 
  separate(NAME, c("City","State","Extra State"), sep = ",") %>% 
  mutate(State = if_else(!is.na(`Extra State`), `Extra State`, State)) %>% 
  select(-c(`Extra State`))

###Demographic characteristics for occupied housing units####
vars <- load_variables(
  2021,
  dataset = c("acs5/subject")
)

vars_rent <- vars %>% 
  filter(grepl("S2502", name)) %>% 
  select(-c("concept")) %>% 
  separate(label, c("pre","label", "desc","extra", "more","evenmore"), "!!") %>% 
  filter(label %in% c("Owner-occupied housing units",
                      "Percent owner-occupied housing units",
                      "Renter-occupied housing units",
                      "Percent renter-occupied housing units"),
         extra %in% c("RACE AND HISPANIC OR LATINO ORIGIN OF HOUSEHOLDER")) %>% 
  mutate(evenmore = ifelse(is.na(evenmore), more, evenmore)) %>% 
  select(name, label, evenmore) %>% 
  filter(evenmore %notin% c("Hispanic or Latino origin", "White alone, not Hispanic or Latino"))

###dem char rent owner zip code####
dem_table <- get_acs(
  geography = "zcta",
  table = "S2502", #median household income 
  year = 2021 #2017-2021 is the most recent 5-year ACS, I'm not crazy lol 
)

dem_table_labelled <- dem_table %>% 
  filter(variable %in% vars_rent$name) %>% 
  left_join(vars_rent, by = c("variable"="name")) %>% 
  select(GEOID, label, evenmore, estimate) %>% 
  pivot_wider(names_from = label,
              values_from = estimate) %>% 
  rename(Zipcode = GEOID,
         Race = evenmore)


###dem char rent owner by place#### 
dem_table_place <- get_acs(
  geography = "place",
  table = "S2502", #median household income 
  year = 2021 #2017-2021 is the most recent 5-year ACS, I'm not crazy lol 
)

dem_table_labelled_place <- dem_table_place %>% 
  filter(variable %in% vars_rent$name) %>% 
  left_join(vars_rent, by = c("variable"="name")) %>% 
  select(NAME, label, evenmore, estimate) %>% 
  pivot_wider(names_from = label,
              values_from = estimate) %>% 
  rename(Race = evenmore) %>% 
  separate(NAME, c("City","State","Extra State"), sep = ",") %>% 
  mutate(State = if_else(!is.na(`Extra State`), `Extra State`, State)) %>% 
  select(-c(`Extra State`))

####Feathers####
# income_bands <- read_csv("acs_income_bands_2017_2021.csv")
# med_mean_income <- read_csv("med_mean_income_acs_2017_2021.csv")
#race_eth <- read_csv("race_table_acs_2017_2021.csv")
# hh_char <- read_csv("hh_char_acs_2017_2021.csv")

#write_feather(income_bands, "income_bands.feather")
#write_feather(med_mean_income, "med_mean_income.feather")

#write_feather(race_eth, "race_eth.feather")
#write_feather(race_table_labelled_place, "race_eth_place.feather")

#write_feather(hh_char, "hh_char.feather")
#write_feather(hh_char_formatted_place, "hh_char_place.feather")

#write_feather(income_bands_place_clean, "acs_income_bands_place.feather")
#write_feather(med_mean_income_place, "med_mean_income_place.feather")

#write_feather(master_inc_race, "race_income_bands_zip.feather")
#write_feather(master_inc_race_place, "race_income_bands_place.feather")

#write_feather(dem_table_labelled, "rent_own_zip.feather")
#write_feather(dem_table_labelled_place, "rent_own_zip_place.feather")