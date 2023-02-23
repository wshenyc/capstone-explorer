library(tidycensus)
library(tidyverse)

#Purpose: Pre-process ACS 2017-2021 data for race & ethnicity, median household income,
#household characteristics 

#figure out how to put this into a secret
#census_api_key("0cb3ea8ea0c4a8cdd2bdbb55582d9c1e400093bc", install = T)

#testing census call

#selecting some tables: household income & renter/homeowner breakdown & race/ethnicity
#building a profile 

#feb 22, 2023 
#we're going to pre-load these tables 
#helper functions will just work to 


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

# vars_medinc$label[vars_medinc$name == "S1901_C01_001"] <- "Total Number of Households"
# vars_medinc$label[vars_medinc$name == "S1901_C01_012"] <- "Median Household Income"
# vars_medinc$label[vars_medinc$name == "S1901_C01_013"] <- "Mean Household Income"


income_table <- get_acs(
  geography = "zcta",
  table = "S1901", #median household income 
  year = 2021
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

#testing
# med_mean_income_small <- med_mean_income %>% 
#   filter(Zipcode == "10026")


#this is for the small versions
income_table_small <- income_table %>% 
  filter(GEOID == 10026) %>% #user input
  head(13) %>% 
  left_join(vars_medinc, by = c("variable"="name")) %>% 
  select(extra, estimate) %>% 
  rename(Label = extra,
         Estimate = estimate)

#I think I want a bar graph showing this 

inc_chart <- income_table_small[2:11,] %>% #just selecting the income bands
  e_charts(Label) %>% 
  e_bar(
    Estimate
  ) %>% 
  e_tooltip() 

table_inc <- income_table_small[c(12,13),] %>% 
  mutate(Estimate =  format(round(as.numeric(Estimate), 1), big.mark=","))

datatable(table_inc,
          options = list(
            dom = 't'
          ))

#household characteristics
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
         #estimate =  format(round(as.numeric(estimate), 1), big.mark=",")
  ) %>% 
  select(GEOID, desc, category, estimate) %>%
  rename(Zipcode = GEOID,
         Label = desc,
         Estimate=estimate) 

#hh_char_formatted %>% write_csv("hh_char_acs_2017_2021.csv")

####Race and Ethnicity####

vars_race  <- load_variables(
  2021, #this would be the other thing to make into user input
  dataset = "acs5"
)


vars_race <- vars_race %>% 
  filter(grepl("B02001",name)) #race

race_table <- get_acs(
  geography = "zcta",
  table = "B02001", #selected housing characteristics 
  year = 2021 
)

race_table_labelled <- race_table %>% 
  left_join(vars_race , by =c("variable"="name")) %>% 
  separate(label, c("drop","category","subcat","desc"), "!!") %>% #breaks up by !!
  mutate(subcat = ifelse(is.na(subcat), "Total", subcat), 
         desc = if_else(is.na(desc), subcat, desc),
         Estimate =  format(round(as.numeric(estimate), 1), big.mark=","),
         Label = desc,
         # category = c("Total Population",
         #              "One Race Alone","One Race Alone","One Race Alone",
         #              "One Race Alone","One Race Alone","One Race Alone",
         #              "Two or More Races","Two or More Races","Two or More Races"),
         Zipcode = GEOID,
         category = case_when(grepl("alone", Label) ~ "One Race Alone",
                              grepl("Total", Label) ~ "Total Population",
                              grepl("Two", Label) ~ "Two or More Races")) %>% 
  select(Zipcode, Label, category, Estimate)

#  race_table_labelled %>% write_csv("race_table_acs_2017_2021.csv")

