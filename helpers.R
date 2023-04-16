library(tidyverse)
library(echarts4r)
library(DT)


"%notin%" <- Negate("%in%")


#####actual functions####

####zipcode validation function####

validateRequiredInput <- function(inputData) {
  
  if(inputData %notin% valid_zips) {
      shinyalert(
        title = "Error",
        text = paste("Please enter a valid US zipcode"),
        type = "error"
      )
    
    return(FALSE)
  }
  
 
  shinyalert(
    title = "Success",
    text = paste("Successful search!"),
    type = "success"
  )
  
  return(TRUE)
}

####geo select####
geo_select <- function(df, x, y) {
  if("City" %in% colnames(df)) {
    df %>% 
      filter(str_trim(State) == y) %>% 
      filter(City == x) %>% 
      select(-c(City, State))
  } 
  else {
    df %>% 
      filter(Zipcode == x) %>% 
      select(-c(Zipcode))
  }
 
}


####Household Characteristics#### 


hhchar_table_gen <- function(df, user_zip, chosen_state, cat) {
  x <- geo_select(df, user_zip, chosen_state) %>% 
    filter(category == cat)
  
  DT::renderDT(
  datatable(x,
            extensions = 'RowGroup',
            options = list(
              dom = 'tp',
              pageLength = 5,
              rowGroup = list(dataSrc=c(2)),
                           columnDefs = list(list(visible=FALSE, targets=c(2))))
  ) %>% 
   formatString('Percentage', suffix="%")
  )
  
}


####hh char react versions####
hhchar_table_report <- function(df, user_zip,chosen_state) {

  x <-   geo_select(df, user_zip,chosen_state) #testing if I can get away with just changing geo_select
}

hhchar_react_report <- function(df) {
  datatable(df,
            extensions = 'RowGroup',
            options = list(
              dom = 'tpf', #adding a filter
              pageLength = 5,
              rowGroup = list(dataSrc=c(2)),
              columnDefs = list(list(visible=FALSE, targets=c(2))))
  ) %>% 
    formatString('Percentage', suffix="%")
}


####Race and Ethnicity####

race_table_gen <- function(df, user_zip,chosen_state) {
  
  x <- geo_select(df, user_zip,chosen_state) %>%  #testing
    mutate(Estimate = as.numeric(str_replace_all(Estimate, "[[:punct:]]", ""))) 
    
  plot <- 
    x %>% 
    e_charts(Label,
             height = '600px') %>% 
    e_pie(Estimate) %>% 
    e_tooltip(
      formatter = htmlwidgets::JS("
      function(params){
       return(params.name + ': ' + params.value +  '<br />' +
      'Percentage: ' +  params.percent + '%'
       )
      }
    ")
    )%>%  
    e_legend(show=FALSE)


renderEcharts4r({
  plot
})

}

##react race/eth###
race_table_report <- function(df, user_zip,chosen_state) {
  x <-   geo_select(df, user_zip,chosen_state) %>%  #testing
    mutate(Estimate = as.numeric(str_replace_all(Estimate, "[[:punct:]]", ""))) 
}

race_table_react <- function(df) {
    df %>% 
    e_charts(Label,
             height = '600px') %>% 
    e_pie(Estimate) %>% 
    e_tooltip(
      formatter = htmlwidgets::JS("
      function(params){
       return(params.name + ': ' + params.value +  '<br />' +
      'Percentage: ' +  params.percent + '%'
       )
      }
    ")
    )%>%  
    e_legend(show=FALSE)
}



####Income ####
income_table_gen <- function(df, user_zip,chosen_state) {
  x <- geo_select(df, user_zip,chosen_state)
  
  plot <- x %>% 
    e_charts(Label) %>% 
    e_bar(Estimate) %>% 
    e_tooltip(
      formatter = htmlwidgets::JS("
      function(params){
        return(params.value[0]+ ': ' +params.value[1] + '%')
      }
    ")
    )%>% 
    e_x_axis(axisLabel = list(interval = 0, rotate = 20)) %>% 
    e_legend(show = FALSE)
  
  renderEcharts4r({
    plot
  })

}

###react income bands
income_table_report <- function(df, user_zip,chosen_state) {
  
  x <-   geo_select(df, user_zip,chosen_state) 
}

income_react_report <- function(df) {
  df %>% 
    e_charts(Label) %>% 
    e_bar(Estimate) %>% 
    e_tooltip(
      formatter = htmlwidgets::JS("
      function(params){
        return(params.value[0]+ ': ' +params.value[1] + '%')
      }
    ")
    )%>% 
    e_x_axis(axisLabel = list(interval = 0, rotate = 20)) %>% 
    e_legend(show = FALSE)
}


####Income by Race####
inc_race_gen <- function(df, user_zip,chosen_state) {
  x <- geo_select(df, user_zip,chosen_state) %>% 
    rename(Asian = estimate_asian,
           Black = estimate_black,
           Hispanic = estimate_his,
           White = estimate_white)
  
  plot <- x %>% 
    e_charts(subcat) %>% 
    e_bar(Asian) %>% 
    e_bar(Black) %>% 
    e_bar(Hispanic) %>% 
    e_bar(White) %>% 
    e_tooltip(
      formatter = htmlwidgets::JS("
      function(params){
       return(params.name + ': ' + params.value[1]  
       )
      }
    ")
    )%>% 
    e_x_axis(axisLabel = list(interval = 0, rotate = 20))
  
  renderEcharts4r({
    plot
  })
  
}

###react income by race###
inc_race_report <- function(df, user_zip,chosen_state) {
  x <- geo_select(df, user_zip,chosen_state) %>% 
    rename(Asian = estimate_asian,
           Black = estimate_black,
           Hispanic = estimate_his,
           White = estimate_white)
}

inc_race_react <- function(df){
  df %>% 
    e_charts(subcat) %>% 
    e_bar(Asian) %>% 
    e_bar(Black) %>% 
    e_bar(Hispanic) %>% 
    e_bar(White) %>% 
    e_tooltip(
      formatter = htmlwidgets::JS("
      function(params){
       return(params.name + ': ' + params.value[1]  
       )
      }
    ")
    )%>% 
    e_x_axis(axisLabel = list(interval = 0, rotate = 20)) 
}


####Renter/Owner by Race####
rent_own_gen <- function(df, user_zip, chosen_state) {
  x <- geo_select(df, user_zip, chosen_state) 
  
  DT::renderDT({
    datatable(x,
              options = list(
                dom = 't', 
                pageLength = 10
                )
    ) %>% 
      formatString(columns = c('Percent owner-occupied housing units', 
                               'Percent renter-occupied housing units'), 
                   suffix="%")
  }) 
}

###react renter/owner###
rent_own_report <- function(df, user_zip, chosen_state) {
  x <- geo_select(df, user_zip, chosen_state) 
}

rent_own_react <- function(df){
  datatable(df,
            options = list(
              dom = 't', 
              pageLength = 10
            )
  ) %>% 
    formatString(columns = c('Percent owner-occupied housing units', 
                             'Percent renter-occupied housing units'), 
                 suffix="%")
}

####Median Income####
income_med_gen <- function(df, user_zip,chosen_state) {
  x <- geo_select(df, user_zip,chosen_state) %>% 
    filter(!grepl("Total",Label))
  
  med_income <- x %>% 
    filter(grepl("Median", Label))
  
  med_income_value <- med_income$Estimate
  
  med_income_value <- format(round(as.numeric(med_income_value), 1),  big.mark=",")
  
}

#####Avg Income####
income_avg_gen <- function(df, user_zip,chosen_state) {
  x <- geo_select(df, user_zip,chosen_state) %>% 
    filter(!grepl("Total",Label))
  
  avg_income <- x %>% 
    filter(grepl("Mean", Label))
  
  avg_income_value <- avg_income$Estimate 
  
  avg_income_value <- format(round(as.numeric(avg_income_value), 1),  big.mark=",")
   
}


####Home value trends####
val_table_gen <- function(df, user_zip) {
  if("State" %in% colnames(df)) {
    x <- df %>% 
      filter(RegionName == user_zip) %>% 
      select(-c(RegionName, RegionID, SizeRank, RegionType, StateName, State, City, Metro, CountyName)) %>% 
      pivot_longer(cols = starts_with("20"),
                   names_to = "date",
                   values_to = "Value") 
  } else {
    x <- df %>% 
      filter(RegionName == user_zip) %>% 
      select(-c(RegionID, SizeRank, RegionType, StateName)) %>% 
      pivot_longer(cols = starts_with("20"),
                   names_to = "date",
                   values_to = "Value") 
  }
  
  plot <- x %>% 
    e_charts(date) %>% 
    e_line(Value) %>% 
    e_datazoom(type = "slider") %>% 
    e_tooltip() %>% 
    e_legend(FALSE) %>% 
    e_title(text = "Typical Home Values from 2000 - 2023",
            subtext = "Source: Zillow Home Value Index (Single-Family Homes or By Bedroom Size For All Types of Homes)") 
  
  renderEcharts4r({
    plot
  })
}

##react zillow home value
val_table_report <- function(df, user_zip) {
  if("State" %in% colnames(df)) {
    x <- df %>% 
      filter(RegionName == user_zip) %>% 
      select(-c(RegionName, RegionID, SizeRank, RegionType, StateName, State, City, Metro, CountyName)) %>% 
      pivot_longer(cols = starts_with("20"),
                   names_to = "date",
                   values_to = "Value") 
  } else {
    x <- df %>% 
      filter(RegionName == user_zip) %>% 
      select(-c(RegionID, SizeRank, RegionType, StateName)) %>% 
      pivot_longer(cols = starts_with("20"),
                   names_to = "date",
                   values_to = "Value") 
  }
}

val_react_report <- function(df) {
  df %>% 
    e_charts(date) %>% 
    e_line(Value) %>% 
    e_datazoom(type = "slider") %>% 
    e_tooltip() %>% 
    e_legend(FALSE) %>% 
    e_title(text = "Typical Home Values from 2000 - 2023",
            subtext = "Source: Zillow Home Value Index (Single-Family Homes or By Bedroom Size For All Types of Homes)") 
}

####home value growth####
date_growth <- function(df, user_zip, date_val) {
  if("State" %in% colnames(df)) {
    x <- df %>% 
      filter(RegionName == user_zip) %>% 
      select(-c(RegionName, RegionID, SizeRank, RegionType, StateName, State, City, Metro, CountyName)) %>% 
      pivot_longer(cols = starts_with("20"),
                   names_to = "date",
                   values_to = "Growth") 
  } else {
    x <- df %>% 
      filter(RegionName == user_zip) %>% 
      select(-c(RegionID, SizeRank, RegionType, StateName)) %>% 
      pivot_longer(cols = starts_with("20"),
                   names_to = "date",
                   values_to = "Growth")  
  }

  value <- x %>% 
    filter(grepl(date_val, date))
  
 date_value <- value$Growth 
}

####home val pct change####
pct_change_val <- function(df, user_zip) {
  x <- df %>% 
    filter(RegionName == user_zip) %>% 
    select(date, pct_change)
}

pct_change_react <- function(df) {
  datatable(df,
            colnames=c("Year","Percent Change from Previous Year"),
            options = list(
              dom = 'tp',
              pageLength = 5)
  ) %>% 
    formatString('pct_change', suffix = '%')
    
  
}

####home val avg pct change####
pct_change_avg <- function(df, user_zip) {
  x <- df %>% 
    filter(RegionName == user_zip) %>% 
    select(year_range, pct_change)
}

pct_avg_react <- function(df) {
  datatable(df,
            colnames=c("Range","Average Percent Change in Home Value"),
            options = list(
              dom = 'tp',
              pageLength = 5)
  ) %>% 
    formatString('pct_change', suffix = '%')
  
}
