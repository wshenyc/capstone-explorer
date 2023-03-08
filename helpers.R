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

####user zip selection####
zip_select <- function(df, x) {
  df %>% 
    filter(Zipcode == x) %>% 
    select(-c(Zipcode))
}


####Household Characteristics#### 


hhchar_table_gen <- function(df, user_zip, cat) {
  x <- zip_select(df, user_zip) %>% 
    filter(category == cat)
  
  DT::renderDT(
  datatable(x,
            extensions = 'RowGroup',
            options = list(
              dom = 'tp',
              pageLength = 5,
              rowGroup = list(dataSrc=c(2)),
                           columnDefs = list(list(visible=FALSE, targets=c(2))))
  )
  )
  
}


####Race and Ethnicity####

race_table_gen <- function(df, user_zip) {
  x <- zip_select(df, user_zip)
  
  plot <- x %>% 
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



####Income ####
income_table_gen <- function(df, user_zip) {
  x <- zip_select(df, user_zip)
  
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

####Median Income####
income_med_gen <- function(df, user_zip) {
  x <- zip_select(df, user_zip) %>% 
    filter(!grepl("Total",Label))
  
  med_income <- x %>% 
    filter(grepl("Median", Label))
  
  med_income_value <- med_income$Estimate
  
  med_income_value <- format(round(as.numeric(med_income_value), 1),  big.mark=",")
  
}

#####Avg Income####
income_avg_gen <- function(df, user_zip) {
  x <- zip_select(df, user_zip) %>% 
    filter(!grepl("Total",Label))
  
  avg_income <- x %>% 
    filter(grepl("Mean", Label))
  
  avg_income_value <- avg_income$Estimate 
  
  avg_income_value <- format(round(as.numeric(avg_income_value), 1),  big.mark=",")
   
}


####Home value trends####
val_table_gen <- function(df, user_zip) {
  x <- df %>% 
    filter(RegionName == user_zip) %>% 
    select(-c(RegionName, RegionID, SizeRank, RegionType, StateName, State, City, Metro, CountyName)) %>% 
    pivot_longer(cols = starts_with("20"),
                 names_to = "date",
                 values_to = "Value") 
  
  plot <- x %>% 
    e_charts(date) %>% 
    e_line(Value) %>% 
    e_datazoom(type = "slider") %>% 
    e_tooltip() %>% 
    e_legend(FALSE) %>% 
    e_title(text = "Typical Home Values from Jan 2000 - Jan 2023",
            subtext = "Source: Zillow Home Value Index") 
  
  renderEcharts4r({
    plot
  })
}

####home value growth####
date_growth <- function(df, user_zip, date_val) {
  x <- df %>% 
    filter(RegionName == user_zip) %>% 
    select(-c(RegionName, RegionID, SizeRank, RegionType, StateName, State, City, Metro, CountyName)) %>% 
    pivot_longer(cols = starts_with("20"),
                 names_to = "date",
                 values_to = "Growth") 
  
  value <- x %>% 
    filter(grepl(date_val, date))
  
 date_value <- value$Growth 
}
