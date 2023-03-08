library(shiny)
library(tidyverse)

source("ui.R")
source("helpers.R")

####Intro Modal####
server <- function(input, output, session) {
  shinyalert(
    title = "Introduction",
    text = 
      '<div align = "left">Grounded Solutions Network Demo:</div>
        </br>
        <div align = "left"><ul>
              <li>Testing out data explorer functions</li>
            <ul></div>
        </br>
        <div align = "left">To start, select one of the tabs on the left.</div>', 
    html = TRUE,
    type = "info",
    closeOnClickOutside = TRUE
  )
  
source("load_data.R")
  
##----CENSUS SEARCH---------------------------------------------------------
#
# Description: sets up census search
#____________________________________________________________________________
  
  observeEvent(input$zip_search_button, {
    
    user_input <- input$zip_entry
    
    #checking its a valid zipcode
    validationResult <- validateRequiredInput(inputData = user_input)
    
    if(validationResult) {
    
    
    #these are the outputs 
    
    #race/ethnicity table 
    output$race_pie <- race_table_gen(race_eth, user_input) 
      
    #household characteristics
    output$occupancy_table <- hhchar_table_gen(hh_char, user_input, "HOUSING OCCUPANCY")
    output$tenure_table <- hhchar_table_gen(hh_char, user_input, "HOUSING TENURE")
    output$gross_rent_table <- hhchar_table_gen(hh_char, user_input, "GROSS RENT")
    output$grapi_table <- hhchar_table_gen(hh_char, user_input, "GROSS RENT AS A PERCENTAGE OF HOUSEHOLD INCOME (GRAPI)")
    output$smocapi_table <- hhchar_table_gen(hh_char, user_input, "SELECTED MONTHLY OWNER COSTS AS A PERCENTAGE OF HOUSEHOLD INCOME (SMOCAPI)")
    output$value_table <- hhchar_table_gen(hh_char, user_input, "VALUE")
    
    
    #med income box
    output$med_income_box <- renderValueBox({
      valueBox(
        income_med_gen(med_mean_income, user_input),
        "Median Household Income",
        icon = icon("dollar-sign"),
        color = "blue"
      )
    })
    
    #avg income box
    output$avg_income_box <- renderValueBox({
      valueBox(
        income_avg_gen(med_mean_income, user_input),
        "Average Household Income",
        icon = icon("dollar-sign"),
        color = "blue"
      )
    })
    
    #income bands
    output$income_band_table <- income_table_gen(income_bands, user_input)
    
    #show tables
    shinyjs::show("race_wrapper")
    shinyjs::show("hhchar_wrapper")
    shinyjs::show("income_band_wrapper")
    shinyjs::show("med_mean_wrapper")
    shinyjs::show("med_inc_box_wrapper")
    shinyjs::show("avg_inc_box_wrapper")
    
    }#validation result 
    
  }) #census search observe event
  
##----REAL ESTATE SEARCH---------------------------------------------------------
#
# Description: sets up real estate market search
#____________________________________________________________________________
  
  
  observeEvent(input$market_search_button, {
    
    user_input <- input$zip_entry
    
    #checking its a valid zipcode
    validationResult <- validateRequiredInput(inputData = user_input)
    
    if(validationResult) {
      
      #market trends
      output$home_val_table <- val_table_gen(home_val, user_input)
      
      #growth trends
      #first date
      output$first_date_box <- renderValueBox({
        valueBox(
          date_growth(growth_home, user_input, "2023-02-28"),
          "Forecasted Growth for Feb 28, 2023",
          icon = icon("dollar-sign"),
          color = "blue"
        )
      })
      
      #2nd date
      output$second_date_box <- renderValueBox({
        valueBox(
          date_growth(growth_home, user_input, "2023-04-30"),
          "Forecasted Growth for Apr 30, 2023",
          icon = icon("dollar-sign"),
          color = "blue"
        )
      })

      #3rd date
      output$third_date_box <- renderValueBox({
        valueBox(
          date_growth(growth_home, user_input, "2024-01-31"),
          "Forecasted Growth for Jan 31, 2024",
          icon = icon("dollar-sign"),
          color = "blue"
        )
      })

    }#validation result
    
    #show tables
    shinyjs::show("market_wrapper")
    shinyjs::show("first_date_wrapper")
    shinyjs::show("second_date_wrapper")
    shinyjs::show("third_date_wrapper")
  
    })#market search closer
  
  
} # server closer

