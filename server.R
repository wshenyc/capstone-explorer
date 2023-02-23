library(shiny)
library(tidyverse)

source("ui.R")
source("helpers.R")
source("load_data.R")

server <- function(input, output, session) {
  shinyalert(
    title = "Introduction",
    text = 
      '<div align = "left">Capstone Data Explorer Demo:</div>
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
  
##----REAL ESTATE SEARCH---------------------------------------------------------
#
# Description: sets up real estate market search
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
    output$hhchar_table <- hhchar_table_gen(hh_char, user_input) 
    
    
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
    
  })
  

  
} # server closer

