library(shiny)

source("ui.R")
source("helpers.R")

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
    
    #need to create a validation function
    
    #pulling census data
    df_race <- race_table_pull(user_input)
    df_hhchar <- hhchar_table_pull(user_input)
    
    #these are the outputs 
    
    #race/ethnicity table 
    output$race_table <- race_table_gen(df_race) 
      
    #household characteristics
    output$hhchar_table <- hhchar_table_gen(df_hhchar) 
    
    #show tables
    shinyjs::show("race_wrapper")
    shinyjs::show("hhchar_wrapper")
    
  })
  

  
} # server closer

