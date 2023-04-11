library(shiny)
library(tidyverse)


source("helpers.R")
source("ui.R")

####Intro Modal####
server <- function(input, output, session) {
  
  rv <- reactiveValues()
  
  shinyalert(
    title = "Introduction",
    text = 
      '<div align = "left">CLT Assessment Tool:</div>
        </br>
        <div align = "left"><ul>
              <li>The data explorer is an interactive resource 
              that equips Grounded Solutions Network staff with data 
              to streamline their initial feasibility assessment process. 
              The tool includes a dashboard that displays processed data specific to the project location, 
              and a compilation of external websites that can be visited within the data tool itself.</li>
            <ul></div>
        </br>
        <div align = "left">To start, select one of the tabs on the left.</div>', 
    html = TRUE,
    type = "info",
    closeOnClickOutside = TRUE
  )
  
source("load_data.R")
  
##----CENSUS ZIP SEARCH---------------------------------------------------------
#
# Description: census zip search
#____________________________________________________________________________
  
  observeEvent(input$zip_search_button, {
    
    user_input <- input$zip_entry
    
    #selected geo box 
    output$geoBox <- renderInfoBox({
      infoBox(
        "Selected Geography", user_input, icon = icon("map"),
        color = "purple"
      )
    })
    
    #saving the city 
    rv$place <- user_input
    
    #checking its a valid zipcode
    validationResult <- validateRequiredInput(inputData = user_input)
    
    if(validationResult) {
      
    #these are the outputs 
    
    
    #race/ethnicity table 
    output$race_pie <- race_table_gen(race_eth, user_input) 
    
    ##react version of race/eth##
    race_table <- race_table_report(race_eth, user_input)
    rv$race_eth <- race_table_react(race_table)
    
    #household characteristics
    output$occupancy_table <- hhchar_table_gen(hh_char, user_input, "test", "HOUSING OCCUPANCY")
    output$tenure_table <- hhchar_table_gen(hh_char, user_input, "test","HOUSING TENURE")
    output$gross_rent_table <- hhchar_table_gen(hh_char, user_input, "test","GROSS RENT")
    output$grapi_table <- hhchar_table_gen(hh_char, user_input, "test","GROSS RENT AS A PERCENTAGE OF HOUSEHOLD INCOME (GRAPI)")
    output$smocapi_table <- hhchar_table_gen(hh_char, user_input,"test", "SELECTED MONTHLY OWNER COSTS AS A PERCENTAGE OF HOUSEHOLD INCOME (SMOCAPI)")
    output$value_table <- hhchar_table_gen(hh_char, user_input, "test","VALUE")
    
    ##react version of hh char##
    hhchar_table <- hhchar_table_report(hh_char, user_input)
    rv$hh_char_react <- hhchar_react_report(hhchar_table)
    
    
    #med income box
    output$med_income_box <- renderInfoBox({
      infoBox(
        "Median Household Income",
        income_med_gen(med_mean_income, user_input),
        icon = icon("dollar-sign"),
        color = "green"
      )
    })
    
    #react med inc
    rv$med_inc <- income_med_gen(med_mean_income, user_input)
    
    #avg income box
    output$avg_income_box <- renderInfoBox({
      infoBox(
        "Average Household Income",
        income_avg_gen(med_mean_income, user_input),
        icon = icon("dollar-sign"),
        color = "green"
      )
    })
    
    #react avg inc
    rv$avg_inc <- income_avg_gen(med_mean_income, user_input)
    
    #income bands
    output$income_band_table <- income_table_gen(income_bands, user_input)
    
    ##react hh bands
    hhband_table <- income_table_report(income_bands,user_input)
    rv$hh_band <- income_react_report(hhband_table)
    
    #income by race
    output$race_income_table <- inc_race_gen(race_income_bands_zip, user_input)
  
    
    #react income by race
    race_inc_table <- inc_race_report(race_income_bands_zip, user_input)
    rv$inc_race <- inc_race_react(race_inc_table)
    
    #renter/owner by race
    output$rent_own_table <- rent_own_gen(rent_own_zip, user_input)
    
    #react renter/owner by race
    rent_own_table <- rent_own_report(rent_own_zip, user_input)
    rv$rent_own <- rent_own_react(rent_own_table)

    ##table of pct growth 
    pct_table <- pct_change_val(home_val_five, user_input)
    
    rv$pct_change_table <- pct_change_react(pct_table)
    
    output$pct_change_five <- renderDT({rv$pct_change_table}) 
    
    #show tables
    shinyjs::show("race_wrapper")
    shinyjs::show("hhchar_wrapper")
    shinyjs::show("income_band_wrapper")
    shinyjs::show("med_mean_wrapper")
    shinyjs::show("med_inc_box_wrapper")
    shinyjs::show("avg_inc_box_wrapper")
    
    }#validation result 
  }) #census zip search observe event

##----CENSUS PLACE SEARCH---------------------------------------------------------
#
# Description: filters drop down menu & searches by place 
#____________________________________________________________________________
output$city_entry <- renderUI({
  city_choices <- city_state_list %>% 
    filter(State == input$state_entry)
  
  city_choices <- unique(city_choices$City)
  
  selectizeInput("place_entry", "Select a Town/City",
                 choices = city_choices)
})
  
  observeEvent(input$state_city_search, {
    
    user_input <- input$place_entry
    state_input <- input$state_entry
    
    #shiny success
    shinyalert(
      title = "Success",
      text = paste("Successful search!"),
      type = "success"
    )
    
    #saving the city 
    temp_place <- paste(user_input, input$state_entry, sep = ", ")
    rv$place <- temp_place
  
    #selected geo box 
    output$geoBox <- renderInfoBox({
      infoBox(
        "Selected Geography", rv$place, icon = icon("map"),
        color = "purple"
      )
    })
    
    #these are the outputs 
    
    #race/ethnicity table 
    output$race_pie <- race_table_gen(race_eth_place, user_input, state_input)
    
    ##react version of race/eth##
    race_table <- race_table_report(race_eth_place, user_input, state_input)
    rv$race_eth <- race_table_react(race_table)
      
    #household characteristics
    output$occupancy_table <- hhchar_table_gen(hh_char_place, user_input, state_input, "HOUSING OCCUPANCY")
    output$tenure_table <- hhchar_table_gen(hh_char_place, user_input, state_input, "HOUSING TENURE")
    output$gross_rent_table <- hhchar_table_gen(hh_char_place, user_input, state_input,"GROSS RENT")
    output$grapi_table <- hhchar_table_gen(hh_char_place, user_input, state_input,"GROSS RENT AS A PERCENTAGE OF HOUSEHOLD INCOME (GRAPI)")
    output$smocapi_table <- hhchar_table_gen(hh_char_place, user_input, state_input,"SELECTED MONTHLY OWNER COSTS AS A PERCENTAGE OF HOUSEHOLD INCOME (SMOCAPI)")
    output$value_table <- hhchar_table_gen(hh_char_place, user_input, state_input,"VALUE")
  
    ##react version of hh char##
    hhchar_table <- hhchar_table_report(hh_char_place, user_input,state_input)
    rv$hh_char_react <- hhchar_react_report(hhchar_table)


    #med income box
    output$med_income_box <- renderInfoBox({
      infoBox(
        "Median Household Income",
        income_med_gen(med_mean_income_place, user_input,state_input),
        icon = icon("dollar-sign"),
        color = "green"
      )
    })

    #react med inc
    rv$med_inc <- income_med_gen(med_mean_income_place, user_input,state_input)

    #avg income box
    output$avg_income_box <- renderInfoBox({
      infoBox(
        "Average Household Income",
        income_avg_gen(med_mean_income_place, user_input,state_input),
        icon = icon("dollar-sign"),
        color = "green"
      )
    })

    #react avg inc
    rv$avg_inc <- income_avg_gen(med_mean_income_place, user_input,state_input)

    #income bands
    output$income_band_table <- income_table_gen(income_bands_place, user_input,state_input)

    ##react hh bands
    hhband_table <- income_table_report(income_bands_place,user_input,state_input)
    rv$hh_band <- income_react_report(hhband_table)
    
    #income by race
    output$race_income_table <- inc_race_gen(race_income_bands_place, user_input,state_input)
    
    #react income by race
    race_inc_table <- inc_race_report(race_income_bands_place, user_input, state_input)
    rv$inc_race <- inc_race_react(race_inc_table)
    
    #renter/owner by race
    output$rent_own_table <- rent_own_gen(rent_own_place, user_input, state_input)
    
    #react renter/owner by race
    rent_own_table <- rent_own_report(rent_own_place, user_input, state_input)
    rv$rent_own <- rent_own_react(rent_own_table)
    
    #show tables
    shinyjs::show("race_wrapper")
    shinyjs::show("hhchar_wrapper")
    shinyjs::show("income_band_wrapper")
    shinyjs::show("med_mean_wrapper")
    shinyjs::show("med_inc_box_wrapper")
    shinyjs::show("avg_inc_box_wrapper")
    

  }) #census city search observe event

  
  
##----REAL ESTATE SEARCH---------------------------------------------------------
#
# Description: sets up real estate market search
#____________________________________________________________________________
  
  
observeEvent(input$market_search_button, {
    
    user_input <- input$zip_entry_market
    rv$place <- user_input 
    
    #checking its a valid zipcode
    validationResult <- validateRequiredInput(inputData = user_input)
    
    if(validationResult) {
      
      #market trends
      output$home_val_table <- val_table_gen(home_val, user_input)
      
      ##react market
      val_table <- val_table_report(home_val,user_input)
      rv$zhvi <- val_react_report(val_table)
      
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
      
      ##react vals of future growth
      rv$zfhg_one <- date_growth(growth_home, user_input, "2023-02-28")
      rv$zfhg_two <- date_growth(growth_home, user_input, "2023-04-30")
      rv$zfhg_three <- date_growth(growth_home, user_input, "2024-01-31")
      
      ##table of pct growth 
      pct_table <- pct_change_val(home_val_five, user_input)
      
      rv$pct_change_table <- pct_change_react(pct_table)
      
      output$pct_change_five <- renderDT({rv$pct_change_table}) 
      
      ##table of avg pct growth
      avg_table <- pct_change_avg(home_avg_five, user_input)
      rv$pct_avg_table <- pct_avg_react(avg_table)
      output$pct_avg_table <- renderDT({rv$pct_avg_table})

    }#validation result
    
    #show tables
    shinyjs::show("market_wrapper")
    shinyjs::show("first_date_wrapper")
    shinyjs::show("second_date_wrapper")
    shinyjs::show("third_date_wrapper")
    shinyjs::show("pct_change_wrapper")
    shinyjs::show("pct_avg_wrapper")
  
    })#market search closer
  

##----SUMMARY REPORT CENSUS DOWNLOAD---------------------------------------------------------
#
# Description: creates a HTML file that contains all the data
#____________________________________________________________________________
  
  output$downloadReport <- downloadHandler(
    filename = function(){
      paste("Summary Report ", rv$place,".html", sep="")
    },
    content = function(file) {
      tempReport <- file.path(tempdir(), "Report.Rmd")
      file.copy("Report.Rmd", tempReport, overwrite = TRUE)
      
      #set up params to pass to Rmd Doc
      params <- list(place = rv$place,
                     race_eth = rv$race_eth,
                     hh_char = rv$hh_char_react,
                     med_inc = rv$med_inc,
                     avg_inc = rv$avg_inc,
                     hh_band = rv$hh_band,
                     inc_race = rv$inc_race,
                     rent_own = rv$rent_own
      )
      
      #knit doc
      rmarkdown::render(tempReport, output_file = file,
                        params = params,
                        envir = new.env(parent=globalenv()))
      
      
    })
  
##----SUMMARY REPORT MARKET DOWNLOAD---------------------------------------------------------
#
# Description: creates a HTML file that contains all the data
#____________________________________________________________________________
  
  output$downloadReportMarket <- downloadHandler(
    filename = function(){
      paste("Summary Report ", rv$place,".html", sep="")
    },
    content = function(file) {
      tempReport <- file.path(tempdir(), "Report - Market.Rmd")
      file.copy("Report - Market.Rmd", tempReport, overwrite = TRUE)
      
      #set up params to pass to Rmd Doc
      params <- list(place = rv$place,
                     zhvi = rv$zhvi,
                     zfhg_one = rv$zfhg_one,
                     zfhg_two = rv$zfhg_two,
                     zfhg_three = rv
                     $zfhg_three,
                     pct_five = rv$pct_change_table,
                     pct_avg = rv$pct_avg_table
      )
      
      #knit doc
      rmarkdown::render(tempReport, output_file = file,
                        params = params,
                        envir = new.env(parent=globalenv()))
      
      
    })
  
   
  
} # server closer

