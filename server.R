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
      '<div align = "left">
              <p>The data explorer is an interactive resource 
              that equips Grounded Solutions Network staff with data 
              to streamline their initial feasibility assessment process. 
              The tool includes a dashboard that displays processed data specific to the project location, 
              and a compilation of external websites that can be visited within the data tool itself.</p>
            </div>
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
  
    
    #saving the city 
    rv$place <- user_input
    
    #checking its a valid zipcode
    validationResult <- validateRequiredInput(inputData = user_input)
    
    if(validationResult) {
      
    #these are the outputs 
    #selected geo box 
      output$geoBox <- renderInfoBox({
        infoBox(
          "Selected Geography", user_input, icon = icon("map"),
          color = "purple"
        )
      })
      
      #progress bar#
      shiny::withProgress(
        message = paste0("Downloading data"),
        value = 0,
        {
          shiny::incProgress(1/10)
          

    
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
    
    #increment progress bar 
    shiny::incProgress(3/10)
    
    
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
    
    #increment progress bar 
    shiny::incProgress(6/10)
    
    
    #renter/owner by race
    output$rent_own_table <- rent_own_gen(rent_own_zip, user_input)
    
    #react renter/owner by race
    rent_own_table <- rent_own_report(rent_own_zip, user_input)
    rv$rent_own <- rent_own_react(rent_own_table)

    ##table of pct growth 
    pct_table <- pct_change_val(home_val_five, user_input)
    
    rv$pct_change_table <- pct_change_react(pct_table)
    
    output$pct_change_five <- renderDT({rv$pct_change_table}) 
    
    #increment progress bar 
    shiny::incProgress(10/10)
    
        }#progress bar
      )#progress bar
    
    
    
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
  
  selectizeInput("place_entry", "Select a City/Town",
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
    output$geoBox_place <- renderInfoBox({
      infoBox(
        "Selected Geography", rv$place, icon = icon("map"),
        color = "purple"
      )
    })
    
    #progress bar
    shiny::withProgress(
      message = paste0("Downloading data"),
      value = 0,
      {
        shiny::incProgress(1/10)
  

    #these are the outputs 
    
    #race/ethnicity table 
    output$race_pie_place <- race_table_gen(race_eth_place, user_input, state_input)
    
    ##react version of race/eth##
    race_table <- race_table_report(race_eth_place, user_input, state_input)
    rv$race_eth <- race_table_react(race_table)
      
    #household characteristics
    output$occupancy_table_place <- hhchar_table_gen(hh_char_place, user_input, state_input, "HOUSING OCCUPANCY")
    output$tenure_table_place <- hhchar_table_gen(hh_char_place, user_input, state_input, "HOUSING TENURE")
    output$gross_rent_table_place <- hhchar_table_gen(hh_char_place, user_input, state_input,"GROSS RENT")
    output$grapi_table_place <- hhchar_table_gen(hh_char_place, user_input, state_input,"GROSS RENT AS A PERCENTAGE OF HOUSEHOLD INCOME (GRAPI)")
    output$smocapi_table_place <- hhchar_table_gen(hh_char_place, user_input, state_input,"SELECTED MONTHLY OWNER COSTS AS A PERCENTAGE OF HOUSEHOLD INCOME (SMOCAPI)")
    output$value_table_place <- hhchar_table_gen(hh_char_place, user_input, state_input,"VALUE")
  
    ##react version of hh char##
    hhchar_table <- hhchar_table_report(hh_char_place, user_input,state_input)
    rv$hh_char_react <- hhchar_react_report(hhchar_table)
    
    #increment progress bar 
    shiny::incProgress(3/10)
    


    #med income box
    output$med_income_box_place <- renderInfoBox({
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
    output$avg_income_box_place <- renderInfoBox({
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
    output$income_band_table_place <- income_table_gen(income_bands_place, user_input,state_input)

    ##react hh bands
    hhband_table <- income_table_report(income_bands_place,user_input,state_input)
    rv$hh_band <- income_react_report(hhband_table)
    
    #income by race
    output$race_income_table_place <- inc_race_gen(race_income_bands_place, user_input,state_input)
    
    #react income by race
    race_inc_table <- inc_race_report(race_income_bands_place, user_input, state_input)
    rv$inc_race <- inc_race_react(race_inc_table)
    
    #increment progress bar 
    shiny::incProgress(6/10)
    
    
    #renter/owner by race
    output$rent_own_table_place <- rent_own_gen(rent_own_place, user_input, state_input)
    
    #react renter/owner by race
    rent_own_table <- rent_own_report(rent_own_place, user_input, state_input)
    rv$rent_own <- rent_own_react(rent_own_table)
    
    #increment progress bar 
    shiny::incProgress(10/10)
    
      }#progress bar
    )#progress bar 
    
    
    
    #show tables
    shinyjs::show("race_wrapper_place")
    shinyjs::show("hhchar_wrapper_place")
    shinyjs::show("income_band_wrapper_place")
    shinyjs::show("med_mean_wrapper_place")
    shinyjs::show("med_inc_box_wrapper_place")
    shinyjs::show("avg_inc_box_wrapper_place")
    

  }) #census city search observe event

  
  
##----REAL ESTATE ZIP SEARCH---------------------------------------------------------
#
# Description: sets up real estate market zip search
#____________________________________________________________________________
  
  
observeEvent(input$market_search_button, {
    
    user_input <- input$zip_entry_market
    rv$place <- user_input 
    
    #checking its a valid zipcode
    validationResult <- validateRequiredInput(inputData = user_input)
    
    if(validationResult) {
      
      #selected geo box 
      output$geoBox_zillow <- renderInfoBox({
        infoBox(
          "Selected Geography", user_input, icon = icon("map"),
          color = "purple"
        )
      })
      
      #progress bar
      shiny::withProgress(
        message = paste0("Downloading data"),
        value = 0,
        {
          shiny::incProgress(1/10)
          
      
      #market trends
      output$home_val_table <- val_table_gen(home_val, user_input)
      output$onebd_val_table <- val_table_gen(onebd_zhvi, user_input)
      output$twobd_val_table <- val_table_gen(twod_zhvi, user_input)
      output$threebd_val_table <- val_table_gen(threebd_zhvi, user_input)
      output$fourbd_val_table <- val_table_gen(fourbd_zhvi, user_input)
      output$fivebd_val_table <- val_table_gen(fivebd_zhvi, user_input)
      
      ##react market
      val_table <- val_table_report(home_val,user_input)
      rv$zhvi <- val_react_report(val_table)
      
      #by bedroom size
      onebd_val_table <- val_table_report(onebd_zhvi,user_input)
      rv$zhvi_onebd <- val_react_report(onebd_val_table)
      
      twobd_val_table <- val_table_report(twod_zhvi,user_input)
      rv$zhvi_twobd <- val_react_report(twobd_val_table)
      
      threebd_val_table <- val_table_report(threebd_zhvi,user_input)
      rv$zhvi_threebd <- val_react_report(threebd_val_table)
      
      fourbd_val_table <- val_table_report(fourbd_zhvi,user_input)
      rv$zhvi_fourbd <- val_react_report(fourbd_val_table)
      
      fivebd_val_table <- val_table_report(fivebd_zhvi,user_input)
      rv$zhvi_fivebd <- val_react_report(fivebd_val_table)
      
      #progress bar
          shiny::incProgress(3/10)
          
      
      #growth trends
      
      #2nd date
      output$second_date_box <- renderValueBox({
        valueBox(
          paste(date_growth(growth_home, user_input, "2023-06-30"), '%', sep = ""),
          "Forecasted Growth for All Types of Homes for Jun 30, 2023",
          icon = icon("dollar-sign"),
          color = "blue"
   
        )
      })

      #3rd date
      output$third_date_box <- renderValueBox({
        valueBox(
          paste(date_growth(growth_home, user_input, "2024-03-31"), '%', sep = ''),
          "Forecasted Growth for All Types of Homes for Mar 31, 2024",
          icon = icon("dollar-sign"),
          color = "blue"
        )
      })
      
      ##react vals of future growth
      rv$zfhg_one <- date_growth(growth_home, user_input, "2023-04-30")
      rv$zfhg_two <- date_growth(growth_home, user_input, "2023-06-30")
      rv$zfhg_three <- date_growth(growth_home, user_input, "2024-03-31")
      
      shiny::incProgress(6/10)
      
      
      ##table of pct growth 
      pct_table <- pct_change_val(home_val_five, user_input)
      
      rv$pct_change_table <- pct_change_react(pct_table)
  
      output$pct_change_five <- renderDT({rv$pct_change_table}) 
      
      #pct growth by bedroom size
      onebd_pct_table <- pct_change_val(onebd_five_year_growth, user_input)
      rv$onebd_pct_change_table <- pct_change_react(onebd_pct_table)
      output$onebd_change_five <- renderDT({rv$onebd_pct_change_table}) 
      
      twobd_pct_table <- pct_change_val(twobd_five_year_growth, user_input)
      rv$twobd_pct_change_table <- pct_change_react(twobd_pct_table)
      output$twobd_change_five <- renderDT({rv$twobd_pct_change_table}) 
      
      threebd_pct_table <- pct_change_val(threebd_five_year_growth, user_input)
      rv$threebd_pct_change_table <- pct_change_react(threebd_pct_table)
      output$threebd_change_five <- renderDT({rv$threebd_pct_change_table}) 
      
      fourbd_pct_table <- pct_change_val(fourbd_five_year_growth, user_input)
      rv$fourbd_pct_change_table <- pct_change_react(fourbd_pct_table)
      output$fourbd_change_five <- renderDT({rv$fourbd_pct_change_table}) 
      
      fivebd_pct_table <- pct_change_val(fivebd_five_year_growth, user_input)
      rv$fivebd_pct_change_table <- pct_change_react(fivebd_pct_table)
      output$fivebd_change_five <- renderDT({rv$fivebd_pct_change_table}) 
      
      ##table of avg pct growth
      avg_table <- pct_change_avg(home_avg_five, user_input)
      rv$pct_avg_table <- pct_avg_react(avg_table)
      output$pct_avg_table <- renderDT({rv$pct_avg_table})
      
      #bedroom size avg pct growth
      onebd_avg_table <- pct_change_avg(onebd_avg_growth, user_input)
      rv$onebd_pct_avg_table <- pct_avg_react(onebd_avg_table)
      output$onebd_pct_avg_table <- renderDT({rv$onebd_pct_avg_table})
      
      twobd_avg_table <- pct_change_avg(twobd_avg_growth, user_input)
      rv$twobd_pct_avg_table <- pct_avg_react(twobd_avg_table)
      output$twobd_pct_avg_table <- renderDT({rv$twobd_pct_avg_table})
      
      threebd_avg_table <- pct_change_avg(threebd_avg_growth, user_input)
      rv$threebd_pct_avg_table <- pct_avg_react(threebd_avg_table)
      output$threebd_pct_avg_table <- renderDT({rv$threebd_pct_avg_table})
      
      fourbd_avg_table <- pct_change_avg(fourbd_avg_growth, user_input)
      rv$fourbd_pct_avg_table <- pct_avg_react(fourbd_avg_table)
      output$fourbd_pct_avg_table <- renderDT({rv$fourbd_pct_avg_table})
      
      fivebd_avg_table <- pct_change_avg(fivebd_avg_growth, user_input)
      rv$fivebd_pct_avg_table <- pct_avg_react(fivebd_avg_table)
      output$fivebd_pct_avg_table <- renderDT({rv$fivebd_pct_avg_table})
      
      shiny::incProgress(10/10)
   
      
        }#progress bar
      )#progress bar

    }#validation result 
    
   
    
    #show tables
    shinyjs::show("market_wrapper")
    shinyjs::show("pct_change_wrapper")
    shinyjs::show("pct_avg_wrapper")
  
    })#market search closer
  
##----REAL ESTATE MSA SEARCH---------------------------------------------------------
#
# Description: sets up real estate market MSA search
#____________________________________________________________________________
  
  
  observeEvent(input$msa_search_button, {
    
    user_input <- input$msa_entry
    rv$place <- user_input 
    
    #shiny success
    shinyalert(
      title = "Success",
      text = paste("Successful search!"),
      type = "success"
    )
    
    #selected geo box 
    output$geoBox_zillow_msa <- renderInfoBox({
      infoBox(
        "Selected Geography", user_input, icon = icon("map"),
        color = "purple"
      )
    })
    
    #progress bar 
    shiny::withProgress(
      message = paste0("Downloading data"),
      value = 0,
      {
        shiny::incProgress(1/10)
        
       
      
      #market trends
      output$home_val_table_msa <- val_table_gen(home_val_metro, user_input)
      output$onebd_val_table_msa <- val_table_gen(onebd_zhvi_metro, user_input)
      output$twobd_val_table_msa <- val_table_gen(twobd_zhvi_metro, user_input)
      output$threebd_val_table_msa <- val_table_gen(threebd_zhvi_metro, user_input)
      output$fourbd_val_table_msa <- val_table_gen(fourbd_zhvi_metro, user_input)
      output$fivebd_val_table_msa <- val_table_gen(fivebd_zhvi_metro, user_input)
      
      ##react market
      val_table <- val_table_report(home_val_metro,user_input)
      rv$zhvi <- val_react_report(val_table)
      
      #by bedroom size
      onebd_val_table <- val_table_report(onebd_zhvi_metro,user_input)
      rv$zhvi_onebd <- val_react_report(onebd_val_table)
      
      twobd_val_table <- val_table_report(twobd_zhvi_metro,user_input)
      rv$zhvi_twobd <- val_react_report(twobd_val_table)
      
      threebd_val_table <- val_table_report(threebd_zhvi_metro,user_input)
      rv$zhvi_threebd <- val_react_report(threebd_val_table)
      
      fourbd_val_table <- val_table_report(fourbd_zhvi_metro,user_input)
      rv$zhvi_fourbd <- val_react_report(fourbd_val_table)
      
      fivebd_val_table <- val_table_report(fivebd_zhvi_metro,user_input)
      rv$zhvi_fivebd <- val_react_report(fivebd_val_table)
      
      #increment progress bar 
      shiny::incProgress(3/10)
      
      
      #growth trends
      
      #2nd date
      output$second_date_box_msa <- renderValueBox({
        valueBox(
          paste(date_growth(growth_metro, user_input, "2023-06-30"), '%', sep = ""),
          "Forecasted Growth for All Types of Homes for Jun 30, 2023",
          icon = icon("dollar-sign"),
          color = "blue"
        )
      })
      
      #3rd date
      output$third_date_box_msa <- renderValueBox({
        valueBox(
          paste(date_growth(growth_metro, user_input, "2024-03-31"), '%', sep = ''),
          "Forecasted Growth for All Types of Homes for Mar 31, 2024",
          icon = icon("dollar-sign"),
          color = "blue"
        )
      })
      
      ##react vals of future growth
      rv$zfhg_one <- date_growth(growth_metro, user_input, "2023-04-30")
      rv$zfhg_two <- date_growth(growth_metro, user_input, "2023-06-30")
      rv$zfhg_three <- date_growth(growth_metro, user_input, "2024-03-31")
      
      #increment progress bar 
      shiny::incProgress(6/10)
      
      
      ##table of pct growth 
      pct_table <- pct_change_val(home_val_five_metro, user_input)
      
      rv$pct_change_table <- pct_change_react(pct_table)
      
      output$pct_change_five_msa <- renderDT({rv$pct_change_table}) 
      
      #pct growth by bedroom size
      onebd_pct_table <- pct_change_val(onebd_five_year_growth_metro, user_input)
      rv$onebd_pct_change_table <- pct_change_react(onebd_pct_table)
      output$onebd_change_five_msa <- renderDT({rv$onebd_pct_change_table}) 
      
      twobd_pct_table <- pct_change_val(twobd_five_year_growth_metro, user_input)
      rv$twobd_pct_change_table <- pct_change_react(twobd_pct_table)
      output$twobd_change_five_msa <- renderDT({rv$twobd_pct_change_table}) 
      
      threebd_pct_table <- pct_change_val(threebd_five_year_growth_metro, user_input)
      rv$threebd_pct_change_table <- pct_change_react(threebd_pct_table)
      output$threebd_change_five_msa <- renderDT({rv$threebd_pct_change_table}) 
      
      fourbd_pct_table <- pct_change_val(fourbd_five_year_growth_metro, user_input)
      rv$fourbd_pct_change_table <- pct_change_react(fourbd_pct_table)
      output$fourbd_change_five_msa <- renderDT({rv$fourbd_pct_change_table}) 
      
      fivebd_pct_table <- pct_change_val(fivebd_five_year_growth_metro, user_input)
      rv$fivebd_pct_change_table <- pct_change_react(fivebd_pct_table)
      output$fivebd_change_five_msa <- renderDT({rv$fivebd_pct_change_table}) 
      
      ##table of avg pct growth
      avg_table <- pct_change_avg(home_avg_five_metro, user_input)
      rv$pct_avg_table <- pct_avg_react(avg_table)
      output$pct_avg_table_msa <- renderDT({rv$pct_avg_table})
      
      #bedroom size avg pct growth
      onebd_avg_table <- pct_change_avg(onebd_avg_growth_metro, user_input)
      rv$onebd_pct_avg_table <- pct_avg_react(onebd_avg_table)
      output$onebd_pct_avg_table_msa <- renderDT({rv$onebd_pct_avg_table})
      
      twobd_avg_table <- pct_change_avg(twobd_avg_growth_metro, user_input)
      rv$twobd_pct_avg_table <- pct_avg_react(twobd_avg_table)
      output$twobd_pct_avg_table_msa <- renderDT({rv$twobd_pct_avg_table})
      
      threebd_avg_table <- pct_change_avg(threebd_avg_growth_metro, user_input)
      rv$threebd_pct_avg_table <- pct_avg_react(threebd_avg_table)
      output$threebd_pct_avg_table_msa <- renderDT({rv$threebd_pct_avg_table})
      
      fourbd_avg_table <- pct_change_avg(fourbd_avg_growth_metro, user_input)
      rv$fourbd_pct_avg_table <- pct_avg_react(fourbd_avg_table)
      output$fourbd_pct_avg_table_msa <- renderDT({rv$fourbd_pct_avg_table})
      
      fivebd_avg_table <- pct_change_avg(fivebd_avg_growth_metro, user_input)
      rv$fivebd_pct_avg_table <- pct_avg_react(fivebd_avg_table)
      output$fivebd_pct_avg_table_msa <- renderDT({rv$fivebd_pct_avg_table})
      
    
          #increment progress bar 
          shiny::incProgress(10/10)
        }#progress bar
      )#progress bar 
      
    #show tables
    shinyjs::show("market_wrapper_msa")
    shinyjs::show("pct_change_wrapper_msa")
    shinyjs::show("pct_avg_wrapper_msa")
    
  })#market search closer
  
##----SUMMARY REPORT CENSUS DOWNLOAD---------------------------------------------------------
#
# Description: creates a HTML file that contains all the data
#____________________________________________________________________________
  
  output$downloadReport <- downloadHandler(
    filename = function(){
      paste("Summary Census Report ", rv$place,".html", sep="")
    },
    content = function(file) {
      shiny::withProgress(
        message = paste0("Downloading summary report"),
        value = 0,
        {
          shiny::incProgress(1/10)
          Sys.sleep(0.5)
          shiny::incProgress(5/10)
          Sys.sleep(0.5)
          shiny::incProgress(10/10)
       
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
      
    })
  
##----SUMMARY REPORT MARKET DOWNLOAD---------------------------------------------------------
#
# Description: creates a HTML file that contains all the data
#____________________________________________________________________________
  
  output$downloadReportMarket <- downloadHandler(
    
    filename = function(){
      paste("Summary Zillow Report ", rv$place,".html", sep="")
    },
    content = function(file) {
      shiny::withProgress(
        message = paste0("Downloading summary report"),
        value = 0,
        {
          shiny::incProgress(1/10)
          Sys.sleep(0.5)
          shiny::incProgress(5/10)
          Sys.sleep(0.5)
          shiny::incProgress(10/10)
          
      tempReport <- file.path(tempdir(), "Report - Market.Rmd")
      file.copy("Report - Market.Rmd", tempReport, overwrite = TRUE)
      
      #set up params to pass to Rmd Doc
      params <- list(place = rv$place,
                     zhvi = rv$zhvi,
                     zhvi_onebd = rv$zhvi_onebd,
                     zhvi_twobd = rv$zhvi_twobd,
                     zhvi_threebd = rv$zhvi_threebd,
                     zhvi_fourbd = rv$zhvi_fourbd,
                     zhvi_fivebd = rv$zhvi_fivebd,
                     
                     zfhg_two = rv$zfhg_two,
                     zfhg_three = rv$zfhg_three,
                     
                     pct_five = rv$pct_change_table,
                     onebd_pct_change_table = rv$onebd_pct_change_table,
                     twobd_pct_change_table = rv$twobd_pct_change_table,
                     threebd_pct_change_table = rv$threebd_pct_change_table,
                     fourbd_pct_change_table = rv$fourbd_pct_change_table,
                     fivebd_pct_change_table = rv$fivebd_pct_change_table,
                     
                     pct_avg = rv$pct_avg_table,
                     onebd_pct_avg_table = rv$onebd_pct_avg_table,
                     twobd_pct_avg_table = rv$twobd_pct_avg_table,
                     threebd_pct_avg_table = rv$threebd_pct_avg_table,
                     fourbd_pct_avg_table = rv$fourbd_pct_avg_table,
                     fivebd_pct_avg_table = rv$fivebd_pct_avg_table
      )
      
      #knit doc
      rmarkdown::render(tempReport, output_file = file,
                        params = params,
                        envir = new.env(parent=globalenv()))
      
        })
    })
  
##----DATA DICTIONARY---------------------------------------------------------
#
# Description: opens the data dictionary
#____________________________________________________________________________
  
  observeEvent(input$infoBtn, {
    showModal(
      modalDialog(title = NULL,
                  tags$h4("Data Dictionary"), 
                  data_dict_gen(data_dict_file),
                  tags$h4("Link to Github:", tags$a(href = 'https://github.com/wshenyc/capstone-explorer',
                                                    target="_blank", "Here")),
    )
    )
    
  })


 
} # server closer

