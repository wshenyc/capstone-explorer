library(shiny)
library(tidyverse)
library(DT)
library(glue)
library(lubridate)
library(lubridate)
library(shinydashboard)
library(dashboardthemes)
library(shinyalert)
library(echarts4r)

###geographies
city_state_list <- read_csv("city_state_list.csv")

####custom logo####
customLogo <- shinyDashboardLogoDIY(
  boldText = paste(icon("building"), "Capstone")
  ,
  mainText = "Demo"
  ,
  textSize = 20
  ,
  badgeText = "v1.1"
  ,
  badgeTextColor = "white"
  ,
  badgeTextSize = 2
  ,
  badgeBackColor = "#88bed1"
  ,
  badgeBorderRadius = 3
)


####custom theme####
customTheme <- dashboardthemes::shinyDashboardThemeDIY(
  #general
  appFontFamily = 'Verdana'
  ,
  appFontColor = "rgb(0,0,0)"
  ,
  primaryFontColor = "rgb(0,0,0)"
  ,
  infoFontColor = "rgb(0,0,0)"
  ,
  successFontColor = "rgb(0,0,0)"
  ,
  warningFontColor = "rgb(0,0,0)"
  ,
  dangerFontColor = "rgb(0,0,0)"
  ,
  bodyBackColor = "rgb(248,248,248)"
  
  #header
  ,
  #logoBackColor = "rgba(19,156,202,255)"
  logoBackColor = "#004e66"
  
  ,
  #headerButtonBackColor = "rgba(19,156,202,255)"
  headerButtonBackColor = "#024357"
  ,
  headerButtonIconColor = "rgb(255,255,255)"
  #headerButtonIconColor = "#4B4F54"
  ,
  headerButtonBackColorHover = "rgba(18, 140, 183, 1)"
  
  ,
  headerButtonIconColorHover = "rgb(0,0,0)"
  
  ,
  #headerBackColor = "rgba(19,156,202,255)"
  headerBackColor = "#004e66"
  ,
  headerBoxShadowColor = "#aaaaaa"
  ,
  headerBoxShadowSize = "2px 2px 2px"
  
  ### sidebar
  ,
  sidebarBackColor = "rgb(232, 232, 232, 1)"
  ,
  sidebarPadding = 0
  
  ,
  sidebarMenuBackColor = "transparent"
  ,
  sidebarMenuPadding = 0
  ,
  sidebarMenuBorderRadius = 0
  
  ,
  sidebarShadowRadius = "3px 5px 5px"
  ,
  sidebarShadowColor = "#aaaaaa"
  
  ,
  sidebarUserTextColor = "rgb(255,255,255)"
  
  ,
  sidebarSearchBackColor = "rgb(55,72,80)"
  ,
  sidebarSearchIconColor = "rgb(153,153,153)"
  ,
  sidebarSearchBorderColor = "rgb(55,72,80)"
  
  ,
  sidebarTabTextColor = "rgb(50,50,50)"
  ,
  sidebarTabTextSize = 13
  ,
  sidebarTabBorderStyle = "none none solid none"
  ,
  sidebarTabBorderColor = "rgb(209, 209, 209, 1)" ###border tab color
  ,
  sidebarTabBorderWidth = 1
  
  ,
  sidebarTabBackColorSelected = "rgb(209, 209, 209, 1)" #this changes the color of the tab after it's been clicked
  ,
  sidebarTabTextColorSelected = "rgb(0,0,0)"
  ,
  sidebarTabRadiusSelected = "0px 20px 20px 0px"
  
  ,
  sidebarTabBackColorHover = "rgb(209, 209, 209, 1)" #changes color of tab on hover
  ,
  sidebarTabTextColorHover = "rgb(50,50,50)"
  ,
  sidebarTabBorderStyleHover = "none none solid none"
  ,
  sidebarTabBorderColorHover = "rgb(209, 209, 209, 1)"
  ,
  sidebarTabBorderWidthHover = 1
  ,
  sidebarTabRadiusHover = "0px 20px 20px 0px"
  
  ### boxes
  ,
  boxBackColor = "rgb(255,255,255)"
  ,
  boxBorderRadius = 5
  ,
  boxShadowSize = "0px 1px 1px"
  ,
  boxShadowColor = "rgba(0,0,0,.1)"
  ,
  boxTitleSize = 16
  ,
  boxDefaultColor = "rgb(210,214,220)"
  ,
  boxPrimaryColor = "rgb(20,155,203)" #this changes box tab color
  ,
  boxInfoColor = "rgb(210,214,220)"
  ,
  boxSuccessColor = "rgba(0,255,213,1)"
  ,
  boxWarningColor = "rgb(244,156,104)"
  ,
  boxDangerColor = "rgb(255,88,55)"
  
  ,
  tabBoxTabColor = "rgb(255,255,255)"
  #tabBoxTabColor = "#009ECC"
  ,
  tabBoxTabTextSize = 12
  ,
  tabBoxTabTextColor = "rgb(0,0,0)"
  ,
  tabBoxTabTextColorSelected = "rgb(0,0,0)"
  ,
  tabBoxBackColor = "rgb(255,255,255)"
  ,
  tabBoxHighlightColor = "#009ECC"
  ,
  tabBoxBorderRadius = 5
  
  ### inputs
  ,
  buttonBackColor = "rgb(245,245,245)"
  ,
  buttonTextColor = "rgb(0,0,0)"
  ,
  buttonBorderColor = "rgb(200,200,200)"
  ,
  buttonBorderRadius = 5
  
  ,
  buttonBackColorHover = "rgb(235,235,235)"
  ,
  buttonTextColorHover = "rgb(100,100,100)"
  ,
  buttonBorderColorHover = "rgb(200,200,200)"
  
  ,
  textboxBackColor = "rgb(255,255,255)"
  ,
  textboxBorderColor = "rgb(200,200,200)"
  ,
  textboxBorderRadius = 5
  ,
  textboxBackColorSelect = "rgb(245,245,245)"
  ,
  textboxBorderColorSelect = "rgb(200,200,200)"
  
  ### tables
  ,
  tableBackColor = "rgb(255,255,255)"
  ,
  tableBorderColor = "rgb(240,240,240)"
  ,
  tableBorderTopSize = 1
  ,
  tableBorderRowSize = 1
  
)

####sidebar####
sidebar <- dashboardSidebar(
   
  sidebarMenu(
    id = "tabs",
    menuItem(
      "Socioeconomics",
      tabName = "census_search",
      icon = icon("building"),
      menuSubItem(icon = NULL,
                  textInput("zip_entry",
                            "Enter a U.S. Zipcode")),
      
      menuSubItem(icon = NULL,
                  tabName = "census_sub_tab",
                  actionButton("zip_search_button", "Zip Search")), 
      
      # menuSubItem(icon = NULL,
      #             tabName = "market_tab",
      #             actionButton("market_search_button", "Zip Home Value Search")),
      
      menuSubItem(icon = NULL,
                  selectInput("state_entry",
                              "Select a U.S. State",
                              choices = unique(city_state_list$State))),
      
      menuSubItem(icon = NULL, 
                  tabName = "city_tab",
                  uiOutput("city_entry")),

      menuSubItem(icon = NULL,
                  tabName = "state_city_tab",
                  actionButton("state_city_search", "City/Town Search")), 
      
      menuSubItem(
        icon = NULL,
        tabName = "report_tab",
        downloadButton("downloadReport", "Download Report"))
      
      ),#menuitem
      
    menuItem(
      "Real Estate Market",
      tabName = "market_search",
      icon = icon("money-bill"),
      menuSubItem(icon = NULL,
                  textInput("zip_entry_market",
                            "Enter a U.S. Zipcode")),
      menuSubItem(icon = NULL,
                  tabName = "market_tab",
                  actionButton("market_search_button", "Zillow Search")),
      menuSubItem(
        icon = NULL,
        tabName = "report_tab_market",
        downloadButton("downloadReportMarket", "Download Report"))
    ),
    
    # menuItem(
    #   "Subsidy Environment",
    #   tabName = "subsidy_search",
    #   icon = icon("money-bill")
    # ),
    
    menuItem(
      "Political Landscape",
      tabName = "political_search",
      icon = icon("landmark")
      
      # menuSubItem("Electeds Look Up",
      #             icon = icon("check-double"),
      #             tabName="pol_lookup_tab")
    ), 
    
    # menuItem(
    #   "Land Use and Zoning",
    #   tabName = "zoning_search",
    #   icon = icon("landmark")
    # ),
    
    menuItem(
      "Organizational Capacity",
      tabName = "org_search",
      icon = icon("users")
    ),
    
    # menuItem(
    #   "Target Buyer Interest",
    #   tabName = "buyer_search",
    #   icon = icon("money-bill")
    # ),
    
    menuItem(
      "Environmental Risk",
      tabName = "enviro_search",
      icon = icon("seedling"),
      
      menuSubItem("Climate Change Risks",
                  icon = icon("seedling"),
                  tabName = "enviro_risk_tab"),
      
      menuSubItem("Social Vulnerability Index",
                  icon = icon("seedling"),
                  tabName = "svi_tab")
      
      
    ) #menu item
    

  ) #sidebar menu closer
) #dashboard sidebar closer

####body####
body <- dashboardBody(
  customTheme,
  shinyjs::useShinyjs(),
  tabItems(
    tabItem(tabName = "census_sub_tab",
            fluidRow(
              infoBoxOutput("geoBox"),
              infoBoxOutput("med_income_box"), 
              infoBoxOutput("avg_income_box")
            ),#fluidrow closer
                      fluidRow(column(6,
                                      shinyjs::hidden(
                                        div(
                                          id = "race_wrapper",
                                          shinydashboard::box(
                                            width = NULL,
                                            #height = "40em",
                                            title = p("Race/Ethnicity", style = 'font-size:18px;'),
                                            footer = p("Source: 2017-2021 ACS B03002", 
                                                       style = 'font-size:11px; color: gray; font-style: italic;'),
                                            status = "primary",
                                            div(echarts4rOutput('race_pie'), width = "100%")
                                          )
                                        )
                                      )),

                               column(6,
                                      shinyjs::hidden(
                                        div(
                                          id = "hhchar_wrapper",
                                          shinydashboard::tabBox(
                                            width = NULL,
                                            #height = "40em",
                                            #title = p("Household Characteristics", style = 'font-size:18px;'),
                                            id = "hhchar_tab",
                                            footer = p("Source: 2017-2021 ACS DP04", 
                                                       style = 'font-size:11px; color: gray; font-style: italic; margin: 20px;'),
                                            tabPanel("Housing Occupancy", DTOutput("occupancy_table")),
                                            tabPanel("Housing Tenure", DTOutput("tenure_table")),
                                            tabPanel("Gross Rent", DTOutput("gross_rent_table")),
                                            tabPanel("GRAPI", DTOutput("grapi_table")),
                                            tabPanel("SMOCAPI", DTOutput("smocapi_table")),
                                            tabPanel("Owner-Occupied Home Value", DTOutput("value_table"))
                                          )
                                        )
                                      ))
                               ),#fluid row closer

                      fluidRow(
                        column(12,
                             shinyjs::hidden(
                               div(
                                 id = "income_band_wrapper",
                                 shinydashboard::tabBox(
                                   width = NULL,
                                   #title = p("Breakdown of Households by Income Bands", style = 'font-size:18px;'),
                                   footer = p("Source: 2017-2021 ACS S1901, B19001B/D/H/I, S2502", style = 'font-size:11px; color: gray; font-style: italic; margin: 20px'),
                                   #status = "primary",
                                   tabPanel("Breakdown of All Households by Income Bands", echarts4rOutput('income_band_table')),
                                   tabPanel("Breakdown of Households by Income Bands and Race", echarts4rOutput('race_income_table')),
                                   tabPanel("Breakdown of Renters/Owners by Race", DTOutput('rent_own_table'))

                                 )
                               )
                             )
                             ) #column
                      ),#fluidrow closer

                   ), #tab item closer

    tabItem(tabName = "market_tab",
            fluidRow(
              column(12,
                     shinyjs::hidden(
                     div(
                       id = "market_wrapper",
                       shinydashboard::box(
                         width = NULL,
                         title = "Zillow - Market Trends",
                         status = "primary",
                         div(echarts4rOutput('home_val_table'), width =
                               "100%")
                       )
                     )
                     )#shinyjs
              )
            ),#fluidrow closer
            fluidRow(
              column(4,
                     shinyjs::hidden(
                       div(
                         id = "first_date_wrapper",
                         valueBoxOutput("first_date_box", width = 12)
                       )
                     )),
              
              column(4,
                     shinyjs::hidden(
                       div(
                         id = "second_date_wrapper",
                         valueBoxOutput("second_date_box", width = 12)
                       )
                     )),
              
              column(4,
                     shinyjs::hidden(
                       div(
                         id = "third_date_wrapper",
                         valueBoxOutput("third_date_box", width = 12)
                       )
                     ))
            ),#fluidrow closer
            
            fluidRow(
              
              column(6,
                     shinyjs::hidden(
                       div(
                         id = "pct_change_wrapper",
                         shinydashboard::box(
                           width = NULL,
                           title = "Annual Percent Change in Typical Home Values",
                           status = "primary",
                         footer = p("Source: Zillow Home Value Index (Single-Family Homes)", 
                                    style = 'font-size:11px; color: gray; font-style: italic; margin: 20px;'),
                        DTOutput("pct_change_five"))
                       )
                     )),
              
              column(6,
                     shinyjs::hidden(
                       div(
                         id = "pct_avg_wrapper",
                         shinydashboard::box(
                           width = NULL,
                           title = "Average Percent Change in Typical Home Values",
                           status = "primary",
                           footer = p("Source: Zillow Home Value Index (Single-Family Homes)", 
                                      style = 'font-size:11px; color: gray; font-style: italic; margin: 20px;'),
                           DTOutput("pct_avg_table"))
                       )
                     ))
              
            )#fluidrow closer
            
    ),#tabitem closer

    
    tabItem(tabName = "political_search",
            fluidRow(
              column(12,
                     #shinyjs::hidden(
                       div(
                         id = "elected_look_up",
                         shinydashboard::box(
                           width = NULL,
                           title = "Elected Officials Look Up",
                           status = "primary",
                           div(
                             tags$iframe(
                               src = "https://myreps.datamade.us/",
                               height = "800px", width = "100%"
                             )
                           )
                         )
                       )
                     #)#shinyjs
                     )
            )#fluidrow closer
      
    ),#tabitem closer
    
    tabItem(tabName = "org_search",
            fluidRow(
              column(12,
                     #shinyjs::hidden(
                     div(
                       id = "irs_look_up",
                       shinydashboard::box(
                         width = NULL,
                         title = "IRS 990 Look Up",
                         status = "primary",
                         div(
                           tags$iframe(
                             src = "https://apps.irs.gov/app/eos/",
                             height = "800px", width = "100%"
                           )
                         )
                       )
                     )
                     #)#shinyjs
              )
            )#fluidrow closer
            
    ),#tabitem closer
    
    tabItem(
      tabName = "enviro_risk_tab",
      fluidRow(
        column(12,
               #shinyjs::hidden(
               div(
                 id = "enviro_risk_look_up",
                 shinydashboard::box(
                   width = NULL,
                   title = "Climate Change Risks",
                   status = "primary",
                   div(
                     tags$iframe(
                       src = "https://riskfactor.com/",
                       height = "800px", width = "100%"
                     )
                   )
                 )
               )
        )
      )#fluidrow closer
    ),#tabitem closer
    
    tabItem(tabName = "svi_tab",
            fluidRow(
              column(12,
                     #shinyjs::hidden(
                     div(
                       id = "svi_lookup",
                       shinydashboard::box(
                         width = NULL,
                         title = "CDC Social Vulnerability Index",
                         status = "primary",
                         div(
                           tags$iframe(
                             src = "https://www.atsdr.cdc.gov/placeandhealth/svi/interactive_map.html",
                             height = "800px", width = "100%"
                           )
                         )
                       )
                     )
                     #)#shinyjs
              )
            )#fluidrow closer
            
    )#tabitem closer
    

                  )#tab items closer
                  )#dashboard body

#dashboard page
dashboardPage(
  title = "Capstone Demo",
  header = dashboardHeader(title = tags$a(href="https://groundedsolutions.org/",
                                          tags$img(src='https://groundedsolutions.org/themes/custom/groundwork/patternlab/public/images/grounded-solutions-network-logo-white.svg',
                                                   height = '40px'))),
  sidebar,
  body
)