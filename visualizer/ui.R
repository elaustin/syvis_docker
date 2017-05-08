library(shiny)
library(leaflet)

options(device = 'cairo')

# Choices for drop-downs
vars <- c(
 "Particle Matter" = "pm25", #"Is SuperZIP?" = "superzip",
 "Carbon Monoxide" = "CO", #"Centile score" = "centile",
 "Nitrogen Oxide" = "NO",
 "Nitrogen Dioxide" = "NO2",
 "Ozone" = "O3"
)


shinyUI(navbarPage("BETA ---- Community Monitoring --- BETA", id = "nav",
 tabPanel("Interactive map",
  div(class = "outer",
   
   tags$head(
    # Include our custom CSS
    includeCSS("styles.css"),
    includeScript("gomap.js")
    ),
   tags$head(includeScript("google-analytics.js")),
   
   leafletOutput("map", width = "100%", height ="100%"),
   
   # Shiny versions prior to 0.11 should use class="modal" instead.
   absolutePanel(
    id = "controls",
    class = "panel panel-default",
    fixed = TRUE,
    draggable = TRUE,
    top = 60,
    #left = "auto",
    right = 120,
    bottom = "auto",
    width = 330,
    height = "auto",
    
    h2("San Ysidro Air Explorer"), #h2("ZIP explorer"),
    
    dateInput("date", label = h3("Date input"), value = max(data_wide$date_day, na.rm=T)),
    
    
    selectInput("color", "Color", vars, selected="pm25")),
    #selectInput("size", "Size", vars, selected = "pm25"),
    # conditionalPanel(
    #  "input.color == 'superzip' || input.size == 'superzip'",
    #  # Only prompt for threshold when coloring or sizing by superzip
    #  numericInput("threshold", "SuperZIP threshold (top n percentile)", 5)
    # ),
    
   
   
   tags$div(
    id = "cite",
    tags$b("The website is currently in BETA mode. All data is preliminary and may be updated at a later time."), tags$br(),
    'Data compiled for the ',
    tags$a(href="http://deohs.washington.edu/syairstudy",
           tags$em('San Ysidro Community Air Monitoring Data Project')),tags$br(),
    "Principle Investigator: Edmund Seto at University of Washington", tags$br(),"Funded by the ", 
    tags$a(href="https://oehha.ca.gov/","Office of Environmental Health Hazard Assessment(OEHHA)"),
    tags$br(),"Created by Elena Austin"
   )
  )
 ),
  tabPanel("Hourly Data",
           # absolutePanel(
           #  id = "controls",
           #  class = "panel panel-default",
           #  fixed = TRUE,
           #  draggable = TRUE,
           #  top = 60,
           #  #left = "auto",
           #  right = "auto",
           #  bottom = "auto",
           #  width = 800,
           #  height = 600,
           
           pageWithSidebar(
             headerPanel('Hourly Data Plot, Select Sites and Dates'),
             sidebarPanel(
               dateInput("date1", label = "Date input", value =max(data_wide$date_day, na.rm=T)),
               
               radioButtons(
                 inputId="radio",
                 label="Site Selection Options:",
                 choices=list(
                   "All",
                   "Manual Select"
                 ),
                 selected="All"),
               
               conditionalPanel(
                 condition = "input.radio != 'All'",
                 checkboxGroupInput(
                   "tssites", 
                   "Show these sites:",
                   choices=site_locations$site )),
                 
             
               selectInput("tsvars", "Pollutant:", vars, selected="pm25")
             ),
             mainPanel(
               
               plotOutput("tsPoll", height = 400) #plotOutput("scatterCollegeIncome", height = 250)
             )
           )
          #   h2("Hourly Data Plot"), 
          #   dateInput("date1", label = "Date input", value = "2017-01-01"),
          #  checkboxGroupInput("tssites", "Sites:", site_locations$site, selected="Community Center", inline=T),
          #   selectInput("tsvars", "Pollutant:", vars, selected="pm25"),
          # plotOutput("tsPoll", height = 600) #plotOutput("scatterCollegeIncome", height = 250)
 #)
 ),
 
 
 tabPanel(
   "Data explorer",
   fluidRow(
     column(3,
            selectInput(
              "sites",#"states",
              "Sites",#"States",
              c("All Sites"="", site_locations$site), multiple=TRUE)
     ),
     column(3,
            conditionalPanel(
              "input.sites", #"input.states",
              dateInput("startdate", label = ("Start Date"), 
                        value = max(data_wide$date_day, na.rm=T)))),
     column(3,
            conditionalPanel(
              "input.sites", #"input.states",
              selectInput("pollutants", "Pollutants", c("All Pollutants" =
                                                          #selectInput("cities", "Cities", c("All cities" =
                                                          ""), multiple = TRUE)
            ))
     # column(3,
     #        conditionalPanel(
     #         "input.states",
     #         selectInput("zipcodes", "Zipcodes", c("All zipcodes" =
     #                                                ""), multiple = TRUE)
     #        ))
   ),
   # fluidRow(column(
   #  1,
   #  numericInput(
   #   "minScore",
   #   "Min score",
   #   min = 0,
   #   max = 100,
   #   value = 0
   #  )
   # ),
   # column(
   #  1,
   #  numericInput(
   #   "maxScore",
   #   "Max score",
   #   min = 0,
   #   max = 100,
   #   value = 100
   #  )
   # )),
   hr(),
   DT::dataTableOutput("sitetable")
 ),
 
 conditionalPanel("false", icon("crosshair"))
))
