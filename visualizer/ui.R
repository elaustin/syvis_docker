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
    
    dateInput("date", label = h3("Date input"), value = "2017-01-01"),
    
    
    selectInput("color", "Color", vars, selected="pm25")),
    #selectInput("size", "Size", vars, selected = "pm25"),
    # conditionalPanel(
    #  "input.color == 'superzip' || input.size == 'superzip'",
    #  # Only prompt for threshold when coloring or sizing by superzip
    #  numericInput("threshold", "SuperZIP threshold (top n percentile)", 5)
    # ),
    
   
   
   tags$div(
    id = "cite",
    'Data compiled for ',
    tags$em('San Ysidro Community Air Monitoring Data Project'),
    '\nPI Edmund Seto, University of Washington, Funded by OEHHA.'
   )
  )
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

  tabPanel("Hourly Data",
           absolutePanel(
            id = "controls",
            class = "panel panel-default",
            fixed = TRUE,
            draggable = TRUE,
            top = 60,
            #left = "auto",
            right = "auto",
            bottom = "auto",
            width = 800,
            height = 600,
            h2("Hourly Data Plot"), 
            dateInput("date1", label = "Date input", value = "2017-01-01"),
            selectInput("tsvars", "Pollutant:", vars, selected="pm25"),
          plotOutput("tsPoll", height = 600) #plotOutput("scatterCollegeIncome", height = 250)
 )),
 
 conditionalPanel("false", icon("crosshair"))
))
