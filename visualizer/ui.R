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
                   theme = "bootstrap.css",
                   
 tabPanel(strong("Map"),
  div(class = "outer",
      
   
   tags$head(
    # Include our custom CSS
    includeCSS("styles.css"),
    includeScript("gomap.js")
    ),
   tags$head(includeScript("google-analytics.js")),
   
   leafletOutput("map", width = "100%", height ="100%"),
   
   absolutePanel(
     id = "controls", class = "panel panel-default", fixed = TRUE,
     draggable = TRUE, bottom = 10, left = "auto", right = 10, top = "auto",
     width = 310, height = "auto",
     HTML('<button data-toggle="collapse" data-target="#demo">Pollutant Information</button>'),
     tags$div(id = 'demo',  class="collapse in",
              htmlOutput('poldesc'))),
   
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
    
    h3("San Ysidro Air Explorer"), 
    dateInput("date", label = h4("Date input"), value = max(data_wide$date_day, na.rm=T)),
    selectInput("color", h4("Pollutant"), vars, selected="pm25")
    ),
  
   absolutePanel(
    id = "cite", 
    class = "panel panel-default", fixed = TRUE,
    draggable = TRUE, bottom = 10, left = 10, right = "auto", top = "auto",
    width = "auto", height = "auto",
    tags$b("The website is currently in BETA mode. All data is preliminary and may be updated at a later time."), tags$br(),
    'Data compiled for the ',
    tags$a(href="http://deohs.washington.edu/syairstudy",
           tags$em('San Ysidro Community Air Monitoring Data Project'), target="_blank"),
    tags$br(),
    tags$a(href="http://deohs.washington.edu/faculty/seto_edmund", target="_blank",
           "Principle Investigator: Edmund Seto at University of Washington"),
    tags$br(),"Funded by the ", 
    tags$a(href="https://oehha.ca.gov/","Office of Environmental Health Hazard Assessment(OEHHA)",
           target="_blank"),
    tags$br(),"Created by Elena Austin at University of Washington"
   )
  )
 ),
  tabPanel(strong("Hourly Data"),
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
               
               plotOutput("tsPoll", height = 400), #plotOutput("scatterCollegeIncome", height = 250)
               tags$hr(),
               strong("The data presented here is NOT regulatory data and errors may exist."),
               strong(tags$a(href="http://sd.sdapcd.org/Airvision/",
                        "Please follow current outdoor conditions and health recommendations from 
                                the San Diego County APCD.", target="_blank")),
               tags$hr(), 
               p(tags$strong("Interpreting this data with respect to government health based standards:")),
               tags$p(textOutput("tsNotation"))
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
   strong("Data explorer"),
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
