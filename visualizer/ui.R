library(shiny)
library(leaflet)

options(device = 'cairo')

# Choices for drop-downs
vars <- c(
 "Particulate Matter" = "pm25", #"Is SuperZIP?" = "superzip",
 "Carbon Monoxide" = "CO", #"Centile score" = "centile",
 "Nitrogen Oxide" = "NO",
 "Nitrogen Dioxide" = "NO2",
 "Ozone" = "O3"
)


shinyUI(navbarPage("BETA ---- Community Monitoring --- BETA", id = "nav",
                   theme = "bootstrap.css",
                   
 
  
           
 tabPanel(strong("Map / Mapa"),
  div(class = "outer",
      
   
   tags$head(
    # Include our custom CSS
    includeCSS("styles.css"),
    includeScript("gomap.js")
    ),
   tags$head(includeScript("google-analytics.js")),
   
   leafletOutput("map", width = "100%", height ="100%"),
   
   # absolutePanel(
   #   id = "controls", class = "panel panel-default", fixed = TRUE,
   #   draggable = TRUE, bottom = 20, left = "auto", right = 10, top = "auto",
   #   width = 310, height = "auto",
   #   HTML('<button data-toggle="collapse" data-target="#demo">Pollutant Information</button>'),
   #   tags$div(id = 'demo',  class="collapse in",
   #            htmlOutput('poldesc'))),
   
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
    
    h4("Daily Concentration / Concentración Diaria"),
    h4(radioButtons("language",(""),c("English"="en", "Español"="sp"),
                 inline=T)),
    #textOutput("translateMessage"),
    dateInput("date", label = h4("Date / Fecha"), value = max(data_wide$date_day, na.rm=T), min="2017-01-01"),
    
    selectInput("color", label=h4("Pollutant / Contaminante"), vars, selected="pm25"),
   
    HTML('<button data-toggle="collapse" data-target="#demo">Information / Información </button>'),
    tags$div(id = 'demo',  class="collapse", 
    style = "overflow-y:scroll; max-height: 40vh",
    htmlOutput('poldesc'))
    ),
  
   fixedPanel(
     id = "cite", 
     class = "panel panel-default", 
     draggable = F, bottom = 10, left = 10, right = "auto", top = "auto",
     width = "auto", height = "auto",
     HTML('<button data-toggle="collapse" data-target="#cites">&#9733</button>'),
     tags$div(id = 'cites',  class="collapse in",
    htmlOutput('citedesc'))
   )
  )
 ),
  tabPanel(strong("Hourly Data / Datos horarios"),
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
               dateInput("date1", label = "Date / Fecha", value =max(data_wide$date_day, na.rm=T)),
               
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
                 
             
               selectInput("tsvars", label=h4("Pollutant / Contaminante"), vars, selected="pm25")
             ),
             mainPanel(
               
               plotOutput("tsPoll", height = 400),
               tags$hr(),
               
               strong(textOutput("tsNotationtitle")),
               tags$p(textOutput("tsNotation")),
               htmlOutput('notreg'),
               tags$hr(),
               tags$em(strong(h3(tags$a(href="https://docs.google.com/forms/d/1ePYLqa4SyiobJTVRp1_O_3GrZxhOdaKlZqPScGIXnQc/viewform?edit_requested=true",
                                        "Feedback / Comentarios", target="_blank"))))
             )
           )
          #   h2("Hourly Data Plot"), 
          #   dateInput("date1", label = "Date input", value = "2017-01-01"),
          #  checkboxGroupInput("tssites", "Sites:", site_locations$site, selected="Community Center", inline=T),
          #   selectInput("tsvars", "Pollutant:", vars, selected="pm25"),
          # plotOutput("tsPoll", height = 600) #plotOutput("scatterCollegeIncome", height = 250)
 #)
 ),
 
 
 # tabPanel(
 #   strong("Data explorer"),
 #   fluidRow(
 #     column(3,
 #            selectInput(
 #              "sites",#"states",
 #              "Sites",#"States",
 #              c("All Sites"="", site_locations$site), multiple=TRUE)
 #     ),
 #     column(3,
 #            conditionalPanel(
 #              "input.sites", #"input.states",
 #              dateInput("startdate", label = ("Start Date"), 
 #                        value = max(data_wide$date_day, na.rm=T)))),
 #     column(3,
 #            conditionalPanel(
 #              "input.sites", #"input.states",
 #              selectInput("pollutants", "Pollutants", c("All Pollutants" =
 #                            ""), multiple = TRUE)
 #            ))
 #        ),
 #     hr(),
 #   DT::dataTableOutput("sitetable")
 # ),
 
 tabPanel(strong("About / Sobre Nosotros"),
          fluidPage(
           
            mainPanel(
              h3("About the San Ysidro Air Monitoring Study"),
              "Welcome to the web visualization of the San Ysidro Community Air monitoring project. This two-year project, funded by the California Office of Environmental Health Hazard Assessment (OEHHA), has the primary objective of understanding the air pollution health risks faced by the community.",
              h3("Acerca del Estudio de Monitoreo del Aire de San Ysidro"),
              "Bienvenido a la visualización por internet del proyecto de monitoreo del Aire de la Comunidad de San Ysidro. Este proyecto de dos años, financiado por la Oficina de Evaluación de Riesgos a la Salud Ambiental (OEHHA), tiene como objetivo principal la comprensión de los riesgos a la salud por la contaminación ambiental que enfrenta la comunidad.",
              tags$hr(),
              h3("Project Partners / Socios del Proyecto"),
              img(src="SYorgwithLogo.png", width=500),
              tags$hr(),
              h3("Data Access"),
              "This project has an open data policy. For those wishing to access the data files used to create this visualization, please complete the following ",
              
              strong(tags$a(href="https://docs.google.com/forms/d/e/1FAIpQLSe-Wme3vV3eQCHB4KqPxOI2XR-QQm1M3WQmjy-yK2SkblQydg/viewform?usp=sf_link",
                            "webform.", target="_blank")),
              " We do ask that you provide a short description of your intended use of the data. This data is not intended for commercial use.",
             
              h3("Acceso a los Datos"),
              "Este Proyecto tiene una política de datos abierta. Para aquellos que quieren acceso a los archivos de datos utilizados para crear esta visualización, por favor complete el siguiente",
              
              strong(tags$a(href="https://docs.google.com/forms/d/e/1FAIpQLSe-Wme3vV3eQCHB4KqPxOI2XR-QQm1M3WQmjy-yK2SkblQydg/viewform?usp=sf_link",
                            "formato de internet", target="_blank")),
              ". Le pediremos que nos proporciones una descripción corta del uso que pretende darle a los datos. Estos datos no son para uso comercial.",
              tags$hr(),
              tags$em(strong(h3(tags$a(href="https://docs.google.com/forms/d/1ePYLqa4SyiobJTVRp1_O_3GrZxhOdaKlZqPScGIXnQc/viewform?edit_requested=true",
                                       "Feedback / Comentarios", target="_blank"))))
              
              
            )))
 
))
