
library(RColorBrewer)
library(shiny)
library(leaflet)
library(RColorBrewer)
library(lattice)
library(dplyr)
library(ggplot2)
library(ggthemes)



options(device='cairo')

shinyServer(function(input, output, session) {
 
##create data
  

  
  
  
  data_summR <- reactive ({
    
    data_summary<-data_wide[date_day%in%as.character(input$date),
                            lapply(.SD, FUN = function (x)
                              mean(as.numeric(as.character(x)), na.rm=T)), 
                            .SDcols=c("longitude","latitude","pm25","CO","NO","NO2","O3"),
                            by="site_short"]
    # missing_sites<-
    #   site_locations$site_short[!site_locations$site_short%in%data_summary$site_short]
    # missing_sites<-site_locations[site_short%in%missing_sites]
    # missing_sites<-missing_sites[,c("site_short","latitude","longitude"),with=F]
    # data_summary <<- rbindlist(list(data_summary,missing_sites),fill=T)
    
    quantileval<-as.numeric(quantile(data_wide[,input$tsvars,with=F],na.rm=T, .995))
    maxval<-max(data_wide[,input$tsvars,with=F],na.rm=T)
    updateSliderInput(session, "ylimpm",max= ceiling(maxval), 
                      value=quantileval)
    if(input$tsvars%in%"CO")
      updateSliderInput(session, "ylimpm",max=ceiling(maxval), 
                        value=quantileval, step=0.2)
    
    
    data_summary
    
  })
  
  # datavalts <- reactive ({
  #   if(!is.null(input$date1)){
  #   wanted_date <- as.POSIXct(input$date1)
  #   mindate = as.POSIXct(min(datavals[,date_day]))
  #   maxdate = as.POSIXct(max(datavals[,date_day]))
  #   if((wanted_date-60*24*60) <= mindate)
  #   {
  #     datavals <- getnewdata(wanted_date)
  #   }
  #   
  #   if((wanted_date) > maxdate)
  #   {
  #     datavals <- getnewdata(wanted_date)
  #   }
  #   
  #   datavals <<- datavals
  #   datavals
  #   }
  # })
  
  data <- reactive ({
    
    wanted_date<-as.POSIXct(input$date)
    mindate = as.POSIXct(min(data_wide[,date_day]))
    maxdate = as.POSIXct(max(data_wide[,date_day]))
    if((wanted_date-60*24*60) <= mindate)
    {
      data_wide <- getnewdata(wanted_date)
    }
    
    if((wanted_date) > maxdate & wanted_date <= Sys.time())
    {
      data_wide <- getnewdata(wanted_date)
    }
    
    if(wanted_date > Sys.time()) {
      return()
    }
    
    data_wide <<- data_wide
    data_wide
    
  })
  
  datats <- reactive ({
    
    datavals <<- data() 
    
    if(input$radio == "All"){
      datavals
    } else {
      datavals[site%in%input$tssites]
    }
  })
  
 ## Interactive Map ###########################################
 
 # Create the map
observe({
 output$map <- renderLeaflet({
  leaflet() %>%
   addTiles(
    urlTemplate = "//{s}.tiles.mapbox.com/v3/jcheng.map-5ebohr46/{z}/{x}/{y}.png",
    attribution = 'Maps by <a href="http://www.mapbox.com/">Mapbox</a>'
   ) %>%
   setView(lng = -116.966454, lat = 32.586481, zoom = 12)
 })
 
 # A reactive expression that returns the set of zips that are
 # in bounds right now
 # sitesInBounds <- reactive({
 #  if (is.null(input$map_bounds))
 #   return(data_summary[FALSE,])
 #  bounds <- input$map_bounds
 #  latRng <- range(bounds$north, bounds$south)
 #  lngRng <- range(bounds$east, bounds$west)
 #  subset(data_wide,
 #         latitude >= latRng[1] & latitude <= latRng[2] &
 #          longitude >= lngRng[1] & longitude <= lngRng[2] &
 #          date_day%in%as.character(input$date))
 # })
 # 

 #update not reg text
 output$notreg<-renderText({
   if(input$language=="en"){
   print(paste(strong("The data presented here is NOT regulatory data and errors may exist."),
         strong(tags$a(href="http://sd.sdapcd.org/Airvision/",
                       "Please follow current outdoor conditions and health recommendations from 
                       the San Diego County APCD.", target="_blank"))))
   } else if (input$language=="sp"){
     
     print(paste(strong("Los datos aquí presentados NO son datos reglamentarios y pueden existir errores. "),
                 strong(tags$a(href="http://sd.sdapcd.org/Airvision/",
                               "Por favor siga las recomendaciones actuales de condiciones en exteriores y de salud del APCD del Condado de San Diego.", target="_blank"))))
     
     
     
     
   }
   
   
 })
 #update select input
 observe({
   if(input$language=="sp"){
     vars <- c(
       "Materia Particulada " = "pm25", #"Is SuperZIP?" = "superzip",
       "Monóxido de Carbono" = "CO", #"Centile score" = "centile",
       "Óxido de Nitrógeno" = "NO",
       "Dióxido de Nitrógeno" = "NO2",
       "Ozono" = "O3"
     )
   
   updateSelectInput(session, inputId="color",
                     choices=vars,
                     selected = "pm25"
   )
   updateSelectInput(session, inputId="tsvars",
                     choices=vars,
                     selected = "pm25"
   )
   
   }
   if(input$language=="en"){
     vars <- c(
       "Particulate Matter" = "pm25", #"Is SuperZIP?" = "superzip",
       "Carbon Monoxide" = "CO", #"Centile score" = "centile",
       "Nitrogen Oxide" = "NO",
       "Nitrogen Dioxide" = "NO2",
       "Ozone" = "O3"
     )
     
     updateSelectInput(session, inputId="color",
                       choices=vars,
                       selected = "pm25"
     )
     updateSelectInput(session, inputId="tsvars",
                       choices=vars,
                       selected = "pm25"
     )
     }
 })
 
 
   
output$translateMessage <- reactive({
  ifelse(input$language=="sp","La versión en español está en desarrollo.",
         "")})
         
})
  
  output$citedesc <- renderText({
    if(input$language=="en") {
      print(paste(tags$b("As of April 30, 2018, the CalEPA-funded San Ysidro air quality research project has concluded. We are currently seeking funding from the CARB AB617 program to sustain and upgrade our air quality monitoring in San Ysidro. Until these funds are available, service for the syairstudy.org website will be degraded."), tags$br(),
                  'Data compiled for the ',
                  tags$a(href="http://deohs.washington.edu/syairstudy",
                         tags$em('San Ysidro Community Air Monitoring Data Project'), target="_blank"),
                  tags$br(),
                  'Principal Investigator: ',
                  tags$a(href="http://deohs.washington.edu/faculty/seto_edmund", target="_blank",
                         "Dr. Edmund Seto"),
                  ' at University of Washington',
                  tags$br(),"Visualization by Dr. Elena Austin at University of Washington",
                  tags$br(),"Funded by the ", 
                  tags$a(href="https://oehha.ca.gov/","Office of Environmental Health Hazard Assessment (OEHHA)",
                         target="_blank")
      ))
    }else if(input$language=="sp") {
      print(paste(tags$b("Los datos aquí presentados NO son datos reglamentarios."), tags$br(),
                  tags$b("El sitio de internet está actualmente en modo BETA. Todos los datos son preliminares y pueden actualizarse posteriormente."), tags$br(),
                  'Los datos recopilados por el  ',
                  tags$a(href="http://deohs.washington.edu/syairstudy",
                         tags$em('Proyecto de Datos de Monitoreo del Aire de la Comunidad de San Ysidro'), target="_blank"),
                  tags$br(),
                  'Investigador Principal:  ',
                  tags$a(href="http://deohs.washington.edu/faculty/seto_edmund", target="_blank",
                         "Dr. Edmund Seto"),
                  ' en la Universidad de Washington',
                  tags$br(),"Visualización por Dr. Elena Austin en la Universidad de Washington",
                  tags$br(),"Financiado por la ", 
                  tags$a(href="https://oehha.ca.gov/","Oficina de Evaluación de Riesgos a la Salud Ambiental  (OEHHA)",
                         target="_blank")
      ))
    }
    
  })

output$poldesc<- renderText({
  if(input$language=="en"){
    if(input$color=="O3"){
      print(paste(tags$p("Ozone is the main ingredient of smog. At ground level, ozone is formed when pollutants chemically react in the presence of sunlight. The main sources of ozone are trucks, cars, planes, trains, factories, farms, construction, and dry cleaners."),
                  tags$p("Ozone can irritate the lungs, cause inflammation, and make chronic illnesses worse, even at low levels of exposure. Children and the elderly are sensitive to the effects of ozone. Ozone levels are highest in the afternoon and on hot days People who spend a lot of time outdoors may also be affected by ozone."),
                  tags$a(href="https://www.epa.gov/ozone-pollution",
                         tags$em("For more information about Ozone see the EPA website.", 
                                 target="_blank"))
                  
      ))
    } else if (input$color=="pm25") 
    {print(paste0(
      tags$p(),
      "Particulate matter or PM",
      tags$sub(2.5), 
      " is very small airborne particle pollution (less than 2.5 micrometers), which is less than the thickness of a human hair. PM",
      tags$sub(2.5),
      " is a mixture of particles that can include organic chemicals, dust, soot and metals. These particles can come from cars and trucks, factories, wood burning, and other activities. They can travel deep into the lungs and cause various health problems including heart and lung disease because they are so small.",
      tags$p(),
      "Children, the elderly, and people suffering from heart or lung disease, asthma, or chronic illness are most sensitive to the effects of PM2.5 exposure.",
      tags$br(),
      tags$a(href="https://www.epa.gov/pm-pollution/particulate-matter-pm-basics#PM",                        tags$em(paste0("For more information about PM see the EPA website.")),
             target="_blank")
    )
    )
    } else if (input$color == "NO2")
    {print(paste0(
      tags$p(),
      "Nitrogen Dioxide (NO",
      tags$sub(2), 
      ")  is one of a group of highly reactive gases known as 
      oxides of nitrogen or nitrogen oxides (NO",
      tags$sub("x"),
      "). Other nitrogen oxides include nitrous acid and nitric acid. NO",
      tags$sub(2),
      " is used as the indicator for the larger group of nitrogen oxides.",
      tags$p(),
      "NO",
      tags$sub(2),
      " primarily gets in the air from the burning of fuel. NO",
      tags$sub(2),
      " forms from emissions from cars, trucks and buses, power plants, and off-road equipment.",
      tags$p(),
      "People with asthma, as well as children and the elderly are generally at greater risk for  the health effects of NO",
      tags$sub(2),".",
      tags$br(),
      tags$a(href="https://www.epa.gov/no2-pollution/basic-information-about-no2#What is NO2",                        tags$em(paste0("For more information about Nitrogen Dioxide see the EPA website.")),
             target="_blank")
    )
    )
    } else if (input$color == "NO")
    {print(paste0(
      tags$p(),
      "Nitrogen Oxide (NO) is one of a group of highly reactive gases known as 
  oxides of nitrogen or nitrogen oxides (NO",
      tags$sub("x"),
      "). Other nitrogen oxides include nitrous acid and nitric acid. NO",
      tags$sub(2),
      " is used by calEPA as the indicator for the larger group of nitrogen oxides.",
      tags$p(),
      "NO is a colorless gas and a major component of diesel exhaust.",
      tags$p(),
      "People with asthma, as well as children and the elderly are generally at greater risk for  the health effects of NO.",
      tags$br(),
      tags$a(href="https://www.epa.gov/no2-pollution/basic-information-about-no2#What is NO2",                        tags$em(paste0("For more information about Nitrogen Oxide see the EPA website.")),
             target="_blank")
    )
    )
    } else if (input$color == "CO")
    {print(paste0(
      tags$p(),
      "Carbon Monoxide (CO) is a colorless, odorless gas that can be harmful when 
  inhaled in large amounts. CO is released when something is burned. The greatest sources of CO to outdoor air are cars, trucks and other vehicles or machinery that burn fossil fuels.",
      tags$p(),
      "Very high levels of CO are not likely to occur outdoors. However, when CO levels are elevated outdoors, they can be of particular concern for people with some types of heart disease. These people already have a reduced ability for getting oxygenated blood to their hearts in situations where the heart needs more oxygen than usual.",
      tags$p(),
      tags$br(),
      tags$a(href="https://www.epa.gov/co-pollution/basic-information-about-carbon-monoxide-co-outdoor-air-pollution#What is CO",                        
             tags$em(paste0("For more information about Carbon Monoxide see the EPA website.")),
             target="_blank")
    )
    )
    }
  }  else if(input$language=="sp"){
    if(input$color=="O3"){
      print(paste(tags$p("El ozono (O3) es el ingrediente principal del smog. A nivel del suelo, el ozono se forma cuando los contaminantes reaccionan químicamente en la presencia de la luz solar. Las principales fuentes de ozono son los camiones, autos, aviones, trenes, industrias, granjas, construcción y tintorerías."),
                  tags$p("El ozono puede irritar los pulmones, causar inflamación y empeorar las enfermedades crónicas, incluso a bajos niveles de exposición. Los niños y los ancianos son sensibles a los efectos del ozono. Los niveles de ozono son más altos por las tardes y en los días calurosos. Las personas que pasan mucho tiempo en exteriores pueden también ser afectados por el ozono."),
                  tags$a(href="https://www.epa.gov/ozone-pollution",
                         tags$em("Para mayor información acerca del Ozono, consulte el sitio de internet de la EPA. ", 
                                 target="_blank"))
                  
      ))
    } else if (input$color=="pm25") 
    {print(paste0(
      tags$p(),
      "La materia particulada (PM",
      tags$sub(2.5), 
      ") son partículas muy pequeñas de contaminación aérea (menos de 2.5 micrómetros), lo cual es menos que el grueso de un cabello humano. La PM",
      tags$sub(2.5),
      " 5 es una mezcla de partículas que pueden incluir sustancias químicas orgánicas, hollín y metales. Estas partículas pueden provenir de los autos y camiones, industrias, quema de madera y otras actividades. Pueden desplazarse profundamente en los pulmones y causar diversos problemas de salud incluyendo enfermedad cardíaca y pulmonar porque son muy pequeñas.",
      tags$p(),
      "Los niños, los ancianos y las personas que sufren enfermedad cardíaca o pulmonar, asma o enfermedades crónicas, son más sensibles a los efectos de la exposición a PM2.5.",
      tags$br(),
      tags$a(href="https://www.epa.gov/pm-pollution/particulate-matter-pm-basics#PM",                        
             tags$em(paste0("Para mayor información acerca de PM, consulte el sitio de internet de la EPA.")),
             target="_blank")
    )
    )
    } else if (input$color == "NO2")
    {print(paste0(
      tags$p(),
      "El Dióxido de Nitrógeno (NO",
      tags$sub(2), 
      ") es uno de un grupo de gases altamente reactivos conocidos como óxidos de nitrógeno (NO",
      tags$sub("x"),
      "). Otros óxidos de nitrógeno incluyen ácido nitroso y ácido nítrico. El NO",
      tags$sub(2),
      " es utilizado como indicador de un grupo más amplio de óxidos de nitrógeno.",
      tags$p(),
      "El NO",
      tags$sub(2),
      " principalmente llega el ambiente proveniente de la quema de combustible. 
      El NO",
      tags$sub(2),
      " se forma a partir de las emisiones de los autos, camiones, autobuses, 
      plantas de energía eléctrica y equipos todo terreno.",
      tags$p(),
      "Las personas con asma, así como los niños y los ancianos, generalmente tienen mayor riesgo de sufrir los efectos del NO",
      tags$sub(2)," a la salud.",
      tags$br(),
      tags$a(href="https://www.epa.gov/no2-pollution/basic-information-about-no2#What is NO2",                        
             tags$em(paste0("Para mayor información acerca del Dióxido de Nitrógeno, consulte el sitio de internet de la EPA.")),
             target="_blank")
      )
    )
    } else if (input$color == "NO")
    {print(paste0(
      tags$p(),
      "El Óxido de Nitrógeno (NO) es uno de un grupo de gases altamente reactivos conocidos como óxidos de nitrógeno (NO",
      tags$sub("x"),
      "). Otros óxidos de nitrógeno incluyen ácido nitroso y ácido nítrico. El NO",
      tags$sub(2),
      "  es utilizado por la CalEPA como indicador de un grupo más amplio de óxidos de nitrógeno.",
      tags$p(),
      " El NO es un gas incoloro y el componente principal de los sistemas de escape a diesel.",
      tags$p(),
      "Las personas con asma, así como los niños y los ancianos tienen mayor riesgo de sufrir los efectos del NO a la salud.",
      tags$br(),
      tags$a(href="https://www.epa.gov/no2-pollution/basic-information-about-no2#What is NO2",                        
             tags$em(paste0("Para mayor información acerca del Óxido de Nitrógeno, consulte el sitio de internet de la EPA. ")),
             target="_blank")
      )
    )
    } else if (input$color == "CO")
    {print(paste0(
      tags$p(),
      "El Monóxido de Carbono (CO) es un gas incoloro e inodoro que puede ser nocivo cuando se inhala en grandes cantidades. El CO se libera cuando algo se quema. Las mayores fuentes de CO al aire exterior son los autos, camiones y otros vehículos o maquinaria que queman combustibles fósiles.",
      tags$p(),
      "Niveles muy altos de CO no es probable que ocurran al aire libre. Sin embargo, cuando los niveles de CO se elevan en exteriores, pueden ser de especial interés para las personas con algunos tipos de enfermedad cardiaca. Estas personas tienen ya una habilidad reducida para recibir sangre oxigenada en su corazón en situaciones donde el corazón necesita más oxígeno de lo habitual. ",
      tags$p(),
      tags$br(),
      tags$a(href="https://www.epa.gov/co-pollution/basic-information-about-carbon-monoxide-co-outdoor-air-pollution#What is CO",                        
             tags$em(paste0("Para mayor información acerca del Monóxido de Carbono, consulte el sitio de internet de la EPA.")),
             target="_blank")
    )
    )
    }
  }
  
})

output$tsNotationtitle<-renderText({
  
  ifelse(input$language=="en", "Interpreting this data with respect to government health based standards:", "La información se interpreta en relación con los estándares gubernamentales de salud.")
  
  
})
 output$tsNotation <- renderText({
   CAAQS24hr<-c("pm25"=35,
                    "O3"=90,
                    "NO2"=100,
                    "NO" = NA,
                    "CO" = 9)
   
   varvalue<-input$tsvars
   scaleby<-ifelse(varvalue=="CO",0.5,10)
   plotdata=datats()[date_day%in%as.character(input$date1)]
   plotdata[,hour:=hour(plotdata$datetime)]
   plotdata[,site:=factor(site, levels=unique(site_locations$site))]
   
   if(input$language=="en")
   labels<-c("pm25"="Particulate mass (PM2.5)",
             "O3"="Ozone",
             "NO2"="Nitrogen Dioxide",
             "NO" = "Nitrogen Oxide",
             "CO" = "Carbon Monoxide")
   if(input$language=="sp"){
     labels<-c("pm25"="Materia Particulada",
               "O3"="Ozono",
               "NO2"="Dióxido de Nitrógeno",
               "NO" = "Óxido de Nitrógeno",
               "CO" = "Monóxido de Carbono")}
   if (sum(is.na(plotdata[,varvalue, with=F]))==nrow(plotdata)) {
     
     return(ifelse(input$language=="en",print("There is no data."), print("No existen datos.")))
     
   }
  if(input$language=="en"){   
   if(varvalue%in%c("O3")) {
   
   print(
     
     if(sum(plotdata[,varvalue, with=F]>=CAAQS24hr[varvalue], na.rm=T)>0) {
          paste("For these sites, on this day, there are hours when",
                labels[varvalue],
                "measured by this sensor network", 
                "exceeds the 1-hour CAAQS standard of 90 ppb set by the calEPA.")
   } else if(sum(plotdata[,varvalue, with=F]>=CAAQS24hr[varvalue], na.rm=T)==0) {
          paste("For these sites, on this day, there are no hours of",
                labels[varvalue], 
                "measured by this sensor network that exceed the 1-hour CAAQS standard of 90 ppb set by the calEPA.")
     })}   
   
   else if (varvalue%in%c("NO")) {print("There is no health based recommendation for this pollutant.")
   } else if(varvalue%in%c("NO2")) {
     
     print(
       
       if(sum(plotdata[,varvalue, with=F]>=CAAQS24hr[varvalue], na.rm=T)>0) {
         paste("For these sites, on this day, there are hours when",
               labels[varvalue],
               "measured by this sensor network", 
               "exceeds the 1-hour NAAQS standard of 100 ppb set by the EPA.")
       } else if(sum(plotdata[,varvalue, with=F]>=CAAQS24hr[varvalue], na.rm=T)==0) {
         paste("For these sites, on this day, there are no hours of",
               labels[varvalue], 
               "measured by this sensor network that exceed the 1-hour NAAQS standard of 100 ppb set by the EPA.")
       } else {paste("There is no data.")}
   )} else if(varvalue%in%c("CO")) {
     
     print(
       if(sum(plotdata[,varvalue, with=F]>=CAAQS24hr[varvalue], na.rm=T)>0)
     {
       paste("For these sites, on this day, there are hours when",
             labels[varvalue],
             "measured by this sensor network", 
             "exceeds the 8-hour NAAQS standard of 9 ppm set by the EPA.")
     }   else if(sum(plotdata[,varvalue, with=F]>=CAAQS24hr[varvalue], na.rm=T)==0) {
       paste("For these sites, on this day, there are no hours of",
             labels[varvalue], 
             "measured by this sensor network that exceed the 8-hour NAAQS standard of 9 ppm set by the EPA.")
       } else {paste("There is no data.")} )
   } else if(varvalue%in%c("pm25")){
     print(
       if(sum(plotdata[,varvalue, with=F]>=CAAQS24hr[varvalue], na.rm=T)>0)
       {
         paste("For these sites, on this day, there are hours when",
               labels[varvalue],
               "measured by this sensor network", 
               "exceeds the 24-hour NAAQS standard of 35 ug/m3 set by the EPA.")
       }   else if(sum(plotdata[,varvalue, with=F]>=CAAQS24hr[varvalue], na.rm=T)==0) {
         paste("For these sites, on this day, there are no hours of",
               labels[varvalue], 
               "measured by this sensor network that exceed the 24-hour NAAQS standard of 35 ug/m3 set by the EPA.")
         }   else {paste("There is no data.")}
     )
   }
   
 } else  if(input$language=="sp"){   
   if(varvalue%in%c("O3")) {
     
     print(
       
       if(sum(plotdata[,varvalue, with=F]>=CAAQS24hr[varvalue], na.rm=T)>0) {
         paste("Para estos sitios, en este día, existen horas cuando él",
               labels[varvalue],
               "medido por esta red de sensores exceda en 1 hora el estándar CAAQS de 90 ppb establecido por la calEPA.")
       } else if(sum(plotdata[,varvalue, with=F]>=CAAQS24hr[varvalue], na.rm=T)==0) {
         paste("Para estos sitios, en este día, NO existen horas del",
               labels[varvalue], 
               "medido por esta red de sensores exceda en 1 hora el estándar CAAQS de 90 ppb establecido por la calEPA.")
       })}   
   
   else if (varvalue%in%c("NO")) {print("No existe recomendación con base en la salud para este contaminante.")
   } else if(varvalue%in%c("NO2")) {
     
     print(
       
       if(sum(plotdata[,varvalue, with=F]>=CAAQS24hr[varvalue], na.rm=T)>0) {
         paste("Para estos sitios, en este día, existen horas cuando él",
               labels[varvalue],
               "medido por esta red de sensores", 
               "exceda en 1 hora el estándar NAAQS de 100 ppb establecido por la EPA.")
       } else if(sum(plotdata[,varvalue, with=F]>=CAAQS24hr[varvalue], na.rm=T)==0) {
         paste("Para estos sitios, en este día, NO existen horas cuando él",
               labels[varvalue],
               "medido por esta red de sensores", 
               "exceda en 1 hora el estándar NAAQS de 100 ppb establecido por la EPA.")
       } else {paste("There is no data.")}
     )} else if(varvalue%in%c("CO")) {
       
       print(
         if(sum(plotdata[,varvalue, with=F]>=CAAQS24hr[varvalue], na.rm=T)>0)
         {
           paste("Para estos sitios, en este día, existen horas cuando él",
                 labels[varvalue],
                 "medido por esta red de sensores", 
                 "exceda en 8-hora el estándar NAAQS de 9 ppm establecido por la EPA.")
         }   else if(sum(plotdata[,varvalue, with=F]>=CAAQS24hr[varvalue], na.rm=T)==0) {
           paste("Para estos sitios, en este día, NO existen horas cuando él",
                 labels[varvalue],
                 "medido por esta red de sensores", 
                 "exceda en 8-hora el estándar NAAQS de 9 ppm establecido por la EPA.")} )
     } else if(varvalue%in%c("pm25")){
       print(
         if(sum(plotdata[,varvalue, with=F]>=CAAQS24hr[varvalue], na.rm=T)>0)
         {
           paste("Para estos sitios, en este día, existen horas cuando él",
                 labels[varvalue],
                 "medido por esta red de sensores", 
                 "exceda en 24-hora el estándar NAAQS de 35 ug/m3 establecido por la EPA.")
         }   else if(sum(plotdata[,varvalue, with=F]>=CAAQS24hr[varvalue], na.rm=T)==0) {
           paste("Para estos sitios, en este día, NO existen horas cuando él",
                 labels[varvalue],
                 "medido por esta red de sensores", 
                 "exceda en 24-hora el estándar NAAQS de 35 ug/m3 establecido por la EPA.")}
       )
     }
   
 }
   })
   
   
   
 
 output$tsPoll <- renderPlot({
  # If no zipcodes are in view, don't plot
   #fix selecting sites!!!!
   varvalue<-input$tsvars
   scaleby<-ifelse(input$tsvars=="CO",0.5,10)
   
   updateDateInput(session, "date", value=input$date1)
   
   
   plotdata=datats()[date_day%in%as.character(input$date1)]
   plotdata[,hour:=hour(plotdata$datetime)]
   plotdata[,site:=factor(site, levels=unique(site_locations$site))]
   
   if(nrow(plotdata)>0){
   if(min(plotdata$date_day,na.rm=T)>="2017-03-01") 
      plotdata<-plotdata[!site_short=="martinez",]
   }
 
  #minval<-min(data_wide[,input$tsvars,with=F],na.rm=T)
  
    
  maxval<-as.numeric(as.character(input$ylimpm))
  
  if(input$language=="en"){
  ylab.values<-c("pm25"="Particulate Mass PM2.5 (ug/m3)",
                   "O3"="Ozone (ppb)",
                   "NO2"="Nitrogen Dioxide (ppb)",
                   "NO" = "Nitrogen Oxide (ppb)",
                   "CO" = "Carbon Monoxide (ppm)")
  }else if(input$language=="sp")
    ylab.values<-c("pm25"="Materia Particulada  - PM2.5 (ug/m3)",
                   "O3"="Ozono (ppb)",
                   "NO2"="Dióxido de Nitrógeno (ppb)",
                   "NO" = "Óxido de Nitrógeno  (ppb)",
                   "CO" = "Monóxido de Carbono (ppm)")
 
  if(varvalue%in%c("CO") & nrow(plotdata)>0){
  print(ggplot(data=plotdata,
               aes_string("hour",varvalue, color="site")) +
          scale_x_continuous(limits=c(0,24),
                             breaks=0:11*2,
                             labels=c("12 am",paste(1:5*2,"am"),
                                      "12 pm",
                                      paste(7:11*2-12,"pm"))) +
          theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))+
         geom_line(size=1.2)+
          theme_pander(18)+
         ylab("Carbon Monoxide (ppm)")+
         guides(label="",colour = guide_legend(override.aes = list(size=3)))+
         guides(fill=guide_legend(nrow=2,byrow=TRUE))+
        scale_y_continuous(breaks = seq(0, maxval, scaleby), limits=c(0, maxval))+
          scale_color_manual(name="", values = c(myColors))
        )}
        
  if(nrow(plotdata)<1)
    {
    print(ggplot() + 
      annotate("text", 
               x = 4, y = 25, size=8, color="darkgrey",
        label = "There is no data available for this day 
        and pollutant combination. \nPlease select again.") + 
      theme_bw() +
      theme(panel.grid.major=element_blank(),
            panel.grid.minor=element_blank())+
        theme(line = element_blank(),
              text = element_blank(),
              title = element_blank())
        
    )
  }
  
  if(!varvalue%in%c("CO")&nrow(plotdata)>0)
    {
    print(ggplot(data=plotdata,
                 aes_string("hour",varvalue, color="site")) +
          geom_line(size=1.2)+theme_pander(18)+xlab("")+
           scale_x_continuous(limits=c(0,24),
                              breaks=0:11*2,
                              labels=c("12 am",paste(1:5*2,"am"),
                                       "12 pm",
                                       paste(7:11*2-12,"pm"))) +
           theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))+
          ylab(ylab.values[varvalue])+
          guides(label="",colour = guide_legend(override.aes = list(size=3)))+
          guides(fill=guide_legend(nrow=2,byrow=TRUE))+
          scale_y_continuous(breaks = seq(0, maxval, scaleby), limits=c(0, maxval)) +
           #add to include donovan APCD data
          # geom_line(aes(eval(as.name("hour")),
          #               eval(as.name(paste0(varvalue, "_donovan"))),
          #                color="Donovan Regulatory"), size=1.2)+
          scale_color_manual(name="", values = c(myColors))
          #                                         "Donovan Regulatory"="black"))
         
   )}
  
 })

 
 # This observer is responsible for maintaining the circles and legend,
 # according to the variables the user has chosen to map to color and size.
 observe({
  colorBy <- input$color
  #sizeBy <- input$size
  
  updateDateInput(session, "date1", value=input$date)
  
  #if (colorBy == "superzip") {
  #  # Color and palette are treated specially in the "superzip" case, because
  #  # the values are categorical instead of continuous.
  #  #colorData <- ifelse(zipdata$centile >= (100 - input$threshold), "yes", "no")
  #  #pal <- colorFactor("Spectral", colorData)
  #  pal <- colorFactor("Spectral", data_summary$pm25)
  # } else {
  #  #colorData <- zipdata[[colorBy]]
  
  if(as.POSIXct(input$date) >= Sys.time())
     return()
  
  data_wide <- data()
  data_summ <- data_summR()
  
  limitsbypoll<-c("pm25"=80,
                  "O3"=100,
                  "NO"=120,
                  "NO2"=120,
                  "CO" =2)
  binsbypoll<-c("pm25"=10,
                "O3"=10,
                "NO"=10,
                "NO2"=10,
                "CO" =6)
  
  if(min(data_summ$date_day,na.rm=T)>="2017-03-01") 
    data_summ<-data_summ[!site_short=="martinez",]
  
  
  if(nrow(data_summ)>1){
   
   colorData <- data_summ[[colorBy]]
   }
  
  if(nrow(data_summary1)>1){
   
   colorData2 <- data_summary1[[colorBy]]
  # http://davidjohnstone.net/pages/lch-lab-colour-gradient-picker
   palette_rev2 <- c("#0066b2","#00addd","#00e9a2","#f2cd00","#c69522","#9c5600","#62452c")
   #rev(c('#d73027','#fc8d59','#fee090','#e0f3f8','#91bfdb','#4575b4'))                   
   pal2 <- colorBin(palette_rev2, bins= binsbypoll[colorBy], pretty=T, na.color ="lightgrey",
                    c(0,max(limitsbypoll[colorBy],
                            quantile(data_wide[,colorBy, 
                                                   by=date_day,with=F], 
                                            na.rm=T, .996))))}
  
  #}
  
  #if (sizeBy == "superzip") {
  # # Radius is treated specially in the "superzip" case.
  # radius <- ifelse(zipdata$centile >= (100 - input$threshold), 30000, 3000)
  #} else {
  # radius <- zipdata[[sizeBy]] / max(zipdata[[sizeBy]]) * 30000
  #}
  #radius<-data_summary[[sizeBy]]/ max(data_summary[[sizeBy]],na.rm=T) * 500
  
  legend.values<-c("pm25"=paste0("Particulate Matter - PM",
                                 tags$sub(2.5),
                                 " (<span>&#181;</span>","g/m",tags$sup(3),
                                 ")"),
                   "O3"="Ozone (ppb)",
                   "NO2"="Nitrogen Dioxide (ppb)",
                   "NO" = "Nitrogen Oxide (ppb)",
                   "CO" = "Carbon Monoxide (ppm)")
  
  if(input$language=="sp"){
    legend.values<-c("pm25"=paste0("Materia Particulada  - PM",
                                   tags$sub(2.5),
                                   " (<span>&#181;</span>","g/m",tags$sup(3),
                                   ")"),
                     "O3"="Ozono (ppb)",
                     "NO2"="Dióxido de Nitrógeno (ppb)",
                     "NO" = "Óxido de Nitrógeno  (ppb)",
                     "CO" = "Monóxido de Carbono (ppm)")
  }
  
  label.units<-c("pm25"="ug/m3",
                 "O3"="ppb",
                 "NO2"="ppb",
                 "NO" = "ppb",
                 "CO" = "ppm")
  
  label.vals<-colorData
  label.vals<-as.character(paste(signif(label.vals,3), 
                                 label.units[colorBy]))
  
  label.vals[!is.finite(colorData)]="No Data"
  if(input$language=="sp") 
    label.vals[!is.finite(colorData)]="Sin Datos"
  
  
  if(!is.null(colorData)){
  leafletProxy("map", data = data_summ) %>% #data = zipdata) %>%
   clearShapes() %>%  clearPopups() %>%
      clearMarkers() %>%
   addCircleMarkers(~longitude, ~latitude, radius= 12, layerId = ~site_short, 
              stroke=T, color="black",weight=2, opacity=.8,
              fillOpacity=1, 
              fillColor=pal2(colorData),
              options = markerOptions(draggable = FALSE, riseOnHover=T)) %>%
   addLegend(className = "panel panel-default legend",
             "topleft", pal=pal2, values=colorData, title=HTML(legend.values[colorBy]),
             layerId="colorLegend",na.label = ifelse(input$language=="en", "No Data","Sin Datos"), opacity=0.8) 
   }  else {
    leafletProxy("map", data = data_summ) %>% #data = zipdata) %>%
     clearShapes() %>% clearPopups() %>%
       clearMarkers() %>%
       addCircleMarkers(~longitude, ~latitude, radius= 12, 
                        layerId = ~site_short, 
                      stroke=T, color="black",weight=2, opacity=.8,
                      fillOpacity=1, fillColor="lightgrey") %>%
     addLegend("topleft", values=NA, title=as.character(legend.values[colorBy]),
               layerId="colorLegend", opacity=0.8)
     
     }
    
 })
 
 # Show a popup at the given location
 showSitecodePopup <- function(id) {
   
   legend.values<-c("pm25"=paste0("PM",tags$sub(2.5)),
                    "O3"="Ozone",
                    "NO2"="Nitrogen Dioxide",
                    "NO" = "Nitrogen Oxide",
                    "CO" = "Carbon Monoxide")
   units.legend<-c("pm25"=paste0("<span>&#181;</span>","g/m",tags$sup(3)),
            "O3"="ppb",
            "NO2"="ppb",
            "NO" ="ppb",
            "CO" = "ppm")
  
  selectedSite <- data_summR()[site_short%in%id,]
  
  
  content <- as.character(tagList(
   #tags$h4("Mean:", round(selectedSite$pm25,2)),
   tags$strong(HTML(sprintf("%s",
                            site_locations[site_short==selectedSite$site_short,]$site))), 
   tags$br(),
   print(
     if(!is.na(round(selectedSite[,input$color,with=F])))
       {
     HTML(paste("Daily Mean of", HTML(legend.values[input$color]),":",
           round(selectedSite[,input$color,with=F],1), 
           HTML(units.legend[input$color])))
      } else {
     "No data"})
  ))
  leafletProxy("map") %>% addPopups(selectedSite$longitude, selectedSite$latitude, 
                                    content, layerId = id)
 }
 
 # When map is clicked, show a popup with city info
 observe({
  leafletProxy("map") %>% clearPopups()
  event <- input$map_marker_click
  if (is.null(event))
   return()

  isolate({
   showSitecodePopup(event$id)
  })
 })
#})

 ## Data Explorer ###########################################
 
 observe({
  pollnames <- if (is.null(input$sites)) character(0) else {
   c("pm25","CO","NO","NO2","O3")}
   pollnames
  
  stillSelected <- isolate(input$pollutants[input$pollutants %in% pollnames])
  updateSelectInput(session, "pollutants", choices = pollnames,
                    selected = stillSelected)
 })

 observe({
  if (is.null(input$goto))
   return()
  isolate({
   map <- leafletProxy("map")
   map %>% clearPopups()
   dist <- 0.5
   id <- input$goto$site_short
   lat <- input$goto$lat
   lng <- input$goto$lng
   showZipcodePopup(site_short, lat, lng)
   map %>% fitBounds(lng - dist, lat - dist, lng + dist, lat + dist)
  })
 })

 output$sitetable <- shiny::renderDataTable({
  df<-data_wide[site%in%input$sites & datetime>=as.POSIXct(input$startdate) , 
                c("datetime","site","longitude", "latitude", input$pollutants),
                with=F]

  
  df[,datetime:=as.POSIXct(datetime, origin="1970-01-01")]
  
  setnames(df, c("datetime","site", "longitude","latitude"), 
           c("Date","Site Name","Longitude","Latitude"))
  if(!is.null(input$pollutants)){
  df[,(input$pollutants):= lapply(.SD, signif,2), .SDcols=input$pollutants]}
  if(!is.null(input$sites)){
    df[,(c("Longitude","Latitude")):= lapply(.SD, round,4), .SDcols=c("Longitude","Latitude")]
               }
  
  
 
  
  # df <- df  %>%
  #  mutate(Action = paste('<a class="go-map" href="" data-lat="', latitude, '" data-long="', longitude, '" data-id="',
  #                        site, '"><i class="fa fa-crosshairs"></i></a>', sep=""))
  # action <- DT::dataTableAjax(session, df)

 # DT::datatable(df, options = list(ajax = list(url = action)), escape = FALSE)
  shiny::datatable(df)
 })
})

