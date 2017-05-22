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
  
  data <- reactive ({
    
      if(input$radio == "All"){
        data_wide
      } else {
        data_wide[site%in%input$tssites]
      }
    })
  
  data_summary <- reactive ({
    data_summary<-data_wide[date_day%in%as.character(input$date),
                            lapply(.SD, FUN = function (x)
                              mean(as.numeric(as.character(x)), na.rm=T)), .SDcols=c("longitude","latitude",
                                      c("pm25","CO","NO","NO2","O3")),
                            by="site_short"]
    missing_sites<-
      site_locations$site_short[!site_locations$site_short%in%data_summary$site_short]
    missing_sites<-site_locations[site_short%in%missing_sites]
    missing_sites<-missing_sites[,c("site_short","latitude","longitude"),with=F]
    data_summary<-rbindlist(list(data_summary,missing_sites),fill=T)
    data_summary
    
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
  
output$pollDescrip <- reactive({
  varvalue<-input$tsvars
  if(varvalue%in%"pm25"){
    print("This is a test pm25")
  } else if(varvalue%in%"O3"){
      print("This is a test O3")
  } else if(varvalue%in%"NO2"){
    print("This is a test NO2")
  } else "Nothing"
  
})
})

output$poldesc<- renderText({
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
  
})
 output$tsNotation <- renderText({
   CAAQS24hr<-c("pm25"=35,
                    "O3"=90,
                    "NO2"=100,
                    "NO" = NA,
                    "CO" = 9)
   
   varvalue<-input$tsvars
   scaleby<-ifelse(varvalue=="CO",0.5,10)
   plotdata=data()[date_day%in%as.character(input$date1)]
   plotdata[,hour:=hour(plotdata$datetime)]
   plotdata[,site:=factor(site, levels=unique(site_locations$site))]
   
   labels<-c("pm25"="particle mass (PM2.5)",
             "O3"="Ozone",
             "NO2"="Nitrogen Dioxide",
             "NO" = "Nitrogen Oxide",
             "CO" = "Carbon Monoxide")
   if (sum(is.na(plotdata[,varvalue, with=F]))==nrow(plotdata)) {
     
     return(print("There is no data."))
     
   }
     
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
   
 })
   
   
   
 
 output$tsPoll <- renderPlot({
  # If no zipcodes are in view, don't plot
   #fix selecting sites!!!!
   varvalue<-input$tsvars
   scaleby<-ifelse(input$tsvars=="CO",0.5,10)
   plotdata=data()[date_day%in%as.character(input$date1)]
   plotdata[,hour:=hour(plotdata$datetime)]
   plotdata[,site:=factor(site, levels=unique(site_locations$site))]
 
  #minval<-min(data_wide[,input$tsvars,with=F],na.rm=T)
  maxvalnumbers<-c("pm25"=max(quantile(data_wide[,input$tsvars,with=F],na.rm=T, .9995),50),
                   "O3"=max(quantile(data_wide[,input$tsvars,with=F],na.rm=T, .9995),50),
                   "NO2"=max(quantile(data_wide[,input$tsvars,with=F],na.rm=T, .9995),50),
                   "NO" = max(quantile(data_wide[,input$tsvars,with=F],na.rm=T, .9995),50),
                   "CO" = max(quantile(data_wide[,input$tsvars,with=F],na.rm=T, .9995),1))
    
    
  maxval<-quantile(data_wide[,input$tsvars,with=F],na.rm=T, .9995)
  
  myColors <- brewer.pal(nrow(site_locations),"Paired")
  names(myColors) <- levels(as.factor(site_locations$site))
  colScale <- scale_color_manual(name = "",values = myColors)
  
  ylab.values<-c("pm25"="Particle Mass PM2.5 (ug/m3)",
                   "O3"="Ozone (ppb)",
                   "NO2"="Nitrogen Dioxide (ppb)",
                   "NO" = "Nitrogen Oxide (ppb)",
                   "CO" = "Carbon Monoxide (ppm)")
  
 
  if(varvalue%in%c("CO") & nrow(plotdata)>0){
  print(ggplot(data=plotdata,
               aes_string("hour",varvalue, color="site")) +
          scale_x_continuous(limits=c(0,24),
                             breaks=0:11*2,
                             labels=c("12 am",paste(1:5*2,"am"),
                                      "12 pm",
                                      paste(7:11*2-12,"pm"))) +
          theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))+
         geom_line(size=1.2)+colScale+
          theme_pander(18)+
         ylab("Carbon Monoxide (ppm)")+
         guides(label="",colour = guide_legend(override.aes = list(size=3)))+
         guides(fill=guide_legend(nrow=2,byrow=TRUE))+
        scale_y_continuous(breaks = seq(0, maxval, scaleby), limits=c(0, maxval))
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
  
  if(!varvalue%in%c("CO")&nrow(plotdata)>0){
    
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
          scale_y_continuous(breaks = seq(0, maxval, scaleby), limits=c(0, maxval))+
           #add to include donovan APCD data
           geom_line(aes(eval(as.name("hour")), 
                         eval(as.name(paste0(varvalue, "_donovan"))),
                          color="Donovan Regulatory"), size=1.2)+
           scale_color_manual(name="", values = c(myColors, 
                                                  "Donovan Regulatory"="black"))
         
   )}
  
 })

 
 # This observer is responsible for maintaining the circles and legend,
 # according to the variables the user has chosen to map to color and size.
 observe({
  colorBy <- input$color
  #sizeBy <- input$size
  
  #if (colorBy == "superzip") {
  #  # Color and palette are treated specially in the "superzip" case, because
  #  # the values are categorical instead of continuous.
  #  #colorData <- ifelse(zipdata$centile >= (100 - input$threshold), "yes", "no")
  #  #pal <- colorFactor("Spectral", colorData)
  #  pal <- colorFactor("Spectral", data_summary$pm25)
  # } else {
  #  #colorData <- zipdata[[colorBy]]
  
  
  data_summ <- data_summary()
  
  
  if(nrow(data_summ)>1){
   
   colorData <- data_summ[[colorBy]]
   }
  
  if(nrow(data_summary1)>1){
   
   colorData2 <- data_summary1[[colorBy]]
   palette_rev2 <-rev(brewer.pal(n = 9, "RdYlGn")[1:7])
   pal2 <- colorBin(palette_rev2,c(0,
                                   quantile(data()[,colorBy, 
                                                   by=date_day,with=F], 
                                            na.rm=T, .996)), 6, pretty=T, na.color ="lightgrey")}
  
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
  
  label.units<-c("pm25"="ug/m3",
                 "O3"="ppb",
                 "NO2"="ppb",
                 "NO" = "ppb",
                 "CO" = "ppm")
  
  label.vals<-colorData
  label.vals<-as.character(paste(signif(label.vals,3), 
                                 label.units[colorBy]))
  
  label.vals[!is.finite(colorData)]="No Data"
  
  
  if(!is.null(colorData)){
  leafletProxy("map", data = data_summ) %>% #data = zipdata) %>%
   clearShapes() %>%
   addCircleMarkers(~longitude, ~latitude, radius= 11, layerId = ~site_short, 
              stroke=T, color="black",weight=2, opacity=.8,
              fillOpacity=1, 
              fillColor=pal2(colorData), 
              #label=~label.vals,
              #labelOptions = labelOptions(noHide = F, 
              #                            direction="auto")
   ) %>%
   addLegend(className = "panel panel-default legend",
             "topleft", pal=pal2, values=colorData, title=HTML(legend.values[colorBy]),
             layerId="colorLegend",na.label = "No Data", opacity=0.8) 
   }  else {
    leafletProxy("map", data = data_summ) %>% #data = zipdata) %>%
     clearShapes() %>%
       addCircleMarkers(~longitude, ~latitude, radius= 11, 
                        layerId = ~site_short, 
                      stroke=T, color="black",weight=2, opacity=.8,
                      fillOpacity=1, fillColor="grey") %>%
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
  
  selectedSite <- data_summary()[site_short%in%id,]
  
  
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

 output$sitetable <- DT::renderDataTable({
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
  DT::datatable(df)
 })
})

