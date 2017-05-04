library(shiny)
library(leaflet)
library(RColorBrewer)
library(lattice)
library(dplyr)
library(ggplot2)
library(ggthemes)


options(device='cairo')
# 
# # Leaflet bindings are a bit slow; for now we'll just sample to compensate
# set.seed(100)
# zipdata <- allzips[sample.int(nrow(allzips), 10000),]
# # By ordering by centile, we ensure that the (comparatively rare) SuperZIPs
# # will be drawn last and thus be easier to see
# zipdata <- zipdata[order(zipdata$centile),]

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
 #  
 #  subset(data_wide,
 #         latitude >= latRng[1] & latitude <= latRng[2] &
 #          longitude >= lngRng[1] & longitude <= lngRng[2] &
 #          date_day%in%as.character(input$date))
 # })
 # 

 output$tsPoll <- renderPlot({
  # If no zipcodes are in view, don't plot
   #fix selecting sites!!!!
   varvalue<-input$tsvars
   scaleby<-ifelse(input$tsvars=="CO",0.5,10)
   plotdata=data()[date_day%in%as.character(input$date1)]
   plotdata[,hour:=hour(plotdata$datetime)]
   plotdata[,site:=factor(site, levels=unique(site_locations$site))]
 
  #minval<-min(data_wide[,input$tsvars,with=F],na.rm=T)
  maxval<-quantile(data_wide[,input$tsvars,with=F],na.rm=T, .9995)
  
  myColors <- brewer.pal(nrow(site_locations),"Paired")
  names(myColors) <- levels(as.factor(site_locations$site))
  colScale <- scale_color_manual(name = "",values = myColors)
  
  if(varvalue%in%c("CO") & nrow(plotdata)>0){
  print(ggplot(data=plotdata,
               aes_string("hour",varvalue, color="site")) +
         geom_line(size=1.2)+colScale+
          theme_pander(18)+xlab("Time (h)")+
         ylab("")+
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
          geom_line(size=1.2)+theme_pander(18)+xlab("Time (h)")+
          ylab("")+
          guides(label="",colour = guide_legend(override.aes = list(size=3)))+
          guides(fill=guide_legend(nrow=2,byrow=TRUE))+
          scale_y_continuous(breaks = seq(0, maxval, scaleby), limits=c(0, maxval))+
           #add to include donovan APCD data
          # geom_line(aes(eval(as.name("hour")), 
          #               eval(as.name(paste0(varvalue, "_donovan"))),
          #                color="Donovan Regulatory"), size=1.2)+
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
   palette_rev2 <-brewer.pal(n = 9, "YlOrRd")[3:8]
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
  
  legend.values<-c("pm25"="Particle Mass PM2.5 (ug/m3)",
                   "O3"="Ozone (ppb)",
                   "NO2"="Nitrogen Dioxide (ppb)",
                   "NO" = "Nitrogen Oxide (ppb)",
                   "CO" = "Carbon Monoxide (ppm)")
  
  if(!is.null(colorData)){
  leafletProxy("map", data = data_summ) %>% #data = zipdata) %>%
   clearShapes() %>%
   addCircleMarkers(~longitude, ~latitude, radius= 11, layerId = ~site_short, 
              stroke=T, color="black",weight=2, opacity=.8,
              fillOpacity=1, fillColor=pal2(colorData)) %>%
   addLegend("topleft", pal=pal2, values=colorData, title=as.character(legend.values[colorBy]),
             layerId="colorLegend",na.label = "No Data", opacity=0.8)
   }  else {
    leafletProxy("map", data = data_summ) %>% #data = zipdata) %>%
     clearShapes() %>%
       addCircleMarkers(~longitude, ~latitude, radius= 11, layerId = ~site_short, 
                      stroke=T, , color="black",weight=2, opacity=.8,
                      fillOpacity=1, fillColor="grey") %>%
     addLegend("topleft", values=NA, title=as.character(legend.values[colorBy]),
               layerId="colorLegend", opacity=0.8)}
    
 })
 
 # Show a popup at the given location
 showSitecodePopup <- function(id) {
   
   legend.values<-c("pm25"="PM2.5",
                    "O3"="Ozone",
                    "NO2"="Nitrogen Dioxide",
                    "NO" = "Nitrogen Oxide",
                    "CO" = "Carbon Monoxide")
   units.legend<-c("pm25"="ug/m3",
            "O3"="ppb",
            "NO2"="ppb",
            "NO" ="ppb",
            "CO" = "ppm")
  
  selectedSite <- data_summary()[site_short%in%id,]
 
  content <- as.character(tagList(
   #tags$h4("Mean:", round(selectedSite$pm25,2)),
   tags$strong(HTML(sprintf("%s",
                            site_locations[site_short==selectedSite$site_short,]$site))), tags$br(),
   
   sprintf(
     if(!is.na(round(selectedSite[,input$color,with=F])))
       {
     paste("Daily Mean of", as.character(legend.values[input$color]),":",
           round(selectedSite[,input$color,with=F],1), 
           as.character(units.legend[input$color]))
      } else {
     "No data"}), 
   tags$br(),
   tags$a(href="https://www.epa.gov/criteria-air-pollutants#self",
          tags$em("Understanding more about Criteria Pollutants"))
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
  df<-data_wide[site%in%input$sites, 
                c("datetime","site","longitude", "latitude", input$pollutants),
                with=F]
  df[,datetime:=as.POSIXct(datetime, origin="1970-01-01")]
  
 
  
  df <- df  %>%
   mutate(Action = paste('<a class="go-map" href="" data-lat="', latitude, '" data-long="', longitude, '" data-id="',
                         site, '"><i class="fa fa-crosshairs"></i></a>', sep=""))
  action <- DT::dataTableAjax(session, df)

  DT::datatable(df, options = list(ajax = list(url = action)), escape = FALSE)
 })
})

