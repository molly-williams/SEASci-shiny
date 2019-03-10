
server <- function(input,output, session) {

  
  output$mymap <- renderLeaflet({
    
    map <- new %>% 
      filter(year==input$year) %>% 
      filter(month==input$month) %>% 
      filter(vernacularName==input$species)
      
  
    
    leaflet() %>%
      addTiles() %>%
      addProviderTiles(providers$Esri.WorldImagery) %>% 
      addMarkers(lng = new$lon, 
                 lat = new$lat,
                 popup = paste(sep="", 
                               "<font size = 2 color = blue>", 
                               "Scientific Name: ","</font>", 
                               "<font size = 2 color = black>", 
                               new$scientificName,"</font>", 
                               "<br/>", 
                               "<font size = 2 color = blue>", 
                               "Total Sighted: ","</font>", 
                               "<font size = 2 color = black>", 
                               new$individualCount,"</font>",
                               "<br/>", 
                               "<font size = 2 color = blue>", 
                               "Occurrence ID: ","</font>", 
                               "<font size = 2 color = black>", 
                               new$OccurenceID, "</font>",
                               "<br/>", 
                               "<font size = 2 color = blue>", 
                               "Latitude: ","</font>", 
                               "<font size = 2 color = black>", 
                               new$lat, "</font>",
                               "<br/>",
                               "<font size = 2 color = blue>", 
                               "Longitude: ","</font>", 
                               "<font size = 2 color = black>", 
                               new$lon, "</font>",
                               "</b>"))
      
      
  })
  
  
}



