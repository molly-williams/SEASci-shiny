
server <- function(input, output, session) {
  
  
  
  output$mymap <- renderLeaflet({
    
    map <- new %>% 
      filter(vernacularName==input$species) %>% 
      filter(year==input$year) %>% 
      filter(month==input$month)
      
    
    
    leaflet(map) %>%
      addTiles() %>%
      addProviderTiles(providers$Esri.WorldImagery) %>% 
      addAwesomeMarkers(lat=new$lat,lng=new$lon) 
  })
  
}





