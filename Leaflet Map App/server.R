server <- function(input,output, session){


data <- reactive({
  x <- df
  
})

output$mymap <- renderLeaflet({
  df <- data()
  
  m <- leaflet(data = df) %>%
    addTiles() %>%
    addMarkers(lng = ~lon,
               lat = ~lat,
               popup = paste("Species", df$vernacularName,"<br>",
                             "Year",df$EventDate))
  m
})
}