server <- function(input, output, session) {
  
  
  data <- reactive({
    x <- df
    
    
    output$mymap <- renderLeaflet({
      df <- data()
      
      m <- leaflet(data = df) %>%
        addTiles() 
        
      
      m
    })
  })
  
  
}

