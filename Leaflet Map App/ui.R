
ui <- dashboardPage(
  
  
  dashboardHeader(title = "Endangered Cetaceans Sightings", titleWidth = 450),
  
  
  dashboardSidebar(
    sidebarMenu(
      
      
      selectInput(inputId = "year", label="Year:",
                  selected = "2018",
                  choices = sort(unique(new$EventDate))),
      #sliderInput(inputId = "month",
      #     label="Month:",
      #    min = 0, max=12,
      #   value = c(0,12)),
      
      selectInput(inputId = "species",
                  label="Species:",
                  selected = "Blue Whale",
                  choices = sort(unique(new$vernacularName)))
      
      
      
    )),
  
  
  body<-dashboardBody(
    
      
    )
  )
