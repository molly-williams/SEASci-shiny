ui <- dashboardPage(
  
  
  dashboardHeader(title = "Endangered Cetaceans Sightings", titleWidth = 450),
  
  
  dashboardSidebar(
    sidebarMenu(
      
      menuItem("Whale Map", tabName = "mymap", icon=icon("map"),startExpanded = FALSE),
      
      years<- selectInput(inputId = "year", 
                          label="Year:",
                          selected = "2018",
                          choices = sort(unique(new$EventDate))),
      
      
      month <- sliderInput(inputId = "month",
                           label="Month:",
                           min = 0,
                           max=12,
                           value = c(0,12)),
      
      species<-   checkboxGroupInput("species", label = "Species", 
                                     choices = list("Blue Whale" = 1, "Grey Whale" = 2, "Humpback Whale" = 3),
                                     selected = 1),
      
      
      hr(),
      fluidRow(column(3, verbatimTextOutput("value")))
        
      # Species dropdown:   
       #  selectInput(inputId = "species",
        #                    label="Species:",
         #                   selected = "Blue Whale",
          #                  choices = sort(unique(new$vernacularName)))
      
      
      
    )),
  
  
  body<-dashboardBody(
    
    fluidRow(
      column(width = 12,
             box(width = NULL, solidHeader = TRUE,
                 leafletOutput("mymap", height = 500)
             ))))
)



  
