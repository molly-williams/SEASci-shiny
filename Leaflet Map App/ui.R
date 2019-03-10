ui <- dashboardPage(
  
  
  dashboardHeader(title = "Endangered Cetacean Sightings", titleWidth = 450),
  
  
  dashboardSidebar(
    sidebarMenu(
      
      menuItem("Whale Map", tabName = "mymap", icon=icon("map"),startExpanded = FALSE),
      
      selectInput(inputId = "year",
                  label="Year:",
                  selected = "2018",
                  choices = sort(unique(new$year))),
      
      
      sliderInput(inputId = "month",
                  label="Month:",
                  min = 1,
                  max=12,
                  value = c(1,12)),
      
      checkboxGroupInput(inputId = "species", 
                         label = "Species",
                         choices = list("Blue Whale" = 1, "Gray Whale" = 2, "Humpback Whale" = 3),
                         selected = 1),
      
      
      hr(),
      fluidRow(column(3, verbatimTextOutput("value")))
      
      
      
    )),
  
  
  body<-dashboardBody(
    
    fluidRow(
      column(width = 12,
             box(width = NULL, solidHeader = TRUE,
                 leafletOutput("mymap", height = 500)
             ))))
)



  
