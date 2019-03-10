# Global 
library(leaflet)
library(shiny)
library(tidyverse)
library(shinythemes)
library(RColorBrewer)
library(dbplyr)
library(RSQLite)
library(stringr)
library(lubridate)
library(dplyr)
library(sf)
library(sp)
library(raster)
library(rgeos)
library(shinydashboard)

r_colors <- rgb(t(col2rgb(colors()) / 255))
names(r_colors) <- colors()

# Process 
whales<- read_csv("sp_obis_westcoast.csv")

whalesdf <- as.data.frame(whales)

whalesdf$vernacularName <- as.factor(whalesdf$vernacularName)
class(whalesdf$vernacularName)

whalesdf2 <- whalesdf %>% 
  filter(vernacularName == "Blue Whale"| vernacularName =="Gray Whale"| vernacularName =="Humpback Whale") %>% 
  filter(DecimalLatitude > 32 | DecimalLatitude < 39) %>% 
  filter(DecimalLongitude < -116 | DecimalLongitude > -124) %>%
  rename(lat = DecimalLatitude) %>% 
  rename(lon = DecimalLongitude)

# Isolate month and year from observation time stamp:

whalesdf2$date_simple <- as.Date(whalesdf2$EventDate, format="%Y/%M/%D")
whalesdf2$year <- format(as.Date(whalesdf2$EventDate, format="%Y/%M/%d"),"%Y")
whalesdf2$month <- format(as.Date(whalesdf2$date_simple, format="%Y/%m/%d"),"%m")

new <- whalesdf2 %>% 
  filter(year > 1970)



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
                  min = 0,
                  max=12,
                  value = c(0,12)),
      
      checkboxGroupInput(inputId = "species", 
                         label = "Species",
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



server <- function(input, output, session) {

  output$mymap <- renderLeaflet({
    leaflet(map) %>%
      addTiles() %>%
      addProviderTiles(providers$Esri.WorldImagery) %>% 
#      addAwesomeMarkers(lat=map$lat,lng=map$lon) %>% 
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

shinyApp(ui, server)