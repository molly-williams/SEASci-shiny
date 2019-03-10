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
whalesdf2$year <- as.numeric(whalesdf2$year)
whalesdf2$month <- as.numeric(whalesdf2$month)

new <- whalesdf2 %>% 
  filter(year > 1970)

whale_icon <- makeIcon(
  iconUrl = "https://cdn2.iconfinder.com/data/icons/funtime-animals-humans/60/004_004_whale_sea_ocean_animal_fountain-512.png",
  iconWidth = 30, iconHeight = 30,
  iconAnchorX = 10, iconAnchorY = 10
  
)




ui <- fluidPage(theme = shinytheme("cerulean"),
                
                h2("Endangered Cetacean Sightings 2013-2018"),
                
                sidebarLayout(
                  sidebarPanel(
                    
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
                                       selected = 1)),
                  
                  mainPanel(leafletOutput("mymap"))
                )
  )

  
  
server <- function(input, output) {
  
  
  output$mymap <- renderLeaflet({  
    
    
    map <- new %>% 
      filter(year == input$year) %>% 
      filter(month == input$month) %>% 
      filter(vernacularName == input$species)
    
    
    
  leaflet(map) %>%
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
                             "</b>"),
               icon = whale_icon)
  
  })
  
  
}

shinyApp(ui, server)
