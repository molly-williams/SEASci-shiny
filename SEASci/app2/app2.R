# Global
library(shiny)
library(leaflet)
library(tidyverse)
library(shinythemes)
library(RColorBrewer)
library(dbplyr)
library(stringr)
library(lubridate)
library(dplyr)
library(sf)
library(sp)
library(raster)
library(rgeos)
library(shinydashboard)
library(DT)
library(crosstalk)

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


# Define UI 
ui <- dashboardPage(
    
    
    dashboardHeader(title = "Endangered Cetacean Sightings 2013-2018",titleWidth = 450),
                    
    
    dashboardSidebar(
        sidebarMenu(
            

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
                               selected = 1)
            

            
            
        )),
    
    
    body<-dashboardBody(
        
        tabItems(
            
            
            tabItem(tabName="cinms",
                    fluidRow(
                        box(
                            title = "Whale Conservation Map",
                            collapsible = TRUE,
                            background = "blue",
                            width = "100%",
                            height = "1000px",
                            leafletOutput("cinms")
                        )))
            
        )
    )
)










server <- function(input, output, session) {
    
    
    whale_icon <- makeIcon(
        iconUrl = "https://cdn2.iconfinder.com/data/icons/funtime-animals-humans/60/004_004_whale_sea_ocean_animal_fountain-512.png",
        iconWidth = 30, iconHeight = 30,
        iconAnchorX = 10, iconAnchorY = 10)
    
    output$mymap <- renderLeaflet({  
        
        
        map <- new %>% 
            filter(year==input$year) %>%
            filter(month==input$month) %>% 
            filter(species==input$species)
        
            
        })
        
        
        leaflet(map) %>%
            addTiles() %>%
            addProviderTiles(providers$Esri.NatGeoWorldMap) %>% 
            addAwesomeMarkers(lat=new$lat,lng=new$lon)
    }
    
    
    
    
    


shinyApp(ui, server)