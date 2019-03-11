
library(shiny)
library(tidyverse)
library(sf)
library(shinythemes)
library(tmap)
library(leaflet)
library(RColorBrewer)
library(dbplyr)
library(stringr)
library(lubridate)
library(dplyr)
library(sp)
library(raster)
library(rgeos)
library(shinydashboard)
library(DT)
library(crosstalk)

# Process

### Import and wrangle sighting data    
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
    filter(year > 1970) %>% 
    filter(individualCount < 10)


# Create shape file based on lat/longs:
coordinates(new) <- ~lon+lat
proj4string(new)<- CRS("++proj=longlat +datum=WGS84") # set coordinate system to WGS

LL_coords <- spTransform(new,CRS("+proj=longlat"))
raster::shapefile(LL_coords, "WhaleShapefile2.shp", overwrite=TRUE)


whale_shp <- read_sf("WhaleShapefile2.shp")
whale_shp # check extents in output
st_crs(whale_shp) # check projection; its WGS84


### Make whale icon for observation points
whale_icon <- makeIcon(
    iconUrl = "https://cdn2.iconfinder.com/data/icons/funtime-animals-humans/60/004_004_whale_sea_ocean_animal_fountain-512.png",
    iconWidth = 30, iconHeight = 30,
    iconAnchorX = 10, iconAnchorY = 10
    
)


# Define UI for application that draws a histogram
ui <- fluidPage(
    theme = shinytheme("cerulean"),
    # Application title
    titlePanel("Endangered Cetacean Map"),
    
    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            
            helpText("Visualize observations of three endangered whale species recorded on the California Coast from 2013-2018."),
            
            sliderInput(inputId = "month",
                        label = "Month:",
                        min = 1,
                        max = 12,
                        value = c(1,12)),
            
            selectInput(inputId = "year",
                        label = "Year:",
                        selected = "2018",
                        choices = c(2013,2014,2015,2016,2017,2018)),
            
            checkboxGroupInput(inputId = "species", 
                               label = "Species",
                               choices = list("Blue Whale" = 1, "Gray Whale" = 2, "Humpback Whale" = 3),
                               selected = 1)
            
            
        ), # close parenthesis for sidebarPanel
        
        # Show a plot of the generated distribution
        mainPanel(
            leafletOutput("map")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    
    # Creating the reactive output ('map')
    output$map <- renderLeaflet({
        
        whale_obs <- whale_shp %>% 
            filter(month == input$month) %>% 
            filter(year == input$year) 
#            filter(species == input$species)
        
        whale_map <- 
            tm_basemap("Esri.WorldImagery") +
            tm_shape(whale_obs) +
            tm_dots(size = "indvdlC", col = "indvdlC")
        
        tmap_leaflet(whale_map)
        
        
    })
}

# Run the application 
shinyApp(ui = ui, server = server)

