
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
library(shinyWidgets)


# Process

### Import and wrangle sighting data    
whales<- read_csv("sp_wa_sb.csv")
#trips <- read_csv("trips.csv")

whalesdf <- as.data.frame(whales)

whalesdf$vernacularName <- as.factor(whalesdf$vernacularName)
class(whalesdf$vernacularName)
whalesdf$vernacularName[whalesdf$vernacularName=="Humpback"]<-"Humpback Whale"

whalesdf2 <- whalesdf %>% 
    filter(str_detect(tolower(vernacularName), pattern = c("blue whale","fin whale","gray whale", "minke whale","humpback whale")))%>% 
    filter(DecimalLatitude > 32 | DecimalLatitude < 39) %>% 
    filter(DecimalLongitude < -116 | DecimalLongitude > -124) %>%
    rename(lat = DecimalLatitude) %>% 
    rename(lon = DecimalLongitude)# %>% 
   # rename(collectionID = trip_id)

# Isolate month and year from observation time stamp:

whalesdf2$date <- as.Date(whalesdf2$EventDate, format="%Y/%M/%D")
whalesdf2$year <- format(as.Date(whalesdf2$EventDate, format="%Y/%M/%d"),"%Y")
whalesdf2$month <- format(as.Date(whalesdf2$date, format="%Y/%m/%d"),"%m")
whalesdf2$year <- as.numeric(whalesdf2$year)
whalesdf2$month <- as.numeric(whalesdf2$month)

new <- whalesdf2 %>%
  dplyr::select(vernacularName, scientificName, OccurenceID, individualCount, app_used, lat, lon, occurenceStatus, date, month, year) %>%
  dplyr::filter(year > 1970) %>%
  dplyr::filter(individualCount < 10)

#new1<-left_join(trips,new,by=trip_id)

write.csv(new, "newdata.csv")

# Create shape file based on lat/longs:
coordinates(new) <- ~lon+lat
proj4string(new)<- CRS("++proj=longlat +datum=WGS84") # set coordinate system to WGS

LL_coords <- spTransform(new,CRS("+proj=longlat"))
raster::shapefile(LL_coords, "WhaleShapefile2.shp", overwrite=TRUE)


whale_shp <- read_sf("WhaleShapefile2.shp") %>% 
  rename("Whales Sighted" = indvdlC) %>% 
  rename("Species" = vrnclrN) %>% 
  rename("app" = app_usd)


whale_shp$Species <- as.factor(whale_shp$Species)
  
whale_shp # check extents in output
st_crs(whale_shp) # check projection; its WGS84

ship_shp <- read_sf("ship_lane_2013.shp")
st_crs(ship_shp)

sanctuary_shp <- read_sf("cinms1.shp")
st_crs(sanctuary_shp)



# Define UI for application that draws a histogram
ui <- fluidPage(
  setBackgroundColor(
    color = c("#F7FBFF", "#2171B5"),
    gradient = "linear"),
    theme = shinytheme("cerulean"),
    # Application title
    titlePanel("Cetacean Sightings 2013-2019"),
    
    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
          
          
          tabPanel("Live Images", 
                   conditionalPanel(condition = "input.species == 'Humpback Whale'",
                                    img(src = "humpback-whale-pic.jpg", height = 150, width = "100%")
                   ),
                   conditionalPanel(condition = "input.species == 'Blue Whale'",
                                    img(src = "blue.jpg", height = 150, width = "100%")
                   ),
                   conditionalPanel(condition = "input.species == 'Gray Whale'",
                                    img(src = "gray.jpg", height = 150, width = "100%")
                   ),
                   conditionalPanel(condition = "input.species == 'Fin Whale'",
                                    img(src = "fin.jpg", height = 150, width = 300)
                  ),
                  conditionalPanel(condition = "input.species == 'Minke Whale'",
                                   img(src = "minke.jpg", height = 150, width = 300)
                  )
          ),
            
            
            sliderInput(inputId = "month",
                        label = "Month:",
                        min = 1,
                        max = 12,
                        value = range(whale_shp$month),
                        step = 1),
            
            selectInput(inputId = "year",
                        label = "Year:",
                        selected = "2018",
                        choices = c(2013,2014,2015,2016,2017,2018,2019)),
          
          selectInput(inputId = "app",
                      label = "App Used:",
                      selected = "spotter_pro",
                      choices = c("spotter_pro","whale_alert")),
            
           # p(div(img(src='humpback-whale-pic.jpg', height = 150, width = 250))),
            
           checkboxGroupInput(inputId = "species",
                              label = "Species",
                              choices = c("Blue Whale", "Gray Whale", "Humpback Whale", "Fin Whale", "Minke Whale"),
                              selected = "Humpback Whale")
           
            
        ), # close parenthesis for sidebarPanel
        
        # Create tabs for the reactive map, data, and summary of the data
        mainPanel(
          tags$style(type="text/css",
                     ".shiny-output-error { visibility: hidden; }",
                     ".shiny-output-error:before { visibility: hidden; }"
          ),
          tabsetPanel(
            
            #map
            tabPanel("Whale Map", width = "100%",
                     height = "100%",tags$style(type = "text/css", "#map {height: calc(100vh - 80px) !important;}"), leafletOutput("map"), helpText("Visualize observations of three endangered whale species recorded along the California Coast by selecting a year and a month range (1=Jan, 12=Dec). Bubble diameter corresponds to sighting size. Click the layer icon to toggle whale observation and shipping lane appearance.")),
            
    
            
            
            #data
            #tabPanel("Data", dataTableOutput("table")),
            
            
            #summary
            tabPanel("Summary", 
                   
                     h3("Data Collection Summary"),
                     p("This app presents data collected by volunteer citizen scientists using Spotter Pro, a smart phone app designed for observing marine species. The volunteers are part of the Channel Islands Naturalist Corps, a program started by Channel Island National Marine Sanctuary (CINMS) in the 1990s. The Naturalist Corps is comprised of over 160 volunteers who record data on marine life nearly every day aboard whale watching boats in the Santa Barbara Channel (SBC). When the program first started, sightings were recorded on paper logs. Since 2013, volunteers input data directly into the Spotter Pro mobile application while aboard marine vessels, typically the Condor Express whale watching boat out of Santa Barbara, CA. Only trained Naturalists can access the Spotter Pro app, but another CINMS mobile application called Whale Alert allows the general public to record sightings. This citizen-collected information has been used to create on of the largest datasets on marine mammals in the SBC, and was even used by CINMS to move shipping lanes in 2013 by one nautical mile to prevent whale ship strikes.
"),
                     h3("Shipping Lanes"),
                     p("Freight ships are one of the biggest direct threats to whales in our world's oceans, due to ship strikes. The data presented in this application was used by the National Oceanic and Atmospheric Administration to provide evidence for moving the international shipping lane along the California Coast by one nautical mile away from whale feeding areas in 2013. In part, this would not have been possible without the effort of the Channel Islands Naturalist Corps- and is a case study evidencing how citizen science can be applied to policy. 
"),
                     
p(div(img(src='whale.jpeg', height=400, width = 600)), a(br(em("Source: Condor Express")), href = "https://condorexpress.com/")), h6("Citizen scientists aboard the condor express take photos of Humpbacks that surfaced near the boat. These photos are used to identify individual whales as a part of the dataset created by Channel Islands Naturalist Corps volunteers.") ),

          #About us!
          tabPanel("Meet the Team", align="center",
         
         h3("About Us"),
         p("We are a team of graduate students at the UCSB Bren School of Environmental Science and Management. Our thesis project focuses on delineating methods and protocols for NOAA citizen science data to transform it into a format that can be easily applied to scientific and policy pursuits for whale monitoring in the Santa Barbara Channel. Our methods and final product can be applied to other citizen science projects to automatically format and clean data sets to enhance their use.",tags$li(a(href ="http://www.seatizenscience.org/",style = "color:black",
                       img(src='whale-fi_small.png',
                           title = "SEAtizen Science", height = "30px"),
                       style = "padding-top:10px; padding-bottom:10px;"),
                     class = "dropdown")),
        
         p(div(img(src="team2.JPG", height="100%", width = "100%"), style="text-align: center;"), h5("Left to Right: Charlene Kormondy, Niklas Greissbaum, Molly Williams, Sean Goral, Jasmine Vazin & Rae Fuhrman", align = "center", style = "color:black") )

                     
                     

           
          
        )
    )
)))


# Define server logic required to draw a histogram
server <- function(input, output) {
    
  w <- reactive({read.csv("newdata.csv")})
  

    # Creating the reactive output ('map')
  
    output$map <- renderLeaflet({
      
      whale_obs <- whale_shp %>%
        filter(month >= input$month[1] & month <= input$month[2]) %>% #filter BETWEEN function dplyr
        filter(year == input$year) %>%
        filter(Species == input$species) %>% 
        filter(app == input$app)
      
        
        whale_map <- 
          tm_basemap("Esri.WorldImagery") +
          tm_shape(ship_shp, name = "Shipping Lane (2013)", is.master = FALSE, group="ship_shp") +
          tm_polygons(col = "firebrick1", alpha = 0.5)+
          tm_shape(sanctuary_shp, name = "Channel Islands National Marine Sanctuary", is.master = FALSE, group="sanctuary_shp")+
          tm_polygons(col = "deepskyblue3", alpha = 0.5)+
          tm_shape(whale_obs) +
          tm_dots(size = "Whales Sighted", alpha = 0.5, col = "Species",
                  popup.vars = c("Date: " = "date",
                                   "Scientific Name:  " = "scntfcN",
                                   "Total Sighted: " = "Whales Sighted",
                                 "App Used: " = "app",
                                   "Occurrence ID:   " = "OccrnID"),
                  popup.format=list(OccrnID=list(format="s")))
        
        
        tmap_leaflet(whale_map) %>% 
          leaflet::hideGroup("Shipping Lane (2013)") %>% 
          leaflet::hideGroup("Channel Islands National Marine Sanctuary") %>%            
          addScaleBar(position = 'topright') %>% 
          addProviderTiles(providers$Hydda.RoadsAndLabels)
          
        
        })
        
        #Data Table
        output$table <- renderDataTable({ new<- w() })
        
        
}

# Run the application 
shinyApp(ui = ui, server = server)

