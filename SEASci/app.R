#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyverse)
library(shinythemes)
library(RColorBrewer)
library(dbplyr)
library(RSQLite)
library(stringr)
library(lubridate)

#Now we'll get our data:

whales<- read_csv("sp_obis_westcoast.csv")

whalesdf <- as.data.frame(whales)

whalesdf$vernacularName <- as.factor(whalesdf$vernacularName)
class(whalesdf$vernacularName)

new<- whalesdf %>% 
  filter(vernacularName == "Blue Whale"| vernacularName =="Gray Whale"| vernacularName =="Humpback Whale") 

ymd_hms(new$EventDate) 
new$EventDate<- year(new$EventDate)



#Create the user interface

ui <- fluidPage(
  
  theme = shinytheme("slate"),
  titlePanel("Marine Mammel Sightings"),
  sidebarLayout(
    sidebarPanel = (
      radioButtons("side",
                   "Species",
                   c("Humpback Whale",
                     "Blue Whale",
                     "Gray Whale"))
    ),
    
    mainPanel(
      plotOutput(outputId="whaleplot")
    )
    
  )
)

server <- function(input, output) {
  
  output$whaleplot <- renderPlot({
    
    new %>% 
      ggplot(aes(EventDate))+
      geom_density(aes(fill = factor(vernacularName)), alpha = 0.5)+
      theme(axis.text.y = element_blank())+
      xlim(2013,2017)+
      theme(plot.title = element_text(hjust=0.5, size = 16, face = "bold"))+
      labs(title = "Figure 1: Cetaceans In Channel", 
           x = "Year", 
           y = "Density of Cetaceans", 
           fill = "vernacularName")
    
    new %>% 
      ggplot(aes(x = vernacularName, y = EventDate))+
      geom_violin(fill = "lavenderblush3")+
      ylim(2013,2017)+
      theme(plot.title = element_text(hjust=0.5, size = 16, face = "bold"))+
      labs(title = "Figure 2: Cetaceans in Channel", 
           x = "Density of Cetaceans",
           y = "Year")
    
  })
  
  
}

# Run the application 
shinyApp(ui = ui, server = server)

