#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(sf)
library(mapview)
library(plotly)
library(leaflet)
library(lubridate)
 
argos.ls <- readRDS("C:/Users/ccjuhasz/Desktop/SMAC/Projet_publi/X-PTEBAR_argos_JUV/DATA/PTEBAR_JUV_Pinet_data_CLEANED_speed.rds")

argos <- do.call('rbind', argos.ls)
argos$year <- year(argos$deploy)



shinyServer(function(input, output) {
    
    obs_an = reactive({
        if(input$an == 'Toutes'){
            argos
        } else {
            argos[argos$year == input$an,]
        }
    })
    
    output$Vessel_ID <- renderUI({
        
        selectInput('device',
                    'Device ID',
                    c('Tous', unique(as.character(obs_an()$Vessel))))
        
    })
    
    output$mapplot <- renderLeaflet({
        
            leaflet(obs_an()) %>%
                addTiles() %>% # Affichage du fond de carte
                addCircleMarkers(radius = 4, # taille du cercle
                                 popup = NA) # Ajout de fenêtres pop-up
                                             
    })

})


