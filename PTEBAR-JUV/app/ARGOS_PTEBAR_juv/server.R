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

# argos <- read.table("C:/Users/ccjuhasz/Desktop/SMAC/Projet_publi/X-PTEBAR_argos_JUV/DATA/PTEBAR_JUV_Pinet_data_CLEANED_noSPATIAL.txt",
                    # h = T,
                    # sep = '\t')

argos <- do.call('rbind', argos.ls)
argos$year <- year(argos$deploy)
color <- viridis::inferno(length(unique(argos$Vessel)))
argos$color <- color[as.integer(argos$Vessel)]

argos <- argos %>%
    mutate(popup_info = paste0("<b> PTT </b> ",
                               Vessel,
                               "<br/>","<b> Date de déploiement</b> ",
                               deploy,
                               "<br/>",
                               "<b> Date de relocalisation</b> ",
                               Date,
                               "<br/>",
                               "<b> Numéro de burst</b> ",
                               point.group,
                               "<br/>",
                               "<b> Délai avec le point précédent en heure</b> ",
                               round(-1 * delay, digits = 2),
                               "<br/>",
                               "<b> Vitesse en m/s</b> ",
                               round(speed.m.sec, digits = 2)))


shinyServer(function(input, output) {
    
    obs_an = reactive({
        if(input$an == 'Toutes'){
            argos
        } else {
            argos[argos$year == input$an,]
        }
    })
    
    output$Vessel_ID <- renderUI({
        
        checkboxGroupInput('device',
                    'Device ID',
                    unique(as.character(obs_an()$Vessel)),
                    selected = unique(as.character(obs_an()$Vessel)))
        
    })
    
    output$mapplot <- renderLeaflet({
        
            leaflet(obs_an()) %>%
                addTiles() %>% # Affichage du fond de carte
                addCircleMarkers(radius = 4, # taille du cercle
                                 popup = obs_an()$popup,
                                 color = obs_an()$color)
                # addCircleMarkers(data = argos.ls[[1]],
                #                  layerId = 1:dim(argos.ls[[1]])[1],
                #                  group = '162070',
                #                  radius = 2,
                #                  fill = T,
                #                  # popup = argos.ls[[1]]$popup,
                #                  color = color[1]) %>% 
                # addCircleMarkers(layerId = 1:dim(argos.ls[[2]])[1],
                #              group = '162071',
                #              radius = 2,
                #              fill = T,
                #              # popup = argos.ls[[2]]$popup,
                #              color = color[2])
        
        
        # addCircleMarkers(layerId=df$id[1:2], df$lng[1:2], df$lat[1:2], group='one', radius=2, fill = TRUE,color='red') %>%
        #     addCircleMarkers(layerId=df$id[3], df$lng[3], df$lat[3], group='two', radius=2, fill = TRUE,color='red') # Ajout de fenêtres pop-up
                                             
    })

})


