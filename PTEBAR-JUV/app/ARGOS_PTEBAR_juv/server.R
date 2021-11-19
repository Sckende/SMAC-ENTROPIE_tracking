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
library(sp)
library(rgdal)
library(mapview)
library(plotly)
library(leaflet)
library(lubridate)
library(shinyBS)
 
argos.ls <- readRDS("C:/Users/ccjuhasz/Desktop/SMAC/Projet_publi/X-PTEBAR_argos_JUV/DATA/PTEBAR_JUV_Pinet_data_CLEANED_speed.rds")

# argos <- read.table("C:/Users/ccjuhasz/Desktop/SMAC/Projet_publi/X-PTEBAR_argos_JUV/DATA/PTEBAR_JUV_Pinet_data_CLEANED_noSPATIAL.txt",
                    # h = T,
                    # sep = '\t')

argos <- do.call('rbind', argos.ls)
argos$year <- year(argos$deploy)
color <- viridis::inferno(length(unique(argos$Vessel)))
argos$color <- color[as.integer(argos$Vessel)]
coords <- as.data.frame(st_coordinates(argos))
argos <- cbind(argos, coords)
argos$ID <- paste(argos$Vessel, 1:dim(argos)[1], sep = '-')


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

# argos <- argos[argos$Vessel %in% unique(argos$Vessel)[1:3],]


shinyServer(function(input, output) {
    
    # ---------- #
    # Première liste déroulante pour sélection des années de déploiement
    
    obs_an = reactive({
        if(input$an == 'Toutes'){
            argos
        } else {
            argos[argos$year == input$an,]
        }
    })
    
    # ---------- #
    # Check boxes pour les balises argos reliées à la liste déroulante 'an' 
    
    output$Vessel_ID <- renderUI({
        
        checkboxGroupInput(inputId = 'checkGroup',
                           label = 'Device ID',
                           choices = unique(as.character(obs_an()$Vessel)),
                           selected = unique(as.character(obs_an()$Vessel)))
        
    })
    
    # ---------- #
    # Interactive map creation
    
    output$mapplot <- renderLeaflet({
        
            leaflet() %>%
                addTiles() %>% # Affichage du fond de carte
                addCircleMarkers(data = obs_an(),
                                 lng = ~ X,
                                 lat = ~ Y,
                                 radius = 4, # taille du cercle
                                 label = ~ Vessel,
                                 popup = ~ popup_info,
                                 color = ~ color,
                                 fill = T,
                                 group = 'Vessel_ID',
                                 stroke = F,
                                 fillOpacity = 1
                                 )
                                             
    })
    
    mydata_filtered <- reactive({
        obs_an()[obs_an()$Vessel %in% unique(input$checkGroup),]
    })
    
    observeEvent(input$checkGroup, {
        
        leafletProxy('mapplot', data = mydata_filtered()) %>% 
            clearGroup('Vessel_ID') %>% 
            addCircleMarkers(lng = ~ X,
                       lat = ~ Y,
                       radius = 4, # taille du cercle
                       label = ~ Vessel,
                       popup = ~ popup_info,
                       color = ~ color,
                       fill = T,
                       stroke = F,
                       fillOpacity = 1,
                       layerId = ~ ID, # indispensable pour obtenir un id lors d'un click event
                       group = 'Vessel_ID') # Utilisé pour utiliser checkboxGroupInput
    })
    
    # -------------------- #
    # Click on map marker #
    # ------------------ #
    
    observe({
        
        event <- input$mapplot_marker_click # xxxx_marker_click avec xxxx correspondant au nom de la map
        print(event)
        
        output$Bird_ID <- renderText(as.character(mydata_filtered()$Vessel[mydata_filtered()$ID == event$id]))
        # Obtention des plots de vitesse par balise argos
        
        output$sp_hist <- renderPlot({

            if(is.null(event))
                return(NULL)
            
            PTT <- as.character(mydata_filtered()$Vessel[mydata_filtered()$ID == event$id])
            print(PTT)

            speed <- mydata_filtered()$speed.m.sec[mydata_filtered()$Vessel == PTT]
            

            # hist <- plot_ly(x = speed,
            #                  type = 'histogram')
            # print(hist)
            
            hist(speed,
                 breaks = round(max(speed, na.rm = T)))
        })
        
        output$sp_bar <- renderPlot({

            if(is.null(event))
                return(NULL)

            PTT <- as.character(mydata_filtered()$Vessel[mydata_filtered()$ID == event$id])
            speed <- mydata_filtered()$speed.m.sec[mydata_filtered()$Vessel == PTT]

            bar <- barplot(speed)
        })
    })

})


