#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinydashboard)
library(plotly)
library(leaflet)

ui <- dashboardPage(
    dashboardHeader(disable = TRUE),
    dashboardSidebar(disable = TRUE),
    dashboardBody(
        
        column(width = 3,

        box(width = NULL,
            
            selectInput('an',
                        'Année de déploiement',
                        c('Toutes', '2017', '2018')),
            
            uiOutput('Vessel_ID')),
        
        box(width = NULL,
            div(style = "font-size:25px; text-align:center",
                "Graphiques vitesse"),
            
            # plotlyOutput("hist_speed"),
            # 
            # plotlyOutput("barplot_speed")
            )
        ),
        
        column(width = 9,
            box(width = NULL,
                height = 900,
                leafletOutput("mapplot", height = 880))
        )
    )
)

# Run the application
# shinyApp(ui = ui, server = server)
