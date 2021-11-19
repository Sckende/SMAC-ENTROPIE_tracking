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
library(shinyBS)

ui <- dashboardPage(
    dashboardHeader(disable = TRUE),
    dashboardSidebar(disable = TRUE),
    dashboardBody(
        
        column(width = 3,

        box(width = NULL,
            
            selectInput(inputId = 'an',
                        label = 'Année de déploiement',
                        choices = c('Toutes', '2017', '2018')),
            
            uiOutput('Vessel_ID')),
        
        # box(width = NULL,
        #     div(style = "font-size:25px; text-align:center",
        #         "Graphiques vitesse"),
        #     
        #     # plotlyOutput("hist_speed"),
        # 
        #     plotlyOutput("barplot_speed")
        #     )
        box(width = NULL,
            title = 'PTT id',
            textOutput('Bird_ID')),
        box(width = NULL,
            actionButton(inputId = 'histo_speed',
                         label = 'Speed histogram'),
            actionButton(inputId = 'bar_speed',
                         label = 'Speed barplot')
            )
        ),
        
        column(width = 9,
            box(width = NULL,
                height = 900,
                leafletOutput("mapplot", height = 880))),
            
            bsModal(id = 'myModal1',
                    title = 'Speed exploration',
                    trigger = 'histo_speed',
                    size = 'large',
                    plotOutput('sp_hist'))
        ,

            bsModal(id = 'myModal2',
                    title = 'Speed barplot',
                    trigger = 'bar_speed',
                    size = 'large',
                    plotOutput('sp_bar'))

        
    )
)

# Run the application
# shinyApp(ui = ui, server = server)
