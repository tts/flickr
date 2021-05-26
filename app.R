library(shiny)
library(tidyverse)
library(leaflet)
library(htmltools)

data <- readRDS("flickr_geo_coded.RDS")

countries <- as.vector(sort(unique(data$country)))

shiny::shinyApp(

    ui = bootstrapPage(
      tags$style(type = "text/css", "html, body {width:100%;height:100%}"),
      tags$style(
          'div.leaflet-popup-content-wrapper {
              width: 400px;
              opacity: .5;
            }'
        ),
        tags$style(
          'div.leaflet-popup-content {
              width: 280px;
            }'
        ),
      leafletOutput("map", width = "100%", height = "100%"),
      absolutePanel(bottom = 400, right = 20,
                    draggable = TRUE,
                    selectInput(inputId = "country",
                                label = "Country",
                                choices = countries,
                                selected = "Finland"),
                    HTML("<div><br/><a target='blank' href='http://tuijasonkkila.fi/blog/2018/01/streets-of-helsinki/'>[About TBA]</a></div>")
      )
    ),
    
    server = function(input, output, session) {
      
      country_selected <- reactive({
        data %>%
          filter(country == input$country)
      })
      
      
      output$map <- renderLeaflet({
        
        m <- leaflet(data = country_selected()) %>%
          addTiles() %>% 
          addMarkers(clusterOptions = TRUE, 
                     popup = ~popup)
      })
      
    }

)




