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
              width: 600px;
              height: 400px;
              opacity: .9;
            }'
        ),
        tags$style(
          'div.leaflet-popup-content {
              width: 600px;
              height: 400px;
            }'
        ),
      leafletOutput("map", width = "100%", height = "100%"),
      absolutePanel(bottom = 400, right = 20,
                    draggable = TRUE,
                    selectInput(inputId = "country",
                                label = "Country",
                                choices = countries,
                                selected = "Finland"),
                    selectInput(inputId = "year",
                                label = "Year",
                                choices = NULL,
                                selected = NULL)
                    # HTML("<div><br/><a target='blank' href='http://tuijasonkkila.fi/blog/'>[About TBA]</a></div>")
      )
    ),
    
    server = function(input, output, session) {
      
      country_selected <- reactive({
        data %>%
          filter(country == input$country)
      })
      
      
      observe(
        updateSelectInput(session,
                          inputId = 'year',
                          choices = as.vector(sort(unique(lubridate::year(country_selected()$datetaken))))
                          )
        )

      country_year_selected <- reactive({
        country_selected() %>% 
          filter(lubridate::year(datetaken) == input$year)
      })

        
      output$map <- renderLeaflet({
        
        m <- leaflet(data = country_year_selected()) %>%
          addTiles() %>% 
          addMarkers(clusterOptions = TRUE, 
                     popup = ~popup_img,
                     options = popupOptions(closeButton = FALSE))
      })
      
    }

)




