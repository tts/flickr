library(shiny)
library(tidyverse)
library(leaflet)
library(htmltools)

data <- readRDS("flickr_geo_coded.RDS")

countries <- as.vector(sort(unique(data$country)))

shiny::shinyApp(
  
  ui = bootstrapPage(
    title = "My geotagged Flickr photos",
    tags$style(type = "text/css", "html, body {width:100%;height:100%}"),
    tags$style(
      'div.leaflet-popup-content-wrapper {
              width: 200px;
              height: 200px;
              opacity: .9;
            }'
    ),
    tags$style(
      'div.leaflet-popup-content {
              width: 150px;
              height: 150px;
            }'
    ),
    leafletOutput("map", width = "100%", height = "100%"),
    absolutePanel(bottom = 400, right = 20,
                  draggable = TRUE,
                  selectInput(inputId = "country",
                              label = "Country",
                              choices = c("All", countries),
                              selected = NULL)),
    absolutePanel(top = 20, left = 100,
                  draggable = TRUE,
                  htmlOutput(outputId = "title"))),
  
  server = function(input, output, session) {
    
    output$title <- renderUI(HTML("<b>My geotagged Flickr photos since June 2005</b> | Tuija Sonkkila | 2021-05-27 | <a href='https://github.com/tts/flickr'>https://github.com/tts/flickr</a>"))
    
    country_selected <- reactive({
      if(input$country == "All") {
        return(data)
      } else {
        data %>%
          filter(country == input$country)
      }
    })
    
    
    output$map <- renderLeaflet({
      m <- leaflet(data = country_selected()) %>%
        addTiles() %>% 
        addMarkers(clusterOptions = TRUE, 
                   group = "country",
                   label = ~lapply(paste0("<b>Date/time taken:</b> ", country_selected()$datetaken, 
                                          "<br><b>Views:</b> ", country_selected()$count_views,
                                          "<br><b>Faves:</b> ", country_selected()$count_faves), HTML),
                   popup = ~popup_img,
                   options = popupOptions(closeButton = FALSE))
    })
    
  }
  
)




