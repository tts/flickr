library(shiny)
library(shinyMobile)
library(leaflet)
library(tidyverse)
library(htmltools)


data <- readRDS("flickr_geo_coded.RDS")

countries <- as.vector(sort(unique(data$country)))


shiny::shinyApp(
  
  ui = f7Page(
    
    tags$head(
      tags$style(HTML(":root {--f7-theme-color: #d1ae20}")),
      tags$style(
        'div.leaflet-popup-content-wrapper {
              width: 200px;
              height: 250px;
              opacity: .9;
            }'
      ),
      tags$style(
        'div.leaflet-popup-content {
              width: 150px;
              height: 150px;
            }'
      )
      ),
    title = "My geotagged Flickr photos", 
    preloader = FALSE, 
    allowPWA = FALSE,
    
    options = list(
      theme = c("auto"),
      dark = TRUE,
      filled = FALSE,
      touch = list(
        tapHold = TRUE,
        tapHoldDelay = 750,
        iosTouchRipple = FALSE
      ),
      iosTranslucentBars = FALSE,
      navbar = list(
        iosCenterTitle = TRUE,
        hideNavOnPageScroll = TRUE
      ),
      toolbar = list(hideNavOnPageScroll = FALSE),
      pullToRefresh = FALSE
    ),
    
    f7TabLayout(
      navbar = f7Navbar(
        subNavbar = NULL,
        title = "My geotagged Flickr photos (37% of all) since June 2005",
        hairline = TRUE,
        shadow = TRUE,
        bigger = FALSE,
        transparent = FALSE,
        leftPanel = TRUE,
        rightPanel = FALSE
      ),
      
      panels = tagList(
        f7Panel(
          side = "left",
          id = "leftpanel",
          theme = c("light"),
          effect = "cover",
          resizable = TRUE,
          
          f7BlockTitle(title = "Country", size = 'medium'),
          
          f7Card(
            f7SmartSelect(inputId = "country",
                          label = "Country",
                          choices = c("All", countries),
                          selected = NULL,
                          openIn = "popup")
          ),
          
          f7Card(
            textOutput(outputId = "note")
          )
        )
      ),
      
      
      f7Tabs(animated=FALSE, id="tabs", style = c("toolbar"),
             
             f7Tab(
               tabName = "Map",
               icon = f7Icon("map"),
               active = TRUE,
               
               f7Row(
                 f7Col(
                   f7Card(
                     leafletOutput(outputId = "map")
                   )
                 )
               )
             ),
             
             
             f7Tab(
               tabName = "About",
               icon = f7Icon("question_circle"),
               active = FALSE,
               
               f7Row(
                 f7Col(
                   f7Card(
                     f7Text(inputId = "aboutdata", label = "All data via Flickr API with the photosearcher package", value = " "),
                     f7List(
                       f7ListItem(
                         f7Link(label = "Flickr API", href = "https://www.flickr.com/services/api/")
                       )
                     )
                   ),
                   f7Card(
                     f7Text(inputId = "aboutother", label = "Code", value = " "),
                     f7List(
                       f7ListItem(
                         f7Link(label = "R code of this app", href="https://github.com/tts/flickr")
                       )
                     )
                   )
                 )
               )
             )
      )
    )
  ),
  
  
  server = function(input, output, session) {
    
    country_selected <- reactive({
      if(input$country == "All") {
        return(data)
      } else {
        data %>%
          filter(country == input$country)
      }
    })
    
    
    output$map <- renderLeaflet({
      
        m <- leaflet(country_selected()) %>%
          addTiles() 
        
        m <- m %>% 
          addMarkers(clusterOptions = TRUE, 
                     popup = ~paste0(popup_img,
                                     "<b>Taken at:</b> ", country_selected()$datetaken, 
                                     "<br><b>Views:</b> ", country_selected()$count_views,
                                     "<br><b>Faves:</b> ", country_selected()$count_faves),
                     options = popupOptions(closeButton = FALSE))
        
        m
      
    })
  
    
    
    output$note <- renderText({
      "Click the blue marker to see a small size photo, and basic info. Clicking the photo shows the photo in bigger size."
    })
    
    
  }
  
)

