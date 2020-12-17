# load necessary packages
library(shiny)
library(leaflet)

library(dplyr)
library(plotly)
library(htmlwidgets)
library(DT)
library(lubridate)
library(shinyWidgets)
library(readr)
library(mapview)


tag<-read_rds("tag.rds")

ui <- fluidPage(
  list(tags$head(
    HTML('<link rel="icon", href="Rplot.png",
                       type="image/png" />'),
    
    tags$style(
      HTML(
        "
      .navbar .navbar-nav {float: right;
                           color: #ff3368;
                           font-size: 18px;
                           background-color: #FFFF00 ; }
      .navbar .navbar-header {float: left; }
       .navbar-default .navbar-brand { color: blue;
                                       height: 55px;
                                       font-size: 28px;
                                      }

  "
      )
    )
  )),
  navbarPage(
    title =  "Test",
    id = "Main",
    tabPanel(
      title = "Interactive Map",
      # Include the customised CSS
      div(
        class = "outer",
        tags$style(
          type = "text/css",
          ".outer {position: fixed; top: 41px; left: 0; right: 0; bottom: 0; padding: 0}"
        )
        ,
        leafletOutput(
          outputId = "map",
          width = "100%",
          height = "100%"
        ),
        absolutePanel(
          id = "controls",
          class = "panel panel-default",
          style = 'overflow-y:scroll;max-height:350px;',
          fixed = FALSE,
          draggable = FALSE,
          top = 15,
          left = "auto",
          right = 10,
          bottom = 5,
          width = 400,
          selectInput(
            inputId = "year",
            label = "Release Year",
            choices = c(unique(tag$Year)),
            selected = max(tag$Year)
          ),
          br(),br(),br(),br(),
          downloadButton(outputId = "dl", label = "Download Map"))))))

server <- function(input, output, session) {
  
  events <- reactive({
    filter(tag, Year == input$year)
  })
  
  foundational.map <- reactive({
    leaflet() %>%
      addProviderTiles(providers$Esri.OceanBasemap) %>%
      setView(lng = -3.5,
              lat = 53.8,
              zoom = 6)
})
  # render  leaflet map
  output$map <- leaflet::renderLeaflet({
    foundational.map()
    
  })
  
  observe({
    tagging_events <- events()
    col <-
      colorNumeric("viridis", tagging_events$numbercodtagged, n = 5)
    leafletProxy('map') %>%
      clearMarkers() %>% clearControls() %>%
      addCircleMarkers(
        lng = tagging_events$Longitude,
        lat = tagging_events$Latitude,
        radius = tagging_events$numbercodtagged / 3,
        color = col(tagging_events$numbercodtagged),
        stroke = FALSE,
        fillOpacity = 0.9,
        popup = paste(
          "<b>Long:</b> ",
          round(tagging_events$Longitude, 4),
          "<br />",
          "<b>Lat</b>: ",
          round(tagging_events$Latitude, 4),
          "<br />",
          "<b>Year</b>",
          tagging_events$Year,
          "<br />",
          "<b>No. fish tagged</b>",
          tagging_events$numbercodtagged,
          "<br />",
          "<b>Survey</b>",
          tagging_events$Survey_ID,
          "<br />",
          "<b>Station</b>",
          tagging_events$Station
        )
      ) %>%
      flyTo(lng = -3.5,
            lat = 53.8,
            zoom = 7) %>%
      addLegend(
        "bottomleft",
        pal = col,
        values = tagging_events$numbercodtagged,
        title = "Number of Cod tagged"
      )
  })
  
  # store the current user-created version
  # of the Leaflet map for download in
  # a reactive expression
  user.created.map <- reactive({
    # call the foundational Leaflet map
    tagging_events <- events()
    col <-
      colorNumeric("viridis", tagging_events$numbercodtagged, n = 5)
    foundational.map() %>%
      addCircleMarkers(
        lng = tagging_events$Longitude,
        lat = tagging_events$Latitude,
        radius = tagging_events$numbercodtagged / 3,
        color = col(tagging_events$numbercodtagged),
        stroke = FALSE,
        fillOpacity = 0.9,
        popup = paste(
          "<b>Long:</b> ",
          round(tagging_events$Longitude, 4),
          "<br />",
          "<b>Lat</b>: ",
          round(tagging_events$Latitude, 4),
          "<br />",
          "<b>Year</b>",
          tagging_events$Year,
          "<br />",
          "<b>No. fish tagged</b>",
          tagging_events$numbercodtagged,
          "<br />",
          "<b>Survey</b>",
          tagging_events$Survey_ID,
          "<br />",
          "<b>Station</b>",
          tagging_events$Station
        )
      ) %>%
      addLegend(
        "bottomleft",
        pal = col,
        values = tagging_events$numbercodtagged,
        title = "Number of Cod tagged"
      ) %>%
      # store the view based on UI
      setView(
        lng = input$map_center$lng
        ,
        lat = input$map_center$lat
        ,
        zoom = input$map_zoom
      )
  })
  
  # create the output file name
  # and specify how the download button will take
  # a screenshot - using the mapview::mapshot() function
  # and save as a PDF
  output$dl <- downloadHandler(
    filename = paste0(Sys.Date()
                      , "_customLeafletmap"
                      , ".pdf")
    
    ,
    content = function(file) {
      mapshot(
        x = user.created.map()
        ,
        file = file
        ,
        cliprect = "viewport"
        #the clipping rectangle matches the height & width from the viewing port
        ,
        selfcontained = FALSE
        #when this was not specified, the function for produced a PDF of two pages:
        #one of the leaflet map, the other a blank page.
      )
    } # end of content() function
  ) # end of downloadHandler() function
    
  }
  
# run the Shiny app
shinyApp(ui = ui, server = server)
  