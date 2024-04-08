# load packages
library(shiny)
library(sf)
library(here)
library(tidyverse)
library(leaflet)

# Bring in data and manipulate 
nps <- sf::read_sf("/Users/denalistevens/Desktop/Spring 2024/SYE/SYE/NPS_shape/nps_boundary.shp") |>
  sf::st_transform('+proj=longlat +datum=WGS84')
p_names <- nps|>
  mutate(popup = paste0(nps$UNIT_NAME))


ui <- fluidPage(
  h1("National Park Map"),
  hr("This is data collected from the National Park Service to display the boundaries of national parks 
     both federally and non-federally owned"),
  sidebarLayout(
    sidebarPanel(),
    mainPanel(
      leafletOutput("map1")
              )
  )
)


server <- function(input, output) {

    output$map1 <- renderLeaflet({
      leaflet(nps) |>
        setView(lng = -98.583, lat = 39.833, zoom = 5) |>
        addTiles() |>
        addProviderTiles(providers$OpenStreetMap.Mapnik) |>
        addPolygons(color = "#006600",
                    weight = 1.5,
                    smoothFactor = .5,
                    opacity = 1.0, 
                    fillOpacity = 0.5,
                    highlightOptions = highlightOptions(color = "#003300", weight = 2,
                                                        bringToFront = TRUE),
                    popup = p_names$popup)
        
    })
}


shinyApp(ui = ui, server = server)
