# load packages
library(shiny)
library(sf)
library(here)
library(tidyverse)
library(leaflet)
library(plotly)
library(scales)
options(scipen = 999)

# Bring in data and manipulate 
nps <- sf::read_sf("/Users/denalistevens/Desktop/Spring 2024/SYE/SYE/NPS_shape/nps_boundary.shp") |>
  sf::st_transform('+proj=longlat +datum=WGS84')
p_names <- nps|>
  mutate(popup = paste0(nps$UNIT_NAME))


library("httr")
library("readxl")
GET("https://query.data.world/s/fr5k6pcrcweo7wr657vchbeq6f3cci?dws=00000", write_disk(tf <- tempfile(fileext = ".xlsx")))
df <- read_excel(tf)
years <- unique(df$YearRaw) 
type <- unique(df$`Unit Type`)
national_parks3 <- national_parks |> filter(YearRaw != "Total") |> filter(Visitors >= 1000000)

ui <- navbarPage("NavBar!",
                 tabPanel("Map",
                          h1("National Park Map"),
                          hr("This is data collected from the National Park Service to 
                          display the boundaries of national parks 
                          both federally and non-federally owned"),
                          leafletOutput("map1")),
                 tabPanel("Visitors Plot",
                          h1("Number of Visitors from 1904 - 2016"),
                          sidebarLayout(
                            sidebarPanel(
                              sliderInput("years_select", label = "Set Years for Graph",
                                          min = 1904,
                                          max = 2016,
                                          value = c(2000, 2016)),
                              # need to find a way to get rid of commas so it looks like years
                              selectInput("type_select", label = "Select Type to Graph",
                                          choices = type, multiple = TRUE),
                              sliderInput("min_visitors", label = "Set a Minimum Amount of Visitors",
                                          min = 0,
                                          max = 3000000,
                                          value = 1000000),
                            ),
                              mainPanel(
                                plotOutput("visitors_line")
                        
                              )
                            )
                          ))



server <- function(input, output) {

    output$map1 <- renderLeaflet({
      leaflet(joined_clean) |>
        setView(lng = -110, lat = 39.833, zoom = 2.5) |>
        addTiles() |>
        addProviderTiles(providers$OpenStreetMap.Mapnik) |>
        addPolygons(color = "#006600",
                    weight = 1.5,
                    smoothFactor = .5,
                    opacity = 1.0, 
                    fillOpacity = 0.5,
                    highlightOptions = highlightOptions(color = "#003300", weight = 2,
                                                        bringToFront = TRUE),
                    label = joined_clean$UNIT_NAME,
                    popup = label_text)
      
        
    })
    
    output$visitors_line <- renderPlot({
      ggplot(data = national_parks3, aes(x = YearRaw, y = Visitors, group = `Unit Name`)) +
        geom_line(aes(colour = `Unit Name`)) +
        scale_color_viridis_d() +
        scale_y_continuous(labels = scales::comma) +
        labs(title = "Visitors in National Parks from 2000 to 2016", subtitle = "Minimum visitors is set at 1,000,000") +
        theme_minimal()
     
    })
    
    
}


shinyApp(ui = ui, server = server)
