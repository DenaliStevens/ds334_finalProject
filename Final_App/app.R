# load packages
library(shiny)
library(sf)
library(here)
library(tidyverse)
library(leaflet)
library(plotly)
library(scales)
library(rvest)
library(stringr)
library(htmltools)
library(glue)
library("httr")
library("readxl")
library(plotly)
options(scipen = 999)

# Bring in data and manipulate 
nps <- sf::read_sf("/Users/denalistevens/Desktop/Spring 2024/SYE/SYE/NPS_shape/nps_boundary.shp") |>
  sf::st_transform('+proj=longlat +datum=WGS84')
p_names <- nps|>
  mutate(popup = paste0(nps$UNIT_NAME))

# need to make leaflet plot 
url <- "https://en.wikipedia.org/wiki/List_of_national_parks_of_the_United_States"
h <- read_html(url)
tab <- h |> html_nodes("table")
wiki_np_data <- tab[[1]] |> html_table()

wiki_np_clean <- wiki_np_data |> mutate(wiki_name = str_replace_all(Name, "[^[:alnum:]]", " ")) |>
  relocate(wiki_name)
nps_clean <- nps |> mutate(nps_name = str_remove_all(UNIT_NAME, " National") |>
                             str_remove_all(" Historical Site") |> 
                             str_remove_all( " Historical Park") |>
                             str_remove_all( " Historical Trail") |>
                             str_remove_all( " Park")) |>
  relocate(nps_name)

joined2 <- nps_clean |>
  stringdist_inner_join(wiki_np_clean, by = c("nps_name" = "wiki_name"), max_dist = 3) |>
  relocate(wiki_name, nps_name)
names_2 <- joined2 |>
  mutate(popup = paste0('<a href =', joined2$METADATA, '>',
                        joined2$UNIT_NAME, '</a>'))
joined_clean <- joined2 |> rename(Established = `Date established as park[12]`, Area = `Area (2023)[8]`, Visitors_2022 = `Recreation visitors (2022)[11]`) 

joined_clean <- joined_clean |> mutate(popup = paste(
  "Name: ", UNIT_NAME,
  "Date of Establishment: ", Established,
  "Visitors in 2022: ", Visitors_2022,
  "Description of Park: ", Description))
label_text <- glue(
  "<b> Name: </b> {joined_clean$UNIT_NAME}<br/>",
  "<b> Established: </b> {joined_clean$Established}<br/>",
  "<b> Visitors in 2022: </b> {joined_clean$Visitors_2022}<br/>",
  "<b> Description: </b> {joined_clean$Description}<br/>") |>
  lapply(htmltools::HTML)

# need to make visitors plot
GET("https://query.data.world/s/fr5k6pcrcweo7wr657vchbeq6f3cci?dws=00000", write_disk(tf <- tempfile(fileext = ".xlsx")))
df <- read_excel(tf)
df2 <- df |> filter(YearRaw >= "2000")
just_years <- df2 |> filter(YearRaw != "Total")
national_parks <- df2 |> filter(`Unit Type` == "National Park")
national_parks2 <- national_parks |> filter(YearRaw != "Total")
national_parks3 <- national_parks |> filter(YearRaw != "Total") |> filter(Visitors >= 1000000)

# need to make visitors page input options 
years <- unique(df$YearRaw) 
type <- unique(df$`Unit Type`)



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
                              selectInput("type_select", 
                                          label = "Select Type to Graph",
                                          choices = type,
                                          selected = NULL),
                              sliderInput("min_visitors", 
                                          label = "Set a Minimum Amount of Visitors",
                                          min = 0,
                                          max = 3000000,
                                          value = 1000000),
                            ),
                              mainPanel(
                                plotlyOutput("visitors_line")
                        
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
    
    
    output$visitors_line <- renderPlotly({
      plot3 <- ggplot(data = df, aes(x = YearRaw, y = Visitors, group = .data[[input$type_select]])) +
        geom_line(aes(colour = .data[[input$type_select]])) +
        scale_color_viridis_d() +
        scale_y_continuous(labels = scales::comma) +
        labs(title = "Visitors in National Parks from 2000 to 2016", subtitle = "Minimum visitors is set at 1,000,000") +
        theme_minimal()
      ggplotly(plot3)
     
    })
    
    
}


shinyApp(ui = ui, server = server)
