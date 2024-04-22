# Everything for map 

## Packages and data for first map 
library(sf)
library(tidyverse)
library(leaflet)
library(here)
library(plotly)
# I downloaded the data from this website and used the shp file 
# https://public-nps.opendata.arcgis.com/datasets/nps::nps-boundary-4/explore?location=37.015375%2C-81.906543%2C6.00
nps <- sf::read_sf("NPS_Land/nps_boundary.shp") |>
  sf::st_transform('+proj=longlat +datum=WGS84')

#nps <- sf::read_sf("NPS_shape/nps_boundary.shp") |>
  #sf::st_transform('+proj=longlat +datum=WGS84')
# why did this stop working

p_names <- nps|>
  mutate(popup = paste0('<a href =', nps$METADATA, '>',
                        nps$UNIT_NAME, '</a>'))


## Scrape for more info about National Park and clean it so I can merge with existing data 
library(rvest)
library(stringr)
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

## Merge with existing data for map 
library(fuzzyjoin)
joined2 <- nps_clean |>
  stringdist_inner_join(wiki_np_clean, by = c("nps_name" = "wiki_name"), max_dist = 3) |>
  relocate(wiki_name, nps_name)
names_2 <- joined2 |>
  mutate(popup = paste0('<a href =', joined2$METADATA, '>',
                        joined2$UNIT_NAME, '</a>'))

## Okay this has gotten me to the point where I can now plot all of the parks I have the extended info from wiki for. 
# Now I need to figure out have to have that information pop up when the park is clicked on. 
# I also want just the park name to appear when the mouse hovers over it. 
joined_clean <- joined2 |> 
  rename(Established = `Date established as park[12]`,
         Area = `Area (2023)[8]`, 
         Visitors_2022 = `Recreation visitors (2022)[11]`) 
joined_clean <- joined_clean |> mutate(popup = paste(
  "Name: ", UNIT_NAME,
  "Date of Establishment: ", Established,
  "Visitors in 2022: ", Visitors_2022,
  "Description of Park: ", Description))
library(htmltools)
library(glue)
label_text <- glue(
  "<b> Name: </b> {joined_clean$UNIT_NAME}<br/>",
  "<b> Established: </b> {joined_clean$Established}<br/>",
  "<b> Visitors in 2022: </b> {joined_clean$Visitors_2022}<br/>",
  "<b> Description: </b> {joined_clean$Description}<br/>") |>
  lapply(htmltools::HTML)



# need to make visitors plot
library("httr")
library("readxl")
GET("https://query.data.world/s/fr5k6pcrcweo7wr657vchbeq6f3cci?dws=00000", write_disk(tf <- tempfile(fileext = ".xlsx")))
df <- read_excel(tf)
df <- df |> rename(Year = YearRaw) |> rename(Type = `Unit Type`) |> rename(Name = `Unit Name`)
just_years <- df |> filter(Year != "Total")
just_np <- df |> filter(Type == "National Park")

# need to make visitors page input options 
years <- unique(df$Year) 
type <- unique(df$Type)


ui <- navbarPage("",
                 tabPanel("Map",
                          h1("National Park Map"),
                          hr("Click on any green highlighted area to learn more about a National Park!"),
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
      leaflet(nps) |>
        setView(lng = -110, lat = 39.833, zoom = 2.5) |>
        addTiles() |>
        addProviderTiles(providers$OpenStreetMap.Mapnik) |>
        addPolygons(color = "#09008a",
                    weight = .5,
                    smoothFactor = .5,
                    opacity = 1.0, 
                    label = nps$UNIT_NAME) |>
        addPolygons(color = "#006400",
                    weight = 1.5,
                    smoothFactor = .5,
                    opacity = 1.0, 
                    fillOpacity = 0.5,
                    highlightOptions = highlightOptions(color = "#003b00", weight = 2,
                                                        bringToFront = TRUE),
                    label = joined_clean$UNIT_NAME,
                    popup = label_text,
                    data = joined_clean)
    })

    
    year_reactive <- reactive({
      just_np_years <- just_np |> filter(Year != "Total") |> 
        filter(Year >= input$years_select[1]) |>
        filter(Year <= input$years_select[2]) |> 
        filter(Visitors >= input$min_visitors) 
      just_np_years
    })
    
  
    output$visitors_line <- renderPlotly({
      plot1 <- ggplot(data = year_reactive(), aes(x = Year, y = Visitors, group = Name,
                                                         label = Name,
                                                         label2 = Year,
                                                         label3 = Visitors)) +
        geom_line(aes(colour = Name), show.legend = FALSE) +
        scale_color_viridis_d() +
        scale_y_continuous(labels = scales::comma) +
        labs(title = str_glue("Visitors in National Parks from {input$years_select[1]} to {input$years_select[2]}"),
             caption = ("Minimum Visitors is set at ")) +
        theme(legend.position = "none")
        
      
      ggplotly(plot1, tooltip = c("label", "label2", "label3"))
      
      
    })
    
    
}


shinyApp(ui = ui, server = server)
