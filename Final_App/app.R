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

# data for joined map of just national parks with more information
# created in CleaningData.qmd
load("joined_map_data.rda")

# Create labels that users see when clicking on a park 
label_text <- glue(
  "<b> Name: </b> {joined_clean$UNIT_NAME}<br/>",
  "<b> Established: </b> {joined_clean$Established}<br/>",
  "<b> Visitors in 2022: </b> {joined_clean$Visitors_2022}<br/>",
  "<b> Description: </b> {joined_clean$Description}<br/>") |>
  lapply(htmltools::HTML)


# need to make visitors plot
library("httr")
library("readxl")
library(scales)
options(scipen = 999)
GET("https://query.data.world/s/fr5k6pcrcweo7wr657vchbeq6f3cci?dws=00000", write_disk(tf <- tempfile(fileext = ".xlsx")))
df <- read_excel(tf)
np_visitors <- df |> 
  rename(Year = YearRaw) |> 
  rename(Type = `Unit Type`) |> 
  rename(Name = `Unit Name`) |>
  filter(Type == "National Park") |>
  filter(Year != "Total") |>
  filter(Year >= 1995 & Year <= 2016) 
plot_parks <- unique(np_visitors$Name) |> 
  sort()


# stuff for datatables
library(DT)
All_Parks <- nps |> select(UNIT_NAME, UNIT_TYPE, STATE, METADATA ) |> st_drop_geometry()
big_parks <- unique(All_Parks$UNIT_TYPE) |> sort()
big_states <- unique(All_Parks$STATE) |> sort()

load("joined_display.rda")
Just_National_parks <- joined_display |>
  st_drop_geometry()

little_states <- unique(Just_National_parks$State) |>
  sort()



## attempting to make it so both data sets can be used for the table with appropriate drop downs 
parameter_tabs <- tabsetPanel(
  id = "params",
  type = "hidden",
  tabPanel("All_Parks",
           selectizeInput("Unit_types", "Select Classification(s) of Park",
                          choices = big_parks,
                          selected = NULL,
                          multiple = TRUE),
           selectizeInput("State", "Select State(s)",
                          choices = big_states,
                          selected = NULL,
                          multiple = TRUE)
  ),
  tabPanel("Just_National_parks", 
           selectInput("States", "Select State(s)",
                       choices = little_states,
                       selected = NULL,
                       multiple = TRUE)
  )
)


ui <- navbarPage("",
                 tabPanel("Map",
                          fluidPage(
                            h1("National Park Map"),
                            hr(strong("This map includes national parks along with historic sites, trails, recreational areas, 
                             and other designations created by the National Park Service.")),
                            br("Any area highlighted in green is classified as a National Park and more information is 
                             avaible when clicked on."),
                            
                            leafletOutput("map1"),
                            fluidRow(
                              
                              column(4,
                                     selectInput("datasets", "Select Which Dataset to View",
                                                 choices = c("All_Parks", "Just_National_parks"))
                              ),
                              parameter_tabs
                              
                            ),
                            
                            DT::dataTableOutput("table1")
                          )),
                 tabPanel("Collective Visitors Plot",
                          h1("Number of Visitors In All National Parks from 1995 - 2016"),
                          sidebarLayout(
                            sidebarPanel(
                              sliderInput("years_select", label = "Set Years for Graph",
                                          min = 1995,
                                          max = 2016,
                                          value = c(2000, 2016)),
                              # need to find a way to get rid of commas so it looks like years
                              selectizeInput("sel_parks", "Select Parks to Graph",
                                          choices = plot_parks,
                                          multiple = TRUE)
                              
                            ),
                            mainPanel(
                              plotlyOutput("visitors_line")
                              
                            )
                          )
                 ),
                 tabPanel("Single Park Visitors",
                          h1("Specified Visitors Per National Park"),
                          sidebarLayout(
                            sidebarPanel(
                              selectInput("choose_park", "Select a Park to View Their Visitors",
                                             choices = plot_parks,
                                             selected = "Acadia National Park")
                              ),
                            mainPanel(
                              plotlyOutput("visitors_bar")
                            )
                            )
                          )
                 )



server <- function(input, output, session) {
  
  output$map1 <- renderLeaflet({
    leaflet(nps) |>
      setView(lng = -110, lat = 39.833, zoom = 2.5) |>
      addTiles() |>
      addProviderTiles(providers$OpenStreetMap.Mapnik) |>
      ## too much for a shiny app 
      # addPolygons(color = "#09008a",
        #          weight = .5,
         #         smoothFactor = .5,
         #         opacity = 1.0, 
          #        label = nps$UNIT_NAME) |>
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
  
  observeEvent(input$datasets, {
    updateTabsetPanel(inputId = "params", selected = input$datasets)
  }) 
  
  
  sample <- reactive({
    switch(input$datasets,
           All_Parks = 
             if (length(input$Unit_types) == 0 & length(input$State) == 0) {
               All_Parks
             } 
           
           else if (length(input$Unit_types) == 0 & length(input$State) != 0) {
             All_Parks |> 
               filter(STATE %in% input$State)
           }
           
           else if (length(input$Unit_types) != 0 & length(input$State) == 0) {
             All_Parks |> 
               filter(UNIT_TYPE %in% input$Unit_types)
           } 
           else {
             All_Parks |> 
               filter(UNIT_TYPE %in% input$Unit_types &
                        STATE %in% input$State)
           },
           
           Just_National_parks = if(length(input$States) == 0){
             Just_National_parks
           } else {
             Just_National_parks |> filter(State %in% input$States)
           }
    )
  })
  
  
  output$table1 <- DT::renderDataTable({
    datatable(sample())
  })
  
  visit_reactive <- reactive({
    plot_data <- np_visitors |>
      filter(Year >= input$years_select[1]) |>
      filter(Year <= input$years_select[2]) |>
      filter(Name %in% input$sel_parks)
  })
  
  output$visitors_line <- renderPlotly({
    validate(
      need(input$sel_parks, "Please select a park(s)")
    )
    v_plot <- ggplot(data = visit_reactive(), aes(x = Year, y = Visitors, group = Name,
                                             label = Name,
                                             label2 = Year,
                                             label3 = Visitors)) +
      geom_line(aes(color = Name), show.legend = FALSE) +
      scale_color_viridis_d() +
      scale_y_continuous(labels = scales::comma) +
      labs(title = str_glue("Visitors in National Parks from {input$years_select[1]} to {input$years_select[2]}")) +
      theme(legend.position = "none")
    ggplotly(v_plot, tooltip = c("label", "label2", "label3"))
    
  })
  
  op_reactive <- reactive({
    op_data <- np_visitors |>
      filter(Name == input$choose_park)
    op_data
  })
  
  output$visitors_bar <- renderPlotly({
    ggplot(data = op_reactive(), aes(x = Year, y = Visitors)) +
      geom_col(colour = 'darkolivegreen', fill = 'darkolivegreen3',
               position = "dodge") +
      labs(title = str_glue("Vistitors in {input$choose_park} from 1995 to 2016"))
  })
  
}


shinyApp(ui = ui, server = server)
