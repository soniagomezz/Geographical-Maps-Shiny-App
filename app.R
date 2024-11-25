# Load required libraries
library(shiny)
library(ggplot2)
library(sf)
library(dplyr)
library(readr)
library(readxl)
library(gridExtra)  # For arranging the map and legend

# Define the Shiny app
ui <- fluidPage(
  titlePanel("Automatic Geographic Data Mapping"),
  sidebarLayout(
    sidebarPanel(
      fileInput("file", "Upload a File (CSV, TXT, or Excel):", 
                accept = c(".csv", ".txt", ".xlsx")),
      uiOutput("geo_column_select"),
      textInput("map_title", "Enter a Title for the Map:", "Geographic Map"),
      actionButton("generate_map", "Generate Map"),
      downloadButton("download_map", "Download Map")
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("County Map", plotOutput("county_map")),
        tabPanel("State Map", plotOutput("state_map")),
        tabPanel("Country Map", plotOutput("country_map")),
        tabPanel("Map Legend", plotOutput("map_legend"))
      )
    )
  )
)

server <- function(input, output, session) {
  
  # Reactive to load and inspect the uploaded file
  uploaded_data <- reactive({
    req(input$file)
    ext <- tools::file_ext(input$file$name)
    if (ext == "csv") {
      read_csv(input$file$datapath)
    } else if (ext == "txt") {
      read_delim(input$file$datapath, delim = "\t")
    } else if (ext == "xlsx") {
      read_excel(input$file$datapath)
    } else {
      stop("Unsupported file type.")
    }
  })
  
  # Dynamically detect geographic columns
  geo_columns <- reactive({
    data <- uploaded_data()
    cols <- colnames(data)
    geo_keywords <- c("county", "state", "country")
    detected_columns <- cols[grepl(paste(geo_keywords, collapse = "|"), cols, ignore.case = TRUE)]
    detected_columns
  })
  
  # Create dropdown for selecting geographic columns
  output$geo_column_select <- renderUI({
    req(geo_columns())
    selectInput("geo_column", "Select a Geographic Column:",
                choices = geo_columns())
  })
  
  # Load geographic shapefiles (replace with actual shapefiles or sources)
  county_shapes <- reactive({
    st_read(system.file("shape/nc.shp", package = "sf"))
  })
  state_shapes <- reactive({
    st_read(system.file("shape/nc.shp", package = "sf"))
  })
  country_shapes <- reactive({
    st_read(system.file("shape/nc.shp", package = "sf"))
  })
  
  # Generate County Map
  county_map <- reactive({
    req(input$geo_column)
    req(geo_columns())
    shapes <- county_shapes()
    data <- uploaded_data()
    selected_geo <- input$geo_column
    if (selected_geo == "county") {
      ggplot(data = shapes) +
        geom_sf(aes(fill = NAME), color = "black") +  # Replace NAME with shapefile's name column
        labs(
          fill = "County Name",
          title = input$map_title
        ) +
        theme_minimal() +
        theme(legend.position = "none")  # Hide the legend from the map itself
    }
  })
  
  output$county_map <- renderPlot({
    county_map()
  })
  
  # Generate State Map
  state_map <- reactive({
    req(input$geo_column)
    req(geo_columns())
    shapes <- state_shapes()
    data <- uploaded_data()
    selected_geo <- input$geo_column
    if (selected_geo == "state") {
      ggplot(data = shapes) +
        geom_sf(aes(fill = NAME), color = "black") +  # Replace NAME with shapefile's name column
        labs(
          fill = "State Name",
          title = input$map_title
        ) +
        theme_minimal() +
        theme(legend.position = "none")  # Hide the legend from the map itself
    }
  })
  
  output$state_map <- renderPlot({
    state_map()
  })
  
  # Generate Country Map
  country_map <- reactive({
    req(input$geo_column)
    req(geo_columns())
    shapes <- country_shapes()
    data <- uploaded_data()
    selected_geo <- input$geo_column
    if (selected_geo == "country") {
      ggplot(data = shapes) +
        geom_sf(aes(fill = NAME), color = "black") +  # Replace NAME with shapefile's name column
        labs(
          fill = "Country Name",
          title = input$map_title
        ) +
        theme_minimal() +
        theme(legend.position = "none")  # Hide the legend from the map itself
    }
  })
  
  output$country_map <- renderPlot({
    country_map()
  })
  
  # Extract legend from the map (for separate tab)
  extract_legend <- function(map) {
    legend_plot <- ggplotGrob(map + theme(legend.position = "bottom"))
    grid::grid.draw(legend_plot)
  }
  
  # Display the legend in the "Map Legend" tab
  output$map_legend <- renderPlot({
    req(input$geo_column)
    if (input$geo_column == "county") {
      extract_legend(county_map())
    } else if (input$geo_column == "state") {
      extract_legend(state_map())
    } else if (input$geo_column == "country") {
      extract_legend(country_map())
    }
  })
  
  # Download map as PNG
  output$download_map <- downloadHandler(
    filename = function() {
      paste(input$map_title, "map.png", sep = "_")
    },
    content = function(file) {
      ggsave(file, plot = switch(
        input$geo_column,
        "county" = county_map(),
        "state" = state_map(),
        "country" = country_map()
      ), device = "png")
    }
  )
}

# Run the Shiny app
shinyApp(ui, server)
