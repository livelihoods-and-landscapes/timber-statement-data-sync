# Title: QConut Data Sync
# Purpose: Application to compute logged timber volume and convert .gpkg tables edited using QField to csv
# Platform: web - shinyapps.io
# Author: John Duncan
# Date: 31-03-2020

library(shiny)
library(sf)
library(dplyr)
library(stringr)
library(shinythemes)
library(shinyFiles)
library(fs)
library(leaflet)
library(RColorBrewer)
library(ggplot2)
library(htmltools)

options(shiny.maxRequestSize = 1000 * 1024^2)

# user interface
ui <- navbarPage(
  theme = shinytheme("flatly"),
  "QConut: Fiji Timber Statement",
  collapsible = TRUE,
  id = "navbar",

  tabPanel(
    "Data",

    # Sidebar layout with input and output definitions ----
    sidebarLayout(

      # Sidebar panel for inputs ----
      sidebarPanel(

        # Input: Select a file ----
        fileInput("qfield_data", "Select data.gpkg file from QField project",
          multiple = FALSE,
          accept = c(".gpkg")
        ),

        hr(),

        # Input: zone for royalties and fees
        tags$p("Select zone where date was collected:"),
        tags$p("zone 1: Viti Levu"),
        tags$p("zone 2: Vanua Levu"),
        tags$p("zone 3: All other islands"),

        selectInput("f_zones",
          "zone:",
          choices = c(
            "zone 1" = "zone1",
            "zone 2" = "zone2",
            "zone 3" = "zone3"
          )
        ),

        hr(),

        downloadButton("download_data_timber_statement", "Download Timber Statement"),

        hr(),

        downloadButton("download_data_logging_records", "Download Logging Records"),
      ),

      # Show a plot of the generated distribution
      mainPanel(tabsetPanel(
        type = "tabs",

        tabPanel("Timber Statement", div(style = "overflow-x:scroll; overflow-y:scroll", DT::dataTableOutput("logging_area_output"))),

        tabPanel("Logging Records", div(style = "overflow-x:scroll; overflow-y:scroll", DT::dataTableOutput("logging_records_output")))
      ))
    )
  ),

  tabPanel("Map",
    style = "position: fixed; top: 50px; left: 0; right: 0; bottom: 0; padding: 0; overflow: hidden;",
    leafletOutput("map", width = "100%", height = "100%"),

    div(
      style = "opacity: 0.75;",
      absolutePanel(plotOutput("total_volume_plot", height = "130px", width = "100%"),
        plotOutput("royalty_plot", height = "130px", width = "100%"),
        plotOutput("fees_plot", height = "130px", width = "100%"),
        top = NULL, left = NULL, right = "25px", bottom = "25px",
        width = "20%", height = "auto", draggable = TRUE, fixed = TRUE
      )
    )
  )
)
