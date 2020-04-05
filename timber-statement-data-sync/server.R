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
library(tidyr)
library(leaflet)
library(ggplot2)
library(htmltools)

options(shiny.maxRequestSize = 1000 * 1024^2)

# function for volume calculation - use when QField forms are created with different attribute names
total_volume <- function(length, diameter, defect_depth, defect_length, defect_width) {
  tv <- (length * (diameter^2) * 0.0000785398162) - (defect_depth * defect_length * defect_width)
  return(tv)
}

# server
shinyServer(function(input, output, session) {

  # get layer names
  field_layers <- reactive({
    req(input$qfield_data$datapath)

    f_lyrs <- st_layers(input$qfield_data$datapath)
    f_lyrs <- f_lyrs$name
    f_lyrs
  })

  # get species class layer name
  species_class_lyr <- reactive({
    req(field_layers())

    species_class_lyr_idx <- str_detect(field_layers(), "species_class")
    species_class_lyr_name <- field_layers()[species_class_lyr_idx]
    species_class_lyr_name
  })

  # get logging records layer name
  logging_records_lyr <- reactive({
    req(field_layers())

    logging_records_lyr_idx <- str_detect(field_layers(), "logging_records")
    logging_records_lyr_name <- field_layers()[logging_records_lyr_idx]
    logging_records_lyr_name
  })

  # get logging area layer name
  logging_area_lyr <- reactive({
    req(field_layers())

    logging_area_lyr_idx <- str_detect(field_layers(), "logging_area")
    logging_area_lyr_name <- field_layers()[logging_area_lyr_idx]
    logging_area_lyr_name
  })

  # get species class layer table
  species_class_table <- reactive({
    req(species_class_lyr())
    req(input$qfield_data$datapath)

    sc_table <- st_read(input$qfield_data$datapath, layer = species_class_lyr(), stringsAsFactors = FALSE) %>%
      select(Species, Class)
    sc_table
  })

  # get logging records
  logging_records_table <- reactive({
    req(logging_records_lyr())
    req(input$qfield_data$datapath)

    logging_records <- st_read(input$qfield_data$datapath, layer = logging_records_lyr(), stringsAsFactors = FALSE)
    logging_records <- logging_records %>%
      left_join(species_class_table(), by = c("species" = "Species")) %>%
      mutate(
        total_volume = total_volume(length, diameter, defect_depth, defect_length, defect_width)
      ) %>%
      select(-fid, -row_id, -unique_id)
    logging_records
  })

  # royalties table
  royalty_table <- reactive({
    class <- c(1, 2, 3, 4)
    zone1 <- c(54.27, 44.63, 14.47, 8.44)
    zone2 <- c(54.27, 42.21, 14.47, 8.44)
    zone3 <- c(44.62, 32.56, 12.06, 8.44)
    royalties <- as.data.frame(cbind(class, zone1, zone2, zone3)) %>%
      pivot_longer(c(zone1, zone2, zone3), names_to = "zones", values_to = "royalty")
    royalties
  })

  # fees table
  fees_table <- reactive({
    class <- c(1, 2, 3, 4)
    fees1 <- c(5, 5, 5, 5)
    fees2 <- c(5, 5, 5, 5)
    fees3 <- c(5, 5, 5, 5)
    fees <- as.data.frame(cbind(class, fees1, fees2, fees3)) %>%
      rename(
        zone1 = fees1,
        zone2 = fees2,
        zone3 = fees3
      ) %>%
      pivot_longer(c(zone1, zone2, zone3), names_to = "zones", values_to = "fees")
    fees
  })

  # get royalties and fees by zone where data was collected
  royalties_tmp <- reactive({
    req(royalty_table())

    r <- royalty_table() %>%
      filter(zones == input$f_zones)
    r
  })

  fees_tmp <- reactive({
    req(fees_table())

    f <- fees_table() %>%
      filter(zones == input$f_zones)
    f
  })

  # summarise logging records - compute total volume, royalties, and fees per class and per site
  logging_records_summarised <- reactive({
    req(logging_records_table())
    req(royalties_tmp())
    req(fees_tmp())

    logging_records_summary <- logging_records_table() %>%
      group_by(site_id, Class) %>%
      summarise(total_volume = sum(total_volume, na.rm = TRUE)) %>%
      ungroup() %>%
      left_join(royalties_tmp(), by = c("Class" = "class")) %>%
      left_join(fees_tmp(), by = c("Class" = "class", "zones" = "zones")) %>%
      mutate(
        royalty = total_volume * royalty,
        fees = total_volume * fees
      )
    logging_records_summary
  })

  # get logging area - spatial data of area logging records collected and site attributes
  logging_area <- reactive({
    req(input$qfield_data$datapath)
    req(logging_area_lyr())

    lar <- st_read(input$qfield_data$datapath, layer = logging_area_lyr(), stringsAsFactors = FALSE)
    lar
  })

  # join logging records to logging area
  logging_area_summary <- reactive({
    req(logging_area())
    req(logging_records_summarised())

    lar_join <- logging_area() %>%
      left_join(logging_records_summarised(), by = c("id" = "site_id"))
    lar_join
  })

  # Data table to logging records per class and site
  output$logging_records_output <- DT::renderDataTable(DT::datatable({
    req(logging_records_summarised())

    logging_records_summarised()
  }))

  # Data table to visualise data to visualise timber statement per site
  output$logging_area_output <- DT::renderDataTable(DT::datatable({
    req(logging_area_summary())

    logging_area_summary_output <- logging_area_summary() %>%
      select(-fid, -id, -unique_id)
    logging_area_summary_output
  }))

  # Date stamp for downloading files
  dt <- reactive({
    d <- Sys.time()
    d <- str_replace_all(d, ":", "-")
    d <- str_replace(d, " ", "-")
    d
  })

  # Downloadable csv of selected dataset ----
  # Timeber statement
  output$download_data_timber_statement <- downloadHandler(
    filename = function() {
      paste("timeber_statement_", dt(), ".csv", sep = "")
    },
    content = function(file) {
      req(logging_area_summary())

      logging_area_summary_output <- logging_area_summary() %>%
        select(-fid, -unique_id)
      write.csv(logging_area_summary_output, file, row.names = FALSE)
    }
  )

  # Individual logging records
  output$download_data_logging_records <- downloadHandler(
    filename = function() {
      paste("logging_records_", dt(), ".csv", sep = "")
    },
    content = function(file) {
      req(logging_records_summarised())

      write.csv(logging_records_summarised(), file, row.names = FALSE)
    }
  )

  # Summarise data for visulaisation
  data_by_class <- reactive({
    req(logging_records_summarised())

    class_vol <- logging_records_summarised() %>%
      select(total_volume, royalty, fees, Class) %>%
      group_by(Class) %>%
      summarise(
        total_volume = sum(total_volume, na.rm = TRUE),
        royalty = sum(royalty, na.rm = TRUE),
        fees = sum(fees, na.rm = TRUE)
      )
  })

  output$total_volume_plot <- renderPlot({
    req(data_by_class())

    tv_plot <- ggplot(data_by_class(), aes(as.factor(Class), total_volume)) +
      geom_bar(stat = "identity", width = 0.5, color = "black") +
      labs(title = "Total Volume", y = "", x = "class") +
      theme_bw()
    tv_plot
  })

  output$royalty_plot <- renderPlot({
    req(data_by_class())

    r_plot <- ggplot(data_by_class(), aes(as.factor(Class), royalty)) +
      geom_bar(stat = "identity", width = 0.5, color = "black") +
      labs(title = "Royalty ($ FJ)", y = "", x = "class") +
      theme_bw()
    r_plot
  })

  output$fees_plot <- renderPlot({
    req(data_by_class())

    f_plot <- ggplot(data_by_class(), aes(as.factor(Class), fees)) +
      geom_bar(stat = "identity", width = 0.5, color = "black") +
      labs(title = "Fees ($ FJ)", y = "", x = "class") +
      theme_bw()
    f_plot
  })

  data_by_site <- reactive({
    class_vol <- logging_records_summarised() %>%
      select(total_volume, royalty, fees, site_id) %>%
      group_by(site_id) %>%
      summarise(
        total_volume = sum(total_volume, na.rm = TRUE),
        royalty = sum(royalty, na.rm = TRUE),
        fees = sum(fees, na.rm = TRUE)
      )
  })

  # Create polygons of logged area to display on map and join total_volume, royalties, and fees per site
  map_df <- reactive({
    req(logging_area())

    map_df <- logging_area() %>%
      left_join(data_by_site(), by = c("id" = "site_id")) %>%
      st_transform(crs = 4326)
    map_df
  })

  # Data visualisation outputs
  output$map <- renderLeaflet({
    leaflet() %>%
      addTiles(group = "OSM (default)") %>%
      addProviderTiles(providers$OpenTopoMap, group = "OSM (Topo Map)") %>%
      setView(lng = 178.4501, lat = -18.1248, zoom = 13) %>%
      addLayersControl(
        baseGroups = c("OSM (default)", "OSM (Topo Map)"),
        options = layersControlOptions(collapsed = FALSE)
      )
  })

  # observe data input to add polygons to leaflet map
  observe({
    req(map_df())
    req(input$navbar == "Map")

    map_display <- map_df()

    bbox <- st_bbox(map_display) %>%
      as.vector()

    leafletProxy("map") %>%
      addPolygons(
        data = map_display,
        weight = 0.75,
        opacity = 1.0,
        fillOpacity = 0.5,
        highlightOptions = highlightOptions(
          color = "white", weight = 2,
          bringToFront = TRUE
        ),
        popup = paste(
          "Total Volume: ", signif(map_display$total_volume, digits = 5), "<br>",
          "Royalty ($ FJ): ", signif(map_display$royalty, digits = 5), "<br>",
          "Fees ($ FJ): ", signif(map_display$fees, digits = 5), "<br>"
        )
      ) %>%
      fitBounds(bbox[1], bbox[2], bbox[3], bbox[4])
  })
})
