library(shiny)
library(bslib)
library(tcltk)
library(readr)
library(DT)
library(dplyr)
library(oz)
library(sf)
library(ggplot2)

selectData <- function() {
  dir_path <- tk_choose.dir(default = getwd(), caption = "Select a directory")
  tsv_files <- list.files(dir_path, pattern = "\\.tsv$", full.names = TRUE)
  metadata_files <- tsv_files[which(TRUE == grepl("metadata", tsv_files))]
  tsv_files <- tsv_files[which(FALSE == grepl("metadata", tsv_files))]
  tsv_data <- lapply(tsv_files, function(file) {
    df <- read_tsv(file, col_types = cols())
    df[["ST File"]] <- basename(file)
    return(df)
  })
  combined_data <- bind_rows(tsv_data)
  metadata <- lapply(metadata_files, function(file) {
    df <- read_tsv(file, col_types = cols(Gender = col_character())) # Stop from outputting false
    return(df)
  })
  combined_metadata <- bind_rows(metadata)
  all_data <- merge(combined_data, combined_metadata, by = "Sample", all = TRUE)

  return(all_data)
}

renderClapTable <- function(selectedFile, selectedColumns, allData) {
  DT::renderDataTable({
    DT::datatable(allData$data[which(allData$data[["ST File"]] %in% selectedFile), selectedColumns],
                  options = list(
                    lengthMenu = c(5, 10, 25, 50, 100, length(which(allData$data[["ST File"]] %in% selectedFile))),
                    pageLength = 10,
                    columnDefs = list(
                      list(targets = "_all", className = "dt-nowrap")
                    )))
  })
}

renderHistograms <- function() {
  # Define coordinates for Brisbane and the Gold Coast
  brisbane <- c(153.0251, -27.4698)
  gold_coast <- c(153.4000, -28.0167)

  # Offset positions for the histograms
  brisbane_hist <- c(brisbane[1] + 1.5, brisbane[2] + 1.5)
  gold_coast_hist <- c(gold_coast[1] + 1.5, gold_coast[2] - 1.5)

  # Add points for Brisbane and Gold Coast
  points(brisbane[1], brisbane[2], col = "red", pch = 19, cex = 1.5)
  points(gold_coast[1], gold_coast[2], col = "blue", pch = 19, cex = 1.5)

  # Draw connecting lines
  segments(brisbane[1], brisbane[2], brisbane_hist[1], brisbane_hist[2], col = "red", lwd = 2)
  segments(gold_coast[1], gold_coast[2], gold_coast_hist[1], gold_coast_hist[2], col = "blue", lwd = 2)

  # Generate data for histograms
  brisbane_data <- rnorm(100, mean = 10, sd = 3)
  gold_coast_data <- rnorm(100, mean = 7, sd = 2)

  # Function to overlay histogram at an offset location
  plot_hist_on_map <- function(x, y, data, col) {
    hist_data <- hist(data, plot = FALSE)
    rects_x <- seq(x - 1, x + 1, length.out = length(hist_data$counts) + 1)
    rects_y <- y + hist_data$counts / max(hist_data$counts) * 2 # Scale height
    for (i in seq_along(hist_data$counts)) {
      rect(rects_x[i], y, rects_x[i+1], rects_y[i], col = col, border = "black")
    }
  }
  # Overlay histograms at the offset positions
  plot_hist_on_map(brisbane_hist[1], brisbane_hist[2], brisbane_data, col = rgb(1, 0, 0, 0.5))
  plot_hist_on_map(gold_coast_hist[1], gold_coast_hist[2], gold_coast_data, col = rgb(0, 0, 1, 0.5))
}


renderMap <- function() {
  hhs_shapefile <- "./Hospital_and_Health_Service_boundaries.shp"
  qld_hhs <- st_read(hhs_shapefile)

  ggplot(data = qld_hhs) +
    geom_sf(aes(fill = hhs), show.legend = FALSE) +
    geom_sf_text(aes(label = hhs), size = 5)
  renderHistograms()
}

ui <- page_sidebar(
  # title = "This is a title",

  sidebar = sidebar(
    "Menu",
    card(
      actionButton("chooseDir", label = "Choose dir.")
    ),
    card(
      checkboxGroupInput("vis_datasets", "Visible Datasets:", choices = NULL)
    ),
    card(
      checkboxGroupInput("shownDat", "Columns to show:", choices = NULL)
    )
  ),
  # card(
  #   max_height = 100,
  #   "Selected Files",
  #   textOutput("tsv_head")
  # ),
  card(
    tabsetPanel(
      tabPanel("Data Table", DT::dataTableOutput("tsvDatTable")),
      tabPanel("Maps", plotOutput(outputId = "map", height = "800px"))
    ),
  )
)

server <- function(input, output, session) {
  allData <- reactiveValues(data = NULL)



  observeEvent(input$chooseDir, {
    allData$data <- selectData()
    updateCheckboxGroupInput(session, "vis_datasets", choices = unique(allData$data[["ST File"]]), selected = unique(allData$data[["ST File"]]))
    updateCheckboxGroupInput(session, "shownDat", choices = names(allData$data), selected = names(allData$data))
  })
  observeEvent(input$vis_datasets, {
    selectedFile <- input$vis_datasets
    selectedColumns <- input$shownDat
    output$tsv_head <- renderText({paste(selectedFile, collapse=", ")})
    output$tsvDatTable <- renderClapTable(selectedFile, selectedColumns, allData)
    output$map <- renderPlot({renderMap()})
  }, ignoreNULL = FALSE)
  observeEvent(input$shownDat, {
    selectedFile <- input$vis_datasets
    selectedColumns <- input$shownDat
    output$tsvDatTable <- renderClapTable(selectedFile, selectedColumns, allData)
  }, ignoreNULL = FALSE)
}

shinyApp(ui = ui, server = server)
