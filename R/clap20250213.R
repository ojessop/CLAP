```
library(shiny)
library(bslib)
library(tcltk)
library(readr)
library(DT)
library(dplyr)

selectData <- function() {
  dir_path <- tk_choose.dir(default = getwd(), caption = "Select a directory")
  tsv_files <- list.files(dir_path, pattern = "\\.tsv$", full.names = TRUE)
  
  tsv_data <- lapply(tsv_files, function(file) {
    df <- read_tsv(file, col_types = cols())
    df$file <- basename(file)
    return(df)
  })
  combined_data <- bind_rows(tsv_data)
  return(combined_data)
}

renderClapTable <- function(selectedFile, selectedColumns, allData) {
  DT::renderDataTable({
    DT::datatable(allData$data[which(allData$data$file %in% selectedFile), selectedColumns],
                  options = list(
                    lengthMenu = c(5, 10, 25, 50, 100, length(which(allData$data$file %in% selectedFile))),
                    pageLength = 10,
                    columnDefs = list(
                      list(targets = "_all", className = "dt-nowrap")
                    )))
  })
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
  card(
    max_height = 100,
    "Selected Files",
    textOutput("tsv_head")
  ),
  card(
    card_header("Data"),
    DT::dataTableOutput("tsvDatTable")
  )
)

server <- function(input, output, session) {
  allData <- reactiveValues(data = NULL)
  
  observeEvent(input$chooseDir, {
    allData$data <- selectData()
    updateCheckboxGroupInput(session, "vis_datasets", choices = unique(allData$data$file), selected = unique(allData$data$file))
    updateCheckboxGroupInput(session, "shownDat", choices = names(allData$data), selected = names(allData$data))
  })
  observeEvent(input$vis_datasets, {
    selectedFile <- input$vis_datasets
    selectedColumns <- input$shownDat
    output$tsv_head <- renderText({paste(selectedFile, collapse=", ")})
    output$tsvDatTable <- renderClapTable(selectedFile, selectedColumns, allData)
  }, ignoreNULL = FALSE)
  observeEvent(input$shownDat, {
    selectedFile <- input$vis_datasets
    selectedColumns <- input$shownDat
    output$tsvDatTable <- renderClapTable(selectedFile, selectedColumns, allData)
  }, ignoreNULL = FALSE)
}

shinyApp(ui = ui, server = server)
```