#App for automatically translating reports

## TODO
# HAVE AN RDS FILE THAT WE WILL USE AS THE DEFAULT
# IT WILL HAVE A LIST OF LOADED FILES
# SELECTING DIRECTORY WILL LOAD NEW ONLY
# CAN SELECT INDIVIDUAL TO RELOAD
# HAVE A RELOAD ALL BUTTON



startCLAPplication <- function() {
  ui <- fluidPage(
    titlePanel("Choose Directory and Select File"),

    sidebarLayout(
      sidebarPanel(
        shinyDirButton("directory", "Choose Directory", "Select a folder"),
        verbatimTextOutput("dir_path"),
        selectInput("file_select", "Select a file:", choices = NULL)
      ),

      mainPanel(
        textOutput("selected_file")
      )
    )
  )

  server <- function(input, output, session) {
    volumes <- c(Home = fs::path_home(), "Root" = "/")
    shinyDirChoose(input, "directory", roots = volumes, session = session)
    dir_path <- reactive({
      req(input$directory)
      return(parseDirPath(volumes, input$directory))
    })

    output$dir_path <- renderText({
      req(dir_path())
      paste("Selected directory:", dir_path())
    })

    observe({
      path <- dir_path()
      if (!is.null(path)) {
        files <- list.files(path)
        updateSelectInput(session, "file_select", choices = files)
      }
    })

    output$selected_file <- renderText({
      req(input$file_select)
      paste("Selected file:", input$file_select)
    })
  }
  shinyApp(ui, server)
}



# Choose directory interactively (cross-platform)
# dir_path <- tk_choose.dir(default = getwd(), caption = "Select a directory")
#
# tsv_files <- list.files(dir_path, pattern = "\\.tsv$", full.names = TRUE)
# tsv_data <- lapply(tsv_files, read_tsv)
# names(tsv_data) <- basename(tsv_files)
# lapply(tsv_data, head)



