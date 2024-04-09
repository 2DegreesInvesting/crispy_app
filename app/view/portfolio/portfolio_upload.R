# Load required packages
box::use(
  shiny[moduleServer, NS, tags]
)
ui <- function(id) {
  ns <- shiny::NS(id)

  shiny::tags$div(
    class = "ui container", style = "padding-top: 50px;",
    shiny::tags$div(
      class = "ui segment",
      shiny::tags$input(type = "file", id = ns("fileUpload"), style = "display: none;", onchange = "shinyjs.fileChangeEvent"),
      shiny::tags$label(
        `for` = ns("fileUpload"), class = "ui button",
        shiny::tags$i(class = "ui upload icon"),
        "Upload File"
      ),
      shiny::tags$span(id = ns("fileName"), style = "padding-left: 10px;")
    )
  )
}



server <- function(id) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

    shiny::observeEvent(input$fileUpload, {
      browser()
      uploadedFilePath <- input$fileUpload$datapath

      if (is.null(uploadedFilePath)) {
        output$fileName <- shiny::renderUI({
          shiny::HTML("Please upload a file.")
        })
        return()
      }

      tryCatch(
        {
          csvData <- utils::read.csv(uploadedFilePath, stringsAsFactors = FALSE)
          possibleColumns <- c("Column1", "Column2", "Column3", "Column4")
          missingColumns <- setdiff(possibleColumns, colnames(csvData))

          if (length(missingColumns) > 0) {
            missingColumnsString <- paste(missingColumns, collapse = ", ")
            output$fileName <- shiny::renderUI({
              shiny::HTML(paste("The CSV is missing the following required columns:", missingColumnsString))
            })
          } else {
            output$fileName <- shiny::renderUI({
              shiny::HTML("The CSV file is valid and contains all the required columns.")
            })
          }
        },
        error = function(e) {
          output$fileName <- shiny::renderUI({
            shiny::HTML("Error reading the file. Please ensure it is a valid CSV.")
          })
        }
      )
    })
  })
}
