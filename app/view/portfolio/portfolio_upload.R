# Load required packages
box::use(
  shiny[moduleServer, NS, tags]
)
ui <- function(id) {
    ns = NS(id)

    shiny.semantic::semanticPage(
        shiny::tags$div(class = "ui middle aligned center aligned grid container",
        shiny::tags$div(class = "ui fluid segment",
            shiny::tags$input(type = "file", id = ns("fileUpload"), class = "inputfile", onchange = "fileEvent(event)"),
            shiny::tags$label(for = "fileUpload", class = "ui huge red right floated button",
            shiny::tags$i(class = "ui upload icon"),
            "Upload CSV"
            )
        )
        ),
        shiny::uiOutput("fileInfo") # Display information about the file
    )
}



server <- function(id) {
  moduleServer(id, function(input, output, session) {
    shiny::observeEvent(input$fileUpload, {
    uploadedFilePath <- input$fileUpload$datapath
    
    if (is.null(uploadedFilePath)) {
      output$fileInfo <- shiny::renderUI({
        shiny::tags$div("Please upload a file.")
      })
      return()
    }
    
    tryCatch({
      csvData <- utils::read.csv(uploadedFilePath, stringsAsFactors = FALSE)
      # Hardcoded character vector of possible column names
      possibleColumns <- c("Column1", "Column2", "Column3", "Column4")
      
      # Identifying missing columns
      missingColumns <- setdiff(possibleColumns, colnames(csvData))
      
      if (length(missingColumns) > 0) {
        missingColumnsString <- paste(missingColumns, collapse = ", ")
        output$fileInfo <- shiny::renderUI({
          shiny::tags$div(style = "color: red;", 
                          shiny::HTML(paste("The CSV is missing the following required columns: ", missingColumnsString, ".", sep="")))
        })
      } else {
        output$fileInfo <- shiny::renderUI({
          shiny::tags$div(style = "color: green;", "The CSV file is valid and contains all the required columns.")
        })
      }
    }, error = function(e) {
      output$fileInfo <- shiny::renderUI({
        shiny::tags$div(style = "color: red;", "Error reading the file. Please ensure it is a valid CSV.")
      })
    })
  })
})}