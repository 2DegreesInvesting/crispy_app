# Load required packages
box::use(
  shiny[moduleServer, NS, tags]
)

ui <- function(id) {
  ns <- NS(id)
  shiny::tagList(
    shiny::tags$head(
      shiny::tags$style(shiny::HTML(paste0("
        /* Style for the label acting as the button */
        label[for='" , ns("fileUpload") , "'] {
          display: block; /* Makes the label fill the width of its container */
          width: 100%; /* Ensures the label (button) takes full width */
          text-align: center; /* Centers the text inside the button */
          padding: 10px 0; /* Adds some padding inside the button for better visual appearance */
          cursor: pointer; /* Changes the cursor to indicate it's clickable */
          background-color: #d4d4d5; /* Default button color */
          color: white; /* Text color */
          border: none;
          border-radius: 4px;
          margin: 8px 0; /* Margin around the button for spacing */
        }
        
        /* Hover style for the label acting as the button */
        label[for='" , ns("fileUpload") , "']:hover {
          background-color: #cacbcd; /* Slightly darker shade on hover */
        }

        /* Style adjustments for the file name display */
        span#" , ns("fileName") , " {
          display: block; /* Ensures the file name takes its own line */
          margin-top: 10px; /* Adds space above the file name for clarity */
        }
      ")))
    ),
    tags$input(type = "file", id = ns("fileUpload"), style = "display: none;", onchange = "shinyjs.fileChangeEvent"),
    tags$label(
      `for` = ns("fileUpload"), class = "ui button",
      tags$i(class = "ui upload icon"),
      "Upload File"
    ),
    tags$span(id = ns("fileName"), style = "padding-left: 10px;")
  )
}



server <- function(id) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns
    portfolio_uploaded_r <- shiny::reactiveVal(tibble::tibble())

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
    return(portfolio_uploaded_r)
  })
}


