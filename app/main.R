# This file contains the main code for the CRISPY Shiny application. It defines the UI and server functions.

# Load required packages
box::use(
  shiny[moduleServer, NS, renderUI, tags, HTML, uiOutput, conditionalPanel, observe, observeEvent, div, a, reactiveVal, p, eventReactive],
  shiny.semantic[semanticPage],
  semantic.dashboard[dashboardPage, dashboardBody, dashboardSidebar, dashboardHeader, icon]
)

# Load required modules and logic files
box::use(
  # modules
  app / view / sidebar_parameters,
  app / view / homepage,
  app / view / crispy_equities,
  app / view / crispy_loans,
  # logic
  app / logic / constant[
    trisk_input_path,
    backend_trisk_run_folder,
    max_trisk_granularity,
    available_vars,
    hide_vars,
    use_ald_sector
  ]
)





# Define the UI function
#' @export
ui <- function(id) {
  ns <- NS(id)



  dashboardPage(
    title = "Homepage",
    # dashboardHeader
    dashboardHeader(title = "CRISPY"),
    # dashboardSidebar
    dashboardSidebar(
      tags$div(
        sidebar_parameters$ui(
          ns("sidebar_parameters"),
          max_trisk_granularity = max_trisk_granularity,
          available_vars = available_vars
        ),
        shiny::img(
          src = "static/logo_1in1000.png",
          height = "20%", width = "auto",
          style = "
            display: block;
            margin-left: auto;
            margin-right: auto;
            margin-top: 10px;
            margin-bottom: 10px;"
        )
      ),
      size = "wide",
      visible = FALSE
    ),
    # dashboardBody
    dashboardBody(
      shinyjs::useShinyjs(),
      # Include custom CSS to display tabs as full width
      tags$head(
        tags$style(HTML("
          .full-width-tabs {
            width: 100% !important;
            display: flex !important;
          }
          .full-width-tabs .item {
            flex: 1 !important;
            text-align: center !important;
          }
        "))
      ),
      # Fomantic UI tabs with custom CSS to display as full width
      tags$div(
        class = "ui top attached tabular menu full-width-tabs",
        tags$a(class = "item active", `data-tab` = "first", "Home"),
        tags$a(class = "item", `data-tab` = "second", "Equities"),
        tags$a(class = "item", `data-tab` = "third", "Loans")
      ),
      # dynamic tabs content, the `data-tab` attribute must match the `data-tab` attribute
      # of the corresponding tab in the tabular menu
      tags$div(
        class = "ui bottom attached active tab segment", `data-tab` = "first",
        div(
          class = "ui container",
          # homepage tab
          homepage$ui(
            ns("homepage")
          )
        )
      ),
      tags$div(
        class = "ui bottom attached tab segment", `data-tab` = "second",
        div(
          class = "ui container",
          # equities tab
          crispy_equities$ui(
            ns("crispy_equities"),
            max_trisk_granularity = max_trisk_granularity, # constant
            available_vars = available_vars # constant
          )
        )
      ),
      tags$div(
        class = "ui bottom attached tab segment", `data-tab` = "third",
        div(
          class = "ui container",
          # equities tab
          crispy_loans$ui(
            ns("crispy_loans"),
            max_trisk_granularity = max_trisk_granularity, # constant
            available_vars = available_vars # constant
          )
        )
      ),
      # this javascript snippet initializes the tabs menu and makes the tabs clickable
      tags$script(
        "$(document).ready(function() {
                // Initialize tabs (if not already initialized)
                $('.menu .item').tab();
            });"
      )
    )
  )
}

# Define the server function
#' @export
server <- function(id) {
  moduleServer(id, function(input, output, session) {
    # the TRISK runs are generated In the sidebar module
    perimeter <- sidebar_parameters$server(
      "sidebar_parameters",
      max_trisk_granularity = max_trisk_granularity, # constant
      trisk_input_path = trisk_input_path, # constant
      backend_trisk_run_folder = backend_trisk_run_folder, # constant
      available_vars = available_vars, # constant
      hide_vars = hide_vars, # constant
      use_ald_sector = use_ald_sector # constant
    )

    homepage$server("homepage")

    crispy_equities$server(
      "crispy_equities",
      trisk_input_path = trisk_input_path, # constant
      backend_trisk_run_folder = backend_trisk_run_folder, # constant
      max_trisk_granularity = max_trisk_granularity,
      perimeter = perimeter
    )

    crispy_loans$server(
      "crispy_loans"
      # ,
      # trisk_input_path = trisk_input_path, # constant
      # backend_trisk_run_folder = backend_trisk_run_folder, # constant
      # max_trisk_granularity = max_trisk_granularity,
      # perimeter = perimeter
    )
  })
}
