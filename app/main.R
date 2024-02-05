# This file contains the main code for the CRISPY Shiny application. It defines the UI and server functions.

# Load required packages
box::use(
  shiny[moduleServer, NS, renderUI, tags, uiOutput, observe, observeEvent, div, a, reactiveVal, p, eventReactive],
  shiny.semantic[semanticPage],
  shiny.router[router_ui, router_server, route]
)

# Load required modules and logic files
box::use(
  # modules
  app / view / homepage,
  app / view / crispy_equities,
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
  
    router_ui(
      route(
        path = "/",
        ui = homepage$ui(
          ns("homepage")
        )
      ), # Default route
      route(
        path = "crispy_equities",
        ui = crispy_equities$ui(
          ns("crispy_equities"),
          max_trisk_granularity = max_trisk_granularity, # constant
          available_vars = available_vars # constant
        )
      )
    )
  
}

# Define the server function
#' @export
server <- function(id) {
  moduleServer(id, function(input, output, session) {
    router_server(root_page = "/")

    homepage$server("homepage")

    crispy_equities$server(
      "crispy_equities",
      max_trisk_granularity = max_trisk_granularity,
      trisk_input_path = trisk_input_path,
      backend_trisk_run_folder = backend_trisk_run_folder,
      available_vars = available_vars,
      hide_vars = hide_vars,
      use_ald_sector = use_ald_sector
    )
  })
}
