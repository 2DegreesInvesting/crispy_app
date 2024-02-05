# This file contains the main code for the CRISPY Shiny application. It defines the UI and server functions.

# Load required packages
box::use(
  shiny[moduleServer, NS, renderUI, tags, uiOutput, observe, observeEvent, div, a, reactiveVal, p, eventReactive],
  shiny.semantic[semanticPage],
  semantic.dashboard[dashboardPage, dashboardBody, dashboardSidebar, dashboardHeader]
)

# Load required modules and logic files
box::use(
  # modules
  app / view / sidebar_parameters,
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



  dashboardPage(
    title = "Homepage",
    # dashboardHeader
    dashboardHeader(title = "CRISPY"),
    # dashboardSidebar
    dashboardSidebar(
    sidebar_parameters$ui(
      ns("sidebar_parameters"),
      max_trisk_granularity = max_trisk_granularity,
      available_vars = available_vars
    ),
    size = "wide"
  ),
    # dashboardBody
    dashboardBody(
      tags$div(
        class = "ui top attached tabular menu",
        tags$a(class = "item active", `data-tab` = "first", "Home", tags$i(class="close icon")),
        tags$a(class = "item", `data-tab` = "second", "Equities"),
        tags$a(class = "item", `data-tab` = "third", "Loans")
      ),
      tags$div(
        class = "ui bottom attached active tab segment", `data-tab` = "first",
        div(
          class = "ui container",
          homepage$ui(
            ns("homepage")
          )
        )
      ),
      tags$div(
        class = "ui bottom attached tab segment", `data-tab` = "second",
        div(
          class = "ui container",
          crispy_equities$ui(
            ns("crispy_equities"),
            max_trisk_granularity = max_trisk_granularity, # constant
            available_vars = available_vars # constant
          )
        )
      ),
              tags$script(
            "$(document).ready(function() {
                // Initialize tabs (if not already initialized)
                $('.menu .item').tab();

                // Handle click event on close icon
                $('.menu .item .close.icon').on('click', function() {
                    // Prevents the tab from opening while closing
                    event.stopPropagation();

                    // Get the data-tab attribute value
                    var tabName = $(this).closest('.item').attr('data-tab');

                    // Remove the tab item
                    $(this).closest('.item').remove();

                    // Remove the tab content
                    $('.tab.segment[data-tab=\"' + tabName + '\"]').remove();

                    // If you want to open another tab after closing, you can do so here
                });
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
      max_trisk_granularity = max_trisk_granularity,
      perimeter = perimeter
    )
  })
}
