# This file contains the main code for the CRISPY Shiny application. It defines the UI and server functions.

# Load required packages
box::use(
  shiny[moduleServer, NS, renderUI, tags, uiOutput, observe, observeEvent, div, a, reactiveVal, p, eventReactive],
  shiny.semantic[semanticPage, segment, slider_input, card],
  semantic.dashboard[dashboardPage, dashboardHeader, dashboardSidebar, dashboardBody, icon, box],
)

# Load required modules and logic files
box::use(
  # modules
  app/view/sidebar_parameters,
  app/view/crispy_equities,
  app/view/menus[dashboard_header_crispy],
  #logic
  app/logic/constant[
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
  semanticPage(
  dashboardPage(
    title = "CRISPY",
    dashboard_header_crispy(page_select="Home"),
    dashboardSidebar(
      div(
        sidebar_parameters$ui(ns("sidebar_parameters"), max_trisk_granularity, available_vars),
        shiny::img(
          src = "static/logo_1in1000.png",
          height = "20%", width = "auto",
          style = "
            display: block;
            margin-left: auto;
            margin-right: auto;
            margin-top: 10px;
            margin-bottom: 10px;"
        ))
      ,
      size = "wide"
    ),
    dashboardBody(
      # First row with the left (1/3 width) the right (2/3 width)
      semanticPage(
        shiny.router::router_ui(
          shiny.router::route("/", semanticPage() ), # Default route
          shiny.router::route("crispy_equities", crispy_equities$ui(ns("crispy_equities"))) 
        )
      )
    )
  ))
}

# Define the server function
#' @export
server <- function(id) {
  moduleServer(id, function(input, output, session) {
      shiny.router::router_server()
    # SELECT PARAMETERS =========================

    # the TRISK runs are generated In the sidebar module
    perimeter <- sidebar_parameters$server(
      "sidebar_parameters",
      max_trisk_granularity=max_trisk_granularity,
      trisk_input_path = trisk_input_path,
      backend_trisk_run_folder = backend_trisk_run_folder,
      available_vars=available_vars,
      hide_vars=hide_vars,
      use_ald_sector=use_ald_sector
      )

    trisk_granularity_r <- perimeter$trisk_granularity_r
    crispy_data_r <- perimeter$trisk_outputs$crispy_data_r
    trajectories_data_r <- perimeter$trisk_outputs$trajectories_data_r


    # CONSUME TRISK OUTPUTS =========================

    # Crispy Equities module
    # Manages the porfolio creator module
    # Create analysis data by merging crispy to portfolio, and aggrgating to the appropriate granularity
    # Create equity change plots
    crispy_equities$server(
      "crispy_equities", 
      crispy_data_r=crispy_data_r, 
      trajectories_data_r=trajectories_data_r, 
      trisk_granularity_r=trisk_granularity_r,
      max_trisk_granularity=max_trisk_granularity
    )
    
  })
}
