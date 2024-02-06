# This file contains the main code for the CRISPY Shiny application. It defines the UI and server functions.

# Load required packages
box::use(
  shiny[moduleServer, NS, tags, HTML, reactiveVal, observeEvent],
  shiny.semantic[semanticPage, segment, slider_input, card],
  semantic.dashboard[dashboardPage, dashboardBody, icon, box]
)

box::use(
  app / view / modules / trisk_mgmt,
  app / view / modules / portfolio_analysis,
  app / view / modules / equity_change_plots,
  app / view / modules / trajectories_plots,
)

####### UI

ui <- function(id, max_trisk_granularity, available_vars) {
  ns <- NS(id)

  # dashboardBody
  shiny::div(
    class = "pusher container", style = "min-height: 100vh;",
    shiny::div(
      class = "ui segment", style = "min-height: 100vh;",
      shiny::tags$div(
        class = "ui stackable grid",
        trisk_mgmt$ui(ns("trisk_mgmt")),
        portfolio_analysis$ui(ns("portfolio_analysis"), title = "Equities portfolio"),
        equity_change_plots$ui(ns("equity_change_plots")),
        trajectories_plots$ui(ns("trajectories_plots"))
      )
    )
  )
}

####### Server

server <- function(id, perimeter, backend_trisk_run_folder, trisk_input_path, max_trisk_granularity) {
  moduleServer(id, function(input, output, session) {
    # SELECT PARAMETERS =========================
    trisk_granularity_r <- perimeter$trisk_granularity_r
    trisk_run_params_r <- perimeter$trisk_run_params_r

    display_columns_equities <- c(
      names(max_trisk_granularity),
      "exposure_value_usd",
      "crispy_perc_value_change",
      "crispy_value_loss"
    )

    editable_columns_names_equities <- c("exposure_value_usd")

    colored_columns_names_equities <- c("crispy_perc_value_change", "crispy_value_loss")

    results <- trisk_mgmt$server(
      "trisk_mgmt",
      crispy_data_r = crispy_data_r,
      trisk_granularity_r = trisk_granularity_r,
      trisk_run_params_r = trisk_run_params_r,
      backend_trisk_run_folder = backend_trisk_run_folder,
      trisk_input_path = trisk_input_path,
      max_trisk_granularity = max_trisk_granularity
    )

    crispy_data_r <- results$crispy_data_r
    trajectories_data_r <- results$trajectories_data_r

    # MANAGE PORTFOLIO =========================

    # Manages the porfolio creator module
    # Create analysis data by merging crispy to portfolio, and aggrgating to the appropriate granularity
    analysis_data_r <- portfolio_analysis$server(
      "portfolio_analysis",
      crispy_data_r = crispy_data_r,
      trisk_granularity_r = trisk_granularity_r,
      max_trisk_granularity = max_trisk_granularity,
      portfolio_asset_type = "equity",
      display_columns = display_columns_equities,
      editable_columns_names = editable_columns_names_equities,
      colored_columns_names = colored_columns_names_equities
    )

    # CONSUME TRISK OUTPUTS =========================

    # Generate equity change plots
    equity_change_plots$server(
      "equity_change_plots",
      analysis_data_r = analysis_data_r,
      max_trisk_granularity = max_trisk_granularity
    )

    # Generate trajectories plots
    trajectories_plots$server(
      "trajectories_plots",
      trajectories_data_r = trajectories_data_r,
      max_trisk_granularity = max_trisk_granularity
    )
  })
}
