# This file contains the main code for the CRISPY Shiny application. It defines the UI and server functions.

# Load required packages
box::use(
  shiny[moduleServer, NS, tags, HTML, reactiveVal, observeEvent],
  shiny.semantic[semanticPage, segment, slider_input, card],
  semantic.dashboard[dashboardPage, dashboardBody, icon, box]
)

box::use(
  app/view/portfolio_analysis,
  app/view/plots_equities,
  app/view/plots_trajectories,
)

####### UI

ui <- function(id, max_trisk_granularity, available_vars) {
  ns <- NS(id)

  # dashboardBody
  shiny::tags$div(
    class = "pusher container", style = "min-height: 100vh;",
    shiny::tags$div(
      class = "ui segment", style = "min-height: 100vh;",
      shiny::tags$div(
        class = "ui stackable grid",
        portfolio_analysis$ui(ns("portfolio_analysis"), portfolio_class = "Equities portfolio"),
        plots_equities$ui(ns("plots_equities")),
        plots_trajectories$ui(ns("plots_trajectories"))
      )
    )
  )
}

####### Server

server <- function(id, perimeter, backend_trisk_run_folder, max_trisk_granularity) {
  moduleServer(id, function(input, output, session) {
    trisk_granularity_r <- perimeter$trisk_granularity_r
    trisk_run_params_r <- perimeter$trisk_run_params_r
    crispy_data_r <- perimeter$crispy_data_r
    trajectories_data_r <- perimeter$trajectories_data_r

    # GET RESULTS FROM CONFIG =========================

    display_columns_equities <- c(
      names(max_trisk_granularity),
      "exposure_value_usd",
      "crispy_perc_value_change",
      "crispy_value_loss"
    )
    editable_columns_names_equities <- c("exposure_value_usd")
    colored_columns_names_equities <- c("crispy_perc_value_change", "crispy_value_loss")

    # MANAGE PORTFOLIO =========================

    # Manages the porfolio creator module
    # Create analysis data by merging crispy to portfolio, and aggrgating to the appropriate granularity
    out <- portfolio_analysis$server(
      "portfolio_analysis",
      portfolio_class = "Equities portfolio",
      crispy_data_r = crispy_data_r,
      trisk_granularity_r = trisk_granularity_r,
      max_trisk_granularity = max_trisk_granularity,
      portfolio_asset_type = "equity",
      display_columns = display_columns_equities,
      editable_columns_names = editable_columns_names_equities,
      colored_columns_names = colored_columns_names_equities
    )

    analysis_data_r <- out$analysis_data_r
    crispy_data_agg_r <- out$crispy_data_agg_r

    # CONSUME TRISK OUTPUTS =========================

    # Generate equity change plots
    plots_equities$server(
      "plots_equities",
      analysis_data_r = analysis_data_r,
      max_trisk_granularity = max_trisk_granularity
    )

    # Generate trajectories plots
    plots_trajectories$server(
      "plots_trajectories",
      trajectories_data_r = trajectories_data_r,
      max_trisk_granularity = max_trisk_granularity
    )
  })
}
