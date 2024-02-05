# This file contains the main code for the CRISPY Shiny application. It defines the UI and server functions.

# Load required packages
box::use(
  shiny[moduleServer, NS, tagList],
  shiny.semantic[semanticPage, segment, slider_input, card],
  semantic.dashboard[dashboardPage, dashboardBody, icon, box]
)

box::use(
  app / view / modules / menus[dashboard_header_crispy],
  app / view / modules / portfolio_mgmt,
  app / view / modules / equity_change_plots,
  app / view / modules / trajectories_plots
)

####### UI

ui <- function(id, max_trisk_granularity, available_vars) {
  ns <- NS(id)

  # dashboardBody
  dashboardBody(
    tagList(
      portfolio_mgmt$ui(
        ns("portfolio_equities"),
        title = "Equities portfolio"
      ),
      equity_change_plots$ui(ns("equity_change_plots")),
      trajectories_plots$ui(ns("trajectories_plots"))
    )
  )
}

####### Server

server <- function(id, perimeter, max_trisk_granularity) {
  moduleServer(id, function(input, output, session) {
    # SELECT PARAMETERS =========================


    trisk_granularity_r <- perimeter$trisk_granularity_r
    crispy_data_r <- perimeter$trisk_outputs$crispy_data_r
    trajectories_data_r <- perimeter$trisk_outputs$trajectories_data_r



    # MANAGE PORTFOLIO =========================

    display_columns_equities <- c(
      names(max_trisk_granularity),
      "exposure_value_usd",
      "crispy_perc_value_change",
      "crispy_value_loss"
    )

    # Manages the porfolio creator module
    # Create analysis data by merging crispy to portfolio, and aggrgating to the appropriate granularity
    analysis_data_r <- portfolio_mgmt$server(
      "portfolio_equities",
      crispy_data_r = crispy_data_r,
      trisk_granularity_r = trisk_granularity_r,
      max_trisk_granularity = max_trisk_granularity,
      display_columns = display_columns_equities,
      editable_columns_names = c("exposure_value_usd"),
      colored_columns_names = c("crispy_perc_value_change", "crispy_value_loss")
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
