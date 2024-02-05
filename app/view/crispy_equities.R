# This file contains the main code for the CRISPY Shiny application. It defines the UI and server functions.

# Load required packages
box::use(
  shiny[moduleServer, NS, tagList]
)

box::use(
    app/view/modules/portfolio_mgmt,
    app/view/modules/equity_change_plots,
    app/view/modules/trajectories_plots
)

####### UI

ui <- function(id){
    ns <- NS(id)
    tagList(
        portfolio_mgmt$ui(ns("portfolio_equities"), title="Equities portfolio"),
        equity_change_plots$ui(ns("equity_change_plots")),
        trajectories_plots$ui(ns("trajectories_plots"))
    )
}

####### Server

server <- function(id, crispy_data_r, trajectories_data_r, trisk_granularity_r, max_trisk_granularity) {
  moduleServer(id, function(input, output, session) {

      display_columns_equities <- c(
        names(max_trisk_granularity),
        "exposure_value_usd",
        "crispy_perc_value_change",
        "crispy_value_loss"
      )
      editable_columns_names <- c("exposure_value_usd")
      colored_columns_names <- c("crispy_perc_value_change", "crispy_value_loss")

      analysis_data_r <- portfolio_mgmt$server(
        "portfolio_equities", 
        crispy_data_r=crispy_data_r, 
        trisk_granularity_r=trisk_granularity_r, 
        max_trisk_granularity=max_trisk_granularity,
        display_columns=display_columns_equities, 
        editable_columns_names=editable_columns_names, 
        colored_columns_names=colored_columns_names
      )

      # Generate equity change plots
      equity_change_plots$server(
        "equity_change_plots", 
        analysis_data_r = analysis_data_r, 
        max_trisk_granularity = max_trisk_granularity
        )

      # Generate trajectories plots
      trajectories_plots$server(
        "trajectories_plots", 
        trajectories_data_r=trajectories_data_r, 
        max_trisk_granularity=max_trisk_granularity
        )

      })
}