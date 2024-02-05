# This file contains the main code for the CRISPY Shiny application. It defines the UI and server functions.

# Load required packages
box::use(
  shiny[moduleServer, NS, tagList]
)

box::use(
    app/view/modules/equity_change_plots,
    app/view/modules/trajectories_plots
)

####### UI

ui <- function(id){
    ns <- NS(id)
    tagList(
        equity_change_plots$ui(ns("equity_change_plots")),
        trajectories_plots$ui(ns("trajectories_plots"))
    )
}

####### Server

server <- function(id, analysis_data_r, trajectories_data_r, max_trisk_granularity) {
  moduleServer(id, function(input, output, session) {
    # Generate equity change plots
    equity_change_plots$server("equity_change_plots", analysis_data_r=analysis_data_r, max_trisk_granularity=max_trisk_granularity)

    # Generate trajectories plots
    trajectories_plots$server("trajectories_plots", trajectories_data_r=trajectories_data_r, max_trisk_granularity=max_trisk_granularity)

      })
}