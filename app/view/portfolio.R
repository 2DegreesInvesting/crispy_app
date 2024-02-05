# This file contains the main code for the CRISPY Shiny application. It defines the UI and server functions.

# Load required packages
box::use(
  shiny[moduleServer, NS, tagList]
)

box::use(
    app/view/modules/portfolio_mgmt
)

####### UI

ui <- function(id){
    ns <- NS(id)
    tagList(
        portfolio_mgmt$ui(ns("portfolio_equities"), title="Equities portfolio")
    )
}

####### Server

server <- function(id, crispy_data_r, trisk_granularity_r, max_trisk_granularity) {
  moduleServer(id, function(input, output, session) {

        display_columns_equities <- c(
      names(max_trisk_granularity),
      "exposure_value_usd",
      "crispy_perc_value_change",
      "crispy_value_loss"
    )

analysis_data_r <- portfolio_mgmt$server(
 "portfolio_equities", 
 crispy_data_r=crispy_data_r, 
 trisk_granularity_r=trisk_granularity_r, 
 max_trisk_granularity=max_trisk_granularity,
 display_columns=display_columns_equities, 
 editable_columns_names=c("exposure_value_usd"), 
 colored_columns_names=c("crispy_perc_value_change", "crispy_value_loss")
 )

 return(analysis_data_r)

      })
}