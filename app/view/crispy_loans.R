# Load required packages
box::use(
  shiny[moduleServer, NS, div, h1, tags, reactiveVal, observeEvent, observe, eventReactive, HTML, updateTextInput],
  shiny.semantic[semanticPage, button, segment],
  semantic.dashboard[dashboardPage, dashboardBody, dashboardSidebar, dashboardHeader, box],
  DT[DTOutput, renderDT, datatable, JS, dataTableProxy, replaceData],
)

box::use(
  app / view / modules / trisk_mgmt,
  app / view / modules / portfolio_analysis
)



####### UI

ui <- function(id, max_trisk_granularity, available_vars) {
  ns <- NS(id)

  shiny::div(
    class = "pusher container", style = "min-height: 100vh;",
    shiny::div(
      class = "ui segment", style = "min-height: 100vh;",
      trisk_mgmt$ui(ns("trisk_mgmt")),
      portfolio_analysis$ui(ns("portfolio_analysis"), "Loans Portfolio")
    )
  )
}

####### Server

server <- function(id, perimeter, backend_trisk_run_folder, trisk_input_path, possible_trisk_combinations, max_trisk_granularity) {
  moduleServer(id, function(input, output, session) {
    # SELECT PARAMETERS =========================
    trisk_granularity_r <- perimeter$trisk_granularity_r
    trisk_run_params_r <- perimeter$trisk_run_params_r

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


    # INIT PORTFOLIO ===============

    display_columns_loans <- c(
      names(max_trisk_granularity),
      "exposure_value_usd",
      "crispy_perc_value_change",
      "crispy_value_loss",
      "loss_given_default",
      "term"
    )
    editable_columns_names_loans <- c("exposure_value_usd", "loss_given_default", "term")
    colored_columns_names_loans <- c("crispy_perc_value_change", "crispy_value_loss")

    analysis_data_r <- portfolio_analysis$server(
      "portfolio_analysis",
      crispy_data_r = crispy_data_r,
      trisk_granularity_r = trisk_granularity_r,
      max_trisk_granularity = max_trisk_granularity,
      portfolio_asset_type = "fixed_income",
      display_columns = display_columns_loans,
      editable_columns_names = editable_columns_names_loans,
      colored_columns_names = colored_columns_names_loans,
      editable_rows = TRUE, # Allow adding and deleting rows, and gives access to the company granularity
      possible_trisk_combinations=possible_trisk_combinations
    )
  })
}




render_portfolio <- function(output, table_to_display) {
  output$portfolio_table <- renderDT(
    {
      datatable(table_to_display,
        editable = TRUE,
        options = list(
          lengthChange = FALSE, # Remove "Show XXX entries" option
          paging = FALSE, # Remove pagination
          searching = FALSE, # Remove search input
          info = FALSE # Remove "Showing N of X entries"
        )
      )
    },
    server = FALSE
  )
}
