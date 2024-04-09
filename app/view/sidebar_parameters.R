box::use(
  shiny[
    moduleServer, NS, observe, div, tags, reactiveVal, reactiveValues, eventReactive, p, tagList, observeEvent, img,
    HTML, conditionalPanel
  ],
  shiny.semantic[slider_input, dropdown_input, segment, update_dropdown_input, update_slider],
  shinyjs[useShinyjs],
  semantic.dashboard[dashboardSidebar]
)

box::use(
  app/view/modules/params_scenarios,
  app/view/modules/params_dimensions,
  app/view/modules/params_trisk,
  app/view/modules/trisk_button,
  app/view/modules/portfolio_upload,
  app/logic/renamings[rename_string_vector]
)


####### UI
ui <- function(id, max_trisk_granularity, available_vars) {
  ns <- NS(id)
  shiny::tagList(
    shiny::tags$head(
      shiny::tags$style(HTML(paste0("
        .sidebar-section { 
          padding: 20px; 
          background-color: #f9f9f9; 
          margin: 15px 0; 
          border-radius: 8px; 
          box-shadow: 0 2px 4px rgba(0,0,0,0.1);
        }
        .sidebar-section .ui.header { 
          font-size: 18px; 
          color: #333; 
          margin-bottom: 15px; 
        }
        .ui.button, .ui.input { /* Removed the extra comma here */
          background-color: #d4d4d5; 
          color: white; 
          border: none; 
          border-radius: 4px; 
          margin: 8px 0; 
          display: block; 
          width: 100%; /* Ensures full width */
        }
        .ui.button:hover { /* Fixed the syntax here by removing extra comma and braces */
          background-color: #cacbcd;
        }
        .ui.divider { 
          margin: 20px 0; 
        }
        .ui.dropdown { 
          width: 100%; 
          margin-bottom: 10px; 
        }
      ")))
    ),
    # Data Section
    div(
      class = "sidebar-section",
      shiny::tags$div(class = "ui header", "Data"),
      shiny::tags$div(class = "ui divider"),
      # Run TRISK button
      trisk_button$ui(ns("trisk_button")),
      # Dimensions
      params_dimensions$ui(ns("params_dimensions"), max_trisk_granularity),
      portfolio_upload$ui(ns("portfolio_upload"))
    ),
    # Scenario Choice Section
    div(
      class = "sidebar-section",
      shinyjs::useShinyjs(),
      shiny::tags$div(class = "ui header", "Scenario Choice"),
      shiny::tags$div(class = "ui divider"),
      # Scenario Choice
      params_scenarios$ui(ns("params_scenarios"))
    ),
    # TRISK Parameters Section
    div(
      class = "sidebar-section",
      shiny::tags$div(class = "ui header", "TRISK Parameters"),
      shiny::tags$div(class = "ui divider"),
      params_trisk$ui(ns("params_trisk"), available_vars)
    )
  )
}


####### Server



server <- function(id, backend_trisk_run_folder, trisk_input_path,
                   possible_trisk_combinations,
                   available_vars,
                   hide_vars,
                   max_trisk_granularity) {
  moduleServer(id, function(input, output, session) {
    # Update UI elements =========================

    # Collect UI elements (and compute trisks if necessary) =========================
    trisk_granularity_r <- params_dimensions$server(
      "params_dimensions",
      max_trisk_granularity = max_trisk_granularity
    )


    scenario_config_r <- params_scenarios$server(
      "params_scenarios",
      hide_vars = hide_vars,
      possible_trisk_combinations = possible_trisk_combinations
    )

    trisk_config_r <- params_trisk$server("params_trisk", available_vars)


    # reactive variable containing trisk run parameters
    trisk_run_params_r <- shiny::reactive({
      reactiveValues(
        baseline_scenario = scenario_config_r()$baseline_scenario,
        shock_scenario = scenario_config_r()$shock_scenario,
        scenario_geography = scenario_config_r()$scenario_geography,
        shock_year = trisk_config_r()$shock_year,
        discount_rate = trisk_config_r()$discount_rate,
        risk_free_rate = trisk_config_r()$risk_free_rate,
        growth_rate = trisk_config_r()$growth_rate,
        div_netprofit_prop_coef = trisk_config_r()$div_netprofit_prop_coef,
        carbon_price_model = trisk_config_r()$carbon_price_model,
        market_passthrough = trisk_config_r()$market_passthrough
      )
    })

    results <- trisk_button$server(
      "trisk_button",
      trisk_run_params_r = trisk_run_params_r,
      trisk_granularity_r = trisk_granularity_r,
      backend_trisk_run_folder = backend_trisk_run_folder,
      trisk_input_path = trisk_input_path,
      max_trisk_granularity = max_trisk_granularity
    )

    crispy_data_r <- results$crispy_data_r
    trajectories_data_r <- results$trajectories_data_r

    portfolio_uploaded_r <- portfolio_upload$server("portfolio_upload")


    perimeter <- list(
      "trisk_granularity_r" = trisk_granularity_r,
      "trisk_run_params_r" = trisk_run_params_r,
      "crispy_data_r" = crispy_data_r,
      "trajectories_data_r" = trajectories_data_r
    )

    return( list(
      "perimeter"=perimeter,
      "portfolio_uploaded_r"=portfolio_uploaded_r
    ))
  })
}
