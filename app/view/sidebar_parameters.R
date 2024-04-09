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
  app/view/params_scenarios,
  app/view/params_dimensions,
  app/view/params_trisk,
  app/view/trisk_button,
  app/logic/renamings[rename_string_vector]
)


####### UI

ui <- function(id, max_trisk_granularity, available_vars) {
  ns <- NS(id)

  shiny::tagList(
    div(
      class = "content",
      tags$div(class = "header", "Data", style = "font-size: 150%;"),
      tags$hr(),
      # Run TRISK button
      trisk_button$ui(ns("trisk_button")),
      # Dimensions
      params_dimensions$ui(ns("params_dimensions"), max_trisk_granularity)
    ),
    div(
      class = "content",
      shinyjs::useShinyjs(),
      div(class = "header", "Scenario Choice", style = "font-size: 150%;"),
      tags$hr(),
      #  Scenario Choice
      params_scenarios$ui(ns("params_scenarios"))
    ),
    # TRISK params

    div(
      class = "content",
      div(class = "header", "TRISK parameters", style = "font-size: 150%;"),
      tags$hr(), # esthetic separation
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


    perimeter <- list(
      "trisk_granularity_r" = trisk_granularity_r,
      "trisk_run_params_r" = trisk_run_params_r,
      "crispy_data_r" = crispy_data_r,
      "trajectories_data_r" = trajectories_data_r
    )

    return(
      perimeter
    )
  })
}
