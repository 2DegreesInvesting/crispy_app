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
  app / view / params / params_scenarios,
  app / view / params / params_dimensions,
  app / view / params / params_trisk,
  app / logic / renamings[rename_string_vector]
)


####### UI

ui <- function(id, max_trisk_granularity, available_vars) {
  ns <- NS(id)

  list(
    params_dimensions$ui(ns("params_dimensions"), max_trisk_granularity),
    # Second segment in the left half // Scenario Choice
    params_scenarios$ui(ns("params_scenarios")),
    # Third segment in the left half // TRISK params
    params_trisk$ui(ns("params_trisk"), available_vars)
  )
}


####### Server



server <- function(id, backend_trisk_run_folder,
                   possible_trisk_combinations,
                   available_vars,
                   hide_vars,
                   max_trisk_granularity,
                   use_ald_sector) {
  moduleServer(id, function(input, output, session) {
    # Update UI elements =========================

    # Collect UI elements (and compute trisks if necessary) =========================
    trisk_granularity_r <- params_dimensions$server(
      "granularity_switch",
      max_trisk_granularity = max_trisk_granularity
    )


    scenario_config_r <- params_scenarios$server(
      "params_scenarios",
      hide_vars = hide_vars,
      use_ald_sector = use_ald_sector,
      possible_trisk_combinations = possible_trisk_combinations
    )

    trisk_config_r <- params_trisk$server("trisk_params", available_vars)


    # reactive variable containing trisk run parameters
    trisk_run_params_r <- shiny::reactive({
      reactiveValues(
        baseline_scenario = scenario_config_r()$baseline_scenario,
        shock_scenario = scenario_config_r()$shock_scenario,
        scenario_geography = scenario_config_r()$scenario_geography,
        shock_year = trisk_config_r()$shock_year,
        discount_rate = trisk_config_r()$discount_and_growth$discount_rate,
        risk_free_rate = trisk_config_r()$risk_free_rate,
        growth_rate = trisk_config_r()$discount_and_growth$growth_rate,
        div_netprofit_prop_coef = trisk_config_r()$div_netprofit_prop_coef,
        carbon_price_model = trisk_config_r()$carbon_price_model,
        market_passthrough = trisk_config_r()$market_passthrough
      )
    })
    # prevent the UI modal from updating too often (ie. blinking)
    # trisk_run_params_r <- shiny::throttle(trisk_run_params_r, millis = 200)

    perimeter <- list(
      "trisk_granularity_r" = trisk_granularity_r,
      "trisk_run_params_r" = trisk_run_params_r
    )

    return(
      perimeter
    )
  })
}
