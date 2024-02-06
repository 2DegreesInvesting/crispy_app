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
  app / logic / renamings[rename_string_vector]
)


####### UI

ui <- function(id, max_trisk_granularity, available_vars) {
  ns <- NS(id)

  
    tagList(
      # First segment in the left half // Granularity
      shiny.semantic::segment(
        div(
          class = "content",
          div(class = "header", "Granularity")
        ),
        div(
          class = "content",
          slider_input(
            ns("granularity_switch"),
            custom_ticks = rename_string_vector(names(max_trisk_granularity), words_class = "analysis_columns"),
            value = rename_string_vector(names(which(max_trisk_granularity == 1)), words_class = "analysis_columns")
          )
        )
      ),
      # Second segment in the left half // Scenario Choice
      segment(
        div(
          class = "content",
          div(class = "header", "Scenario Choice"),
          div(
            class = "description",
            div(
              class = "content",
              div(class = "header", "Baseline Scenario"),
              div(
                class = "description",
                dropdown_input(ns("baseline_scenario"),
                  choices = NULL
                )
              )
            ),
            div(
              class = "content",
              div(class = "header", "Target Scenario"),
              div(
                class = "description",
                dropdown_input(ns("shock_scenario"),
                  choices = NULL
                )
              )
            ),
            div(
              class = "content",
              div(class = "header", "Scenario Geography"),
              div(
                class = "description",
                dropdown_input(ns("scenario_geography"),
                  choices = NULL
                )
              )
            )
          )
        )
      ),
      # Third segment in the left half // TRISK params
      segment(
        div(
          class = "content",
          div(class = "header", "TRISK params"),
          div(
            class = "description",
            div(
              class = "content",
              div(class = "header", "Shock Year"),
              div(
                class = "description",
                slider_input(
                  ns("shock_year"),
                  custom_ticks = available_vars$available_shock_year,
                  value = NULL
                )
              )
            )
          ),
          p("Risk-Free Rate"),
          slider_input(
            ns("risk_free_rate"),
            custom_ticks = available_vars$available_risk_free_rate,
            value = NULL
          ),
          p("Discount Rate"),
          slider_input(
            ns("discount_rate"),
            custom_ticks = available_vars$available_discount_rate,
            value = NULL
          ),
          p("Growth Rate"),
          slider_input(
            ns("growth_rate"),
            custom_ticks = available_vars$available_growth_rate,
            value = NULL
          ),
          p("Dividend Rate"),
          slider_input(
            ns("div_netprofit_prop_coef"),
            custom_ticks = available_vars$available_dividend_rate,
            value = NULL
          ),
          p("Carbon Price Model"),
          dropdown_input(ns("carbon_price_model"),
            choices = available_vars$available_carbon_price_model,
            value = "no_carbon_tax"
          ),
          conditionalPanel(
            condition = "input.carbon_price_model != 'no_carbon_tax'",
            p("Market Passthrough"),
            slider_input(
              ns("market_passthrough"),
              custom_ticks = available_vars$available_market_passthrough,
              value = NULL
            ),
            ns = ns
          )
        )
      )
    )
  
}


####### Server



server <- function(id, backend_trisk_run_folder,
                   trisk_input_path,
                   available_vars,
                   hide_vars,
                   max_trisk_granularity,
                   use_ald_sector) {
  moduleServer(id, function(input, output, session) {
    # Update UI elements =========================

    update_scenarios_dropdowns(
      input,
      session,
      trisk_input_path,
      hide_vars,
      use_ald_sector
    )

    sync_discount_and_growth(
      input,
      session,
      available_vars
    )

    # Collect UI elements (and compute trisks if necessary) =========================

    trisk_granularity_r <- get_granularity_columns(
      input,
      max_trisk_granularity
    )

    # reactive variable containing trisk run parameters
    trisk_run_params_r <- shiny::reactive({
      reactiveValues(
        baseline_scenario = rename_string_vector(input$baseline_scenario, words_class = "scenarios", dev_to_ux = FALSE),
        shock_scenario = rename_string_vector(input$shock_scenario, words_class = "scenarios", dev_to_ux = FALSE),
        scenario_geography = input$scenario_geography,
        shock_year = as.numeric(input$shock_year),
        discount_rate = as.numeric(input$discount_rate),
        risk_free_rate = as.numeric(input$risk_free_rate),
        growth_rate = as.numeric(input$growth_rate),
        div_netprofit_prop_coef = as.numeric(input$div_netprofit_prop_coef),
        carbon_price_model = input$carbon_price_model,
        market_passthrough = as.numeric(input$market_passthrough)
      )
    })
    # prevent the UI modal from updating too often (ie. blinking)
    trisk_run_params_r <- shiny::throttle(trisk_run_params_r, millis = 500)

    perimeter <- list(
      "trisk_granularity_r" = trisk_granularity_r,
      "trisk_run_params_r" = trisk_run_params_r
    )

    return(
      perimeter
    )
  })
}


####### Logic

# Synchronise the scenarios available depending on user scenario choice
update_scenarios_dropdowns <- function(input, session,
                                       trisk_input_path,
                                       hide_vars,
                                       use_ald_sector) {
  possible_combinations <- r2dii.climate.stress.test::get_scenario_geography_x_ald_sector(trisk_input_path)
  # Observe changes in possible_combinations and update baseline_scenario dropdown
  observe({
    possible_baselines <- possible_combinations |>
      dplyr::distinct(.data$baseline_scenario) |>
      dplyr::filter(!is.na(.data$baseline_scenario)) |>
      dplyr::filter(!.data$baseline_scenario %in% hide_vars$hide_baseline_scenario) |>
      dplyr::pull()

    # rename the scenarios to front end appropriate name
    new_choices <- rename_string_vector(possible_baselines, words_class = "scenarios")

    # Update shock_scenario dropdown with unique values from the filtered data
    update_dropdown_input(session, "baseline_scenario", choices = new_choices)
  })

  # Observe changes in baseline_scenario dropdown and update shock_scenario dropdown
  observeEvent(input$baseline_scenario, ignoreInit = TRUE, {
    selected_baseline <- rename_string_vector(input$baseline_scenario, words_class = "scenarios", dev_to_ux = FALSE)

    possible_shocks <- possible_combinations |>
      dplyr::filter(.data$baseline_scenario == selected_baseline) |>
      dplyr::distinct(.data$shock_scenario) |>
      dplyr::filter(!is.na(.data$shock_scenario)) |>
      dplyr::filter(!.data$shock_scenario %in% hide_vars$hide_shock_scenario) |>
      dplyr::pull()


    # rename the scenarios to front end appropriate name
    new_choices <- rename_string_vector(possible_shocks, words_class = "scenarios")

    # Update shock_scenario dropdown with unique values from the filtered data
    update_dropdown_input(session, "shock_scenario", choices = new_choices)
  })

  # Observe changes in both baseline_scenario and shock_scenario dropdowns to update scenario_geography dropdown
  observeEvent(c(input$baseline_scenario, input$shock_scenario), ignoreInit = TRUE, {
    selected_baseline <- rename_string_vector(input$baseline_scenario, words_class = "scenarios", dev_to_ux = FALSE)
    selected_shock <- rename_string_vector(input$shock_scenario, words_class = "scenarios", dev_to_ux = FALSE)

    # Filter the data based on selected baseline and shock scenarios
    possible_geographies <- possible_combinations |>
      dplyr::filter(
        .data$baseline_scenario == selected_baseline,
        .data$shock_scenario == selected_shock
      ) |>
      dplyr::group_by(.data$shock_scenario, .data$baseline_scenario, .data$scenario_geography) |>
      dplyr::filter(all(use_ald_sector %in% .data$ald_sector)) |> # Only use geographies present in all use_ald_sector
      dplyr::ungroup() |>
      dplyr::distinct(.data$scenario_geography) |>
      dplyr::filter(!is.na(.data$scenario_geography)) |>
      dplyr::filter(!.data$scenario_geography %in% hide_vars$hide_scenario_geography) |>
      dplyr::pull()

    new_choices <- possible_geographies

    # Update scenario_geography dropdown with unique values from the filtered data
    update_dropdown_input(session, "scenario_geography", choices = new_choices)
  })
}

# synchronise discount and growth rates sliders,
# to always keep growth rate < discount rate
sync_discount_and_growth <- function(input, session, available_vars) {
  # When growth rate changes, check if growth rate is higher and adjust if necessary
  observeEvent(c(input$growth_rate, input$discount_rate), {
    if (input$growth_rate >= input$discount_rate) {
      # Find the closest smaller value in 'available_growth_rate'

      smaller_values <- available_vars$available_growth_rate[available_vars$available_growth_rate < input$discount_rate]
      closest_smaller_value <- sort(smaller_values)[length(smaller_values)]

      # Update growth_rate slider
      update_slider(session, "growth_rate", value = as.character(closest_smaller_value))
    }
  })
}

# get the column names defining the displayed data granularity
get_granularity_columns <- function(input, max_trisk_granularity) {
  # get granularity columns
  trisk_granularity_r <- eventReactive(input$granularity_switch, ignoreNULL = TRUE, {
    granularity_picked <- input$granularity_switch |>
      rename_string_vector(words_class = "analysis_columns", dev_to_ux = FALSE)

    granularity_level <- max_trisk_granularity[granularity_picked]
    # Filter names based on values <= given_integer
    trisk_granularity <- names(max_trisk_granularity)[sapply(max_trisk_granularity, function(value) value <= granularity_level)]

    return(trisk_granularity)
  })
  return(trisk_granularity_r)
}
