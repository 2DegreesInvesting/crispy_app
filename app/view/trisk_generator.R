box::use(
  shiny[
    moduleServer, NS, observe, div, tags, reactiveVal, reactiveValues, eventReactive, p, tagList, observeEvent, img,
    HTML, conditionalPanel
  ],
  shiny.semantic[slider_input, dropdown_input, segment, update_dropdown_input, update_slider],
  shinyjs[useShinyjs]
)

box::use(
  app/logic/renamings[RENAMING_SCENARIOS, REV_RENAMING_SCENARIOS],
  app/logic/trisk_mgmt[run_trisk_with_params, append_st_results_to_backend_data, check_if_run_exists, get_run_data_from_run_id]
)


####### UI

ui <- function(id, available_vars) {
  ns <- NS(id)
  div(
    useShinyjs(), # Initialize shinyjs
    # Custom Semantic UI Modal
    tags$div(
      id = "mymodal",
      class = "ui modal",
      tags$div(class = "header", "Processing"),
      tags$div(
        class = "content",
        tags$p("Please wait...")
      )
    ),
    # First segment in the left half
    div(
      class = "eight wide column",
      segment(
        p("Baseline Scenario"),
        dropdown_input(ns("baseline_scenario"),
          choices = NULL
        ),
        p("Target Scenario"),
        dropdown_input(ns("shock_scenario"),
          choices = NULL
        ),
        p("Scenario Geography"),
        dropdown_input(ns("scenario_geography"),
          choices = NULL
        )
      )
    ),
    # Second segment in the right half
    div(
      class = "eight wide column",
      segment(
        p("Shock Year"),
        slider_input(
          ns("shock_year"),
          custom_ticks = available_vars$available_shock_year,
          value = NULL
        ),
        p("Risk-Free Rate"),
        slider_input(
          ns("risk_free_rate"),
          custom_ticks = available_vars$available_risk_free_rate,
          value = NULL
        ),
        p("Growth Rate"),
        slider_input(
          ns("growth_rate"),
          custom_ticks = available_vars$available_growth_rate,
          value = NULL
        ),
        p("Discount Rate"),
        slider_input(
          ns("discount_rate"),
          custom_ticks = available_vars$available_discount_rate,
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
    ),
    img(
      src = "static/logo_1in1000.png",
      height = "20%", width = "auto",
      style = "
      display: block;
      margin-left: auto;
      margin-right: auto;
      margin-top: 10px;
      margin-bottom: 10px;"
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
    update_dropdowns(
      input, session, trisk_input_path, hide_vars, use_ald_sector
    )

    sync_discount_and_growth(input, session, available_vars)

    run_id_r <- reactiveVal(NULL)

    trisk_run_params_r <- shiny::reactive({
      reactiveValues(
        baseline_scenario = REV_RENAMING_SCENARIOS[input$baseline_scenario],
        shock_scenario = REV_RENAMING_SCENARIOS[input$shock_scenario],
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

    observeEvent(trisk_run_params_r(), {
      trisk_run_params <- shiny::reactiveValuesToList(trisk_run_params_r())
      if (trisk_run_params$carbon_price_model == "no_carbon_tax") {
        trisk_run_params$market_passthrough <- 0
      }

      all_input_params_initialized <- !any(sapply(trisk_run_params, function(x) {
        is.na(x) | (nchar(x) == 0)
      }))
      if (all_input_params_initialized) {
        run_id <- check_if_run_exists(trisk_run_params, backend_trisk_run_folder)
        if (is.null(run_id)) {
          shinyjs::runjs("$('#mymodal').modal({closable: false}).modal('show');")
          st_results_wrangled_and_checked <- tryCatch(
            {
              run_trisk_with_params(
                trisk_run_params,
                trisk_input_path
              )
            },
            error = function(e) {
              cat(e$message)
              format_error_message(trisk_run_params)
              NULL
            }
          )

          if (!is.null(st_results_wrangled_and_checked)) {
            append_st_results_to_backend_data(
              st_results_wrangled_and_checked,
              backend_trisk_run_folder,
              max_trisk_granularity
            )
          }
        }
      }

      run_id <- check_if_run_exists(trisk_run_params, backend_trisk_run_folder)


      run_id_r(run_id)


      # Close the modal dialog and re-enable UI
      shinyjs::runjs("$('#mymodal').modal('hide');")
    })


    return(run_id_r)
  })
}




update_dropdowns <- function(input, session,
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
    new_choices <- RENAMING_SCENARIOS[possible_baselines]

    # Update shock_scenario dropdown with unique values from the filtered data
    update_dropdown_input(session, "baseline_scenario", choices = new_choices)
  })

  # Observe changes in baseline_scenario dropdown and update shock_scenario dropdown
  observeEvent(input$baseline_scenario, ignoreInit = TRUE, {
    selected_baseline <- REV_RENAMING_SCENARIOS[input$baseline_scenario]

    possible_shocks <- possible_combinations |>
      dplyr::filter(.data$baseline_scenario == selected_baseline) |>
      dplyr::distinct(.data$shock_scenario) |>
      dplyr::filter(!is.na(.data$shock_scenario)) |>
      dplyr::filter(!.data$shock_scenario %in% hide_vars$hide_shock_scenario) |>
      dplyr::pull()


    # rename the scenarios to front end appropriate name
    new_choices <- RENAMING_SCENARIOS[possible_shocks]

    # Update shock_scenario dropdown with unique values from the filtered data
    update_dropdown_input(session, "shock_scenario", choices = new_choices)
  })

  # Observe changes in both baseline_scenario and shock_scenario dropdowns to update scenario_geography dropdown
  observeEvent(c(input$baseline_scenario, input$shock_scenario), ignoreInit = TRUE, {
    selected_baseline <- REV_RENAMING_SCENARIOS[input$baseline_scenario]
    selected_shock <- REV_RENAMING_SCENARIOS[input$shock_scenario]

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


format_error_message <- function(trisk_run_params) {
  cat("Failed with parameters : ")

  # Function to format each list element
  format_element <- function(name, value) {
    if (is.numeric(value)) {
      return(paste(name, "=", value, sep = ""))
    } else {
      return(paste(name, "=", sprintf('"%s"', value), sep = ""))
    }
  }

  # Apply the function to each element and concatenate them
  formatted_list <- sapply(names(trisk_run_params), function(name) {
    format_element(name, trisk_run_params[[name]])
  }, USE.NAMES = FALSE)

  # Print the formatted string
  cat(paste(formatted_list, collapse = ", "), "\n")
}
