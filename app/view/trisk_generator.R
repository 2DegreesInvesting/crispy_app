box::use(
  shiny[
    moduleServer, NS, observe, div, tags, reactiveVal, reactiveValues, eventReactive, p, tagList, observeEvent, img,
    HTML, conditionalPanel
  ],
  shiny.semantic[slider_input, dropdown_input, segment, update_dropdown_input, update_slider],
  shinyjs[useShinyjs],
  shiny.semantic[card, cards]
)

box::use(
  app/logic/renamings[rename_string_vector],
  app/logic/trisk_mgmt[run_trisk_with_params, append_st_results_to_backend_data, check_if_run_exists, get_run_data_from_run_id]
)


####### UI

ui <- function(id, available_vars) {
  ns <- NS(id)
  list(
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
    segment(
        div(class="content",
         div(class="header", "Scenario Choice"),
        div(class="description", 
        div(class="content",
        div(class="header", "Baseline Scenario"),
        div(class="description", dropdown_input(ns("baseline_scenario"),
          choices = NULL
        ))),
        div(class="content",
        div(class="header", "Target Scenario"),
        div(class="description", dropdown_input(ns("shock_scenario"),
          choices = NULL
        ))),
        div(class="content",
        div(class="header", "Scenario Geography"),
        div(class="description", dropdown_input(ns("scenario_geography"),
          choices = NULL
        )))
      ))),
      segment(
         div(class="content",
         div(class="header", "TRISK params"),
        div(class="description", 
        div(class="content",
        div(class="header",  "Shock Year"),
        div(class="description", slider_input(
          ns("shock_year"),
          custom_ticks = available_vars$available_shock_year,
          value = NULL
        )))),
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
    update_dropdowns(
      input, session, trisk_input_path, hide_vars, use_ald_sector
    )

    sync_discount_and_growth(input, session, available_vars)

    run_id_r <- reactiveVal(NULL)

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

    trisk_run_params_r <- shiny::throttle(trisk_run_params_r, millis = 500)
    observeEvent(trisk_run_params_r(), {
      trisk_run_params <- shiny::reactiveValuesToList(trisk_run_params_r())


      all_input_params_initialized <- !any(sapply(trisk_run_params, function(x) {
        is.null(x)
      }))

      if (all_input_params_initialized) {
        if (trisk_run_params$carbon_price_model == "no_carbon_tax") {
          trisk_run_params$market_passthrough <- 0
        }


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
            # Close the modal dialog and re-enable UI
            append_st_results_to_backend_data(
              st_results_wrangled_and_checked,
              backend_trisk_run_folder,
              max_trisk_granularity
            )
          }
          shinyjs::runjs("$('#mymodal').modal('hide');")
        }
        run_id <- check_if_run_exists(trisk_run_params, backend_trisk_run_folder)
        run_id_r(run_id)
      }
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
