box::use(
  shiny[
    moduleServer, NS, observe, div, tags, reactiveVal, reactiveValues, eventReactive, p, tagList, observeEvent, img,
    HTML
  ],
  shiny.semantic[slider_input, dropdown_input, segment, update_dropdown_input, actionButton],
  shinyjs[useShinyjs]
)

box::use(
  app / logic / constant[
    backend_trisk_run_folder,
    trisk_input_path,
    available_discount_rate,
    available_risk_free_rate,
    available_growth_rate,
    available_shock_year,
    available_baseline_scenario,
    available_shock_scenario,
    available_scenario_geography,
    max_crispy_granularity
  ],
  app / logic / ui_renaming[RENAMING_SCENARIOS, REV_RENAMING_SCENARIOS],
  app / logic / trisk_mgmt[run_trisk_with_params, append_st_results_to_backend_data, check_if_run_exists, get_run_data_from_run_id]
)


####### UI

ui <- function(id) {
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
          choices = available_baseline_scenario
        ),
        p("Target Scenario"),
        dropdown_input(ns("shock_scenario"),
          choices = available_shock_scenario
        ),
        p("Scenario Geography"),
        dropdown_input(ns("scenario_geography"),
          choices = available_scenario_geography
        )
      )
    ),
    # Second segment in the right half
    div(
      class = "eight wide column",
      segment(
        p("Discount Rate"),
        slider_input(
          ns("discount_rate"),
          custom_ticks = available_discount_rate,
          value = NULL
        ),
        p("Risk-Free Rate"),
        slider_input(
          ns("risk_free_rate"),
          custom_ticks = available_risk_free_rate,
          value = NULL
        ),
        p("Growth Rate"),
        slider_input(
          ns("growth_rate"),
          custom_ticks = available_growth_rate,
          value = NULL
        ),
        p("Shock Year"),
        slider_input(
          ns("shock_year"),
          custom_ticks = available_shock_year,
          value = NULL
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




server <- function(id) {
  moduleServer(id, function(input, output, session) {
    update_dropdowns(input, session,
      available_baseline_scenario,
      available_shock_scenario,
      available_scenario_geography,
      possible_combinations = r2dii.climate.stress.test::stress_test_arguments_combinations
    )

    run_id_r <- reactiveVal(NULL)

    trisk_run_params_r <- shiny::reactive({
      reactiveValues(
        discount_rate = as.numeric(input$discount_rate),
        risk_free_rate = as.numeric(input$risk_free_rate),
        growth_rate = as.numeric(input$growth_rate),
        shock_year = as.numeric(input$shock_year),
        baseline_scenario = REV_RENAMING_SCENARIOS[input$baseline_scenario],
        shock_scenario = REV_RENAMING_SCENARIOS[input$shock_scenario],
        scenario_geography = input$scenario_geography
      )
    })

    observeEvent(trisk_run_params_r(), {
      trisk_run_params <- shiny::reactiveValuesToList(trisk_run_params_r())

      if (!any(sapply(trisk_run_params, function(x) {
        is.na(x) | (nchar(x) == 0)
      }))) {
        run_id <- check_if_run_exists(trisk_run_params, backend_trisk_run_folder)
        if (is.null(run_id)) {
          shinyjs::runjs("$('#mymodal').modal({closable: false}).modal('show');")
          tryCatch(
            {
              st_results_wrangled_and_checked <- run_trisk_with_params(
                trisk_run_params,
                trisk_input_path
              )
              append_st_results_to_backend_data(
                st_results_wrangled_and_checked,
                backend_trisk_run_folder,
                max_crispy_granularity
              )
            },
            error = function(e) {
              cat(e$message)
              # Do nothing on error
              NULL
            }
          )
          run_id <- check_if_run_exists(trisk_run_params, backend_trisk_run_folder)
        }

        run_id_r(run_id)
      }

      # Close the modal dialog and re-enable UI
      shinyjs::runjs("$('#mymodal').modal('hide');")
    })


    return(run_id_r)
  })
}




update_dropdowns <- function(input, session,
                             available_baseline_scenario,
                             available_shock_scenario,
                             available_scenario_geography,
                             possible_combinations) {
  # Observe changes in possible_combinations and update baseline_scenario dropdown
  observe({
    # Filter the data based on selected baseline scenario
    new_choices <- unique(possible_combinations$baseline_scenario)
    new_choices <- new_choices[new_choices %in% available_baseline_scenario]
    new_choices <- RENAMING_SCENARIOS[new_choices]

    # Update shock_scenario dropdown with unique values from the filtered data
    update_dropdown_input(session, "baseline_scenario", choices = new_choices)
  })

  # Observe changes in baseline_scenario dropdown and update shock_scenario dropdown
  observeEvent(input$baseline_scenario, ignoreInit = TRUE, {
    selected_baseline <- REV_RENAMING_SCENARIOS[input$baseline_scenario]

    # Filter the data based on selected baseline scenario
    new_choices <- unique(possible_combinations[possible_combinations$baseline_scenario == selected_baseline, ]$shock_scenario)
    new_choices <- new_choices[new_choices %in% available_shock_scenario]
    new_choices <- RENAMING_SCENARIOS[new_choices]

    # Update shock_scenario dropdown with unique values from the filtered data
    update_dropdown_input(session, "shock_scenario", choices = new_choices)
  })

  # Observe changes in both baseline_scenario and shock_scenario dropdowns to update scenario_geography dropdown
  observeEvent(c(input$baseline_scenario, input$shock_scenario), ignoreInit = TRUE, {
    selected_baseline <- REV_RENAMING_SCENARIOS[input$baseline_scenario]
    selected_shock <- REV_RENAMING_SCENARIOS[input$shock_scenario]

    # Filter the data based on selected baseline and shock scenarios
    new_choices <- unique(possible_combinations |>
      dplyr::filter(
        .data$baseline_scenario == selected_baseline,
        .data$shock_scenario == selected_shock,
        .data$scenario_geography %in% available_scenario_geography
      ) |>
      dplyr::pull(scenario_geography))

    # Update scenario_geography dropdown with unique values from the filtered data
    update_dropdown_input(session, "scenario_geography", choices = new_choices)
  })
}
