box::use(
  shiny[moduleServer, NS, observe, div, tags, reactiveVal, reactiveValues, eventReactive, p, tagList, observeEvent, img],
  shiny.semantic[slider_input, dropdown_input, segment, update_dropdown_input]
)

box::use(
  app / logic / constant[
    available_discount_rate,
    available_risk_free_rate,
    available_growth_rate,
    available_shock_year,
    available_baseline_scenario,
    available_shock_scenario,
    available_scenario_geography
  ],
  app / logic / ui_renaming[RENAMING_SCENARIOS, REV_RENAMING_SCENARIOS]
)



####### UI

ui <- function(id) {
  ns <- NS(id)
  div(
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


server <- function(id, backend_trisk_run_data) {
  moduleServer(id, function(input, output, session) {
    update_dropdowns(input, session, backend_trisk_run_data)

    run_id_r <- reactiveVal()

    observeEvent(
      c(
        input$discount_rate,
        input$risk_free_rate,
        input$growth_rate,
        input$shock_year,
        input$baseline_scenario,
        input$shock_scenario,
        input$scenario_geography
      ),
      ignoreInit = TRUE,
      {
        run_id_selected <- backend_trisk_run_data |>
          dplyr::filter(
            .data$discount_rate == input$discount_rate,
            .data$risk_free_rate == input$risk_free_rate,
            .data$growth_rate == input$growth_rate,
            .data$shock_year == input$shock_year,
            .data$baseline_scenario == REV_RENAMING_SCENARIOS[input$baseline_scenario],
            .data$shock_scenario == REV_RENAMING_SCENARIOS[input$shock_scenario],
            .data$scenario_geography == input$scenario_geography
          ) |>
          dplyr::pull(run_id)

        run_id_r(run_id_selected)
      }
    )


    return(run_id_r)
  })
}



update_dropdowns <- function(input, session, backend_trisk_run_data) {
  # Observe changes in backend_trisk_run_data and update baseline_scenario dropdown
  observe({
    # Filter the data based on selected baseline scenario
    new_choices <- unique(backend_trisk_run_data$baseline_scenario)
    new_choices <- RENAMING_SCENARIOS[new_choices]

    # Update shock_scenario dropdown with unique values from the filtered data
    update_dropdown_input(session, "baseline_scenario", choices = new_choices)
  })

  # Observe changes in baseline_scenario dropdown and update shock_scenario dropdown
  observeEvent(input$baseline_scenario, ignoreInit = TRUE, {
    selected_baseline <- REV_RENAMING_SCENARIOS[input$baseline_scenario]

    # Filter the data based on selected baseline scenario
    new_choices <- unique(backend_trisk_run_data[backend_trisk_run_data$baseline_scenario == selected_baseline, ]$shock_scenario)
    new_choices <- RENAMING_SCENARIOS[new_choices]

    # Update shock_scenario dropdown with unique values from the filtered data
    update_dropdown_input(session, "shock_scenario", choices = new_choices)
  })

  # Observe changes in both baseline_scenario and shock_scenario dropdowns to update scenario_geography dropdown
  observeEvent(c(input$baseline_scenario, input$shock_scenario), ignoreInit = TRUE, {
    selected_baseline <- REV_RENAMING_SCENARIOS[input$baseline_scenario]
    selected_shock <- REV_RENAMING_SCENARIOS[input$shock_scenario]

    # Filter the data based on selected baseline and shock scenarios
    new_choices <- unique(backend_trisk_run_data |>
      dplyr::filter(
        .data$baseline_scenario == selected_baseline,
        .data$shock_scenario == selected_shock
      ) |>
      dplyr::pull(scenario_geography))

    # Update scenario_geography dropdown with unique values from the filtered data
    update_dropdown_input(session, "scenario_geography", choices = new_choices)
  })
}
