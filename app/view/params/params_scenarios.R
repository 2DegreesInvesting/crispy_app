box::use(
  shiny[
    moduleServer, NS, observe, div, tags, reactiveVal, reactiveValues, eventReactive, p, tagList, observeEvent, img,
    HTML, conditionalPanel, reactive
  ],
  shiny.semantic[slider_input, dropdown_input, segment, update_dropdown_input, update_slider],
  shinyjs[useShinyjs],
  semantic.dashboard[dashboardSidebar]
)

box::use(
  app / logic / renamings[rename_string_vector]
)

ui <- function(id) {
  ns <- NS(id)
  segment(
    div(
      class = "content",
      shinyjs::useShinyjs(),
      div(class = "header", "Scenario Choice", style = "font-size: 150%;"),
      tags$hr(),
      div(
        class = "description",
        div(
          class = "content",
          p("Baseline Scenario"),
          div(
            class = "description",
            dropdown_input(ns("baseline_scenario"),
                choices = NULL)
          )
        ),
        div(
          class = "content",
          p("Target Scenario"),
          div(
            class = "description",
            dropdown_input(ns("shock_scenario"),
                choices = NULL)
          )
        ),
        div(
          class = "content",
          p("Scenario Geography"),
          div(
            class = "description",
            dropdown_input(ns("scenario_geography"),
                choices = NULL)
          )
        )
      )
    )
  )
}

server <- function(id,
                   hide_vars,
                   use_ald_sector,
                   possible_trisk_combinations) {
  moduleServer(id, function(input, output, session) {
    # Synchronise the scenarios available depending on user scenario choice
    selected_baseline_r <- reactive({
      choice <- input$baseline_scenario
      renamed_choice <- rename_string_vector(choice, words_class = "scenarios", dev_to_ux = FALSE)
      return(renamed_choice)
    })
    selected_shock_r <- reactive({
      choice <- input$shock_scenario
      renamed_choice <- rename_string_vector(choice, words_class = "scenarios", dev_to_ux = FALSE)
      return(renamed_choice)
    })
    selected_geography_r <- reactive({
      choice <- input$scenario_geography
      return(choice)
    })

    # synchronise dropdown choices  with the possible combinations
    update_scenarios_dropdowns(
      input=input,
      session=session,
      trisk_input_path=trisk_input_path,
      hide_vars=hide_vars,
      use_ald_sector=use_ald_sector,
      possible_trisk_combinations=possible_trisk_combinations
    )

    # RETURN THE SCENARIOS
    scenario_config_r <- reactive({
      reactiveValues(
        baseline_scenario = selected_baseline_r(),
        shock_scenario = selected_shock_r(),
        scenario_geography = selected_geography_r()
      )
    })

    return(scenario_config_r)
  })
}





# Synchronise the scenarios available depending on user scenario choice
update_scenarios_dropdowns <- function(input, session,
                                       trisk_input_path,
                                       hide_vars,
                                       use_ald_sector,
                                       possible_trisk_combinations) {
  
  # Observe changes in possible_trisk_combinations and update baseline_scenario dropdown
  observe({
    possible_baselines <- possible_trisk_combinations |>
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

    possible_shocks <- possible_trisk_combinations |>
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
    possible_geographies <- possible_trisk_combinations |>
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



