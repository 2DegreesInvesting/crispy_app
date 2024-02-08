box::use(
  shiny[
    moduleServer, NS, observe, div, tags, reactiveVal, reactiveValues, eventReactive, p, tagList, observeEvent, img,
    HTML, conditionalPanel, reactive
  ],
  shiny.semantic[segment],
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
      div(class = "header", "Scenario Choice", style = "font-size: 150%;"),
      tags$hr(),
      div(
        class = "description",
        div(
          class = "content",
          p("Baseline Scenario"),
          div(
            class = "description",
            create_dropdown_input(ns("baseline_scenario"))
          )
        ),
        div(
          class = "content",
          p("Target Scenario"),
          div(
            class = "description",
            create_dropdown_input(ns("shock_scenario"))
          )
        ),
        div(
          class = "content",
          p("Scenario Geography"),
          div(
            class = "description",
            create_dropdown_input(ns("scenario_geography"))
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
selected_baseline <- reactive({
  rename_string_vector(get_dropdown_choice("baseline_scenario", input, session)(), words_class = "scenarios", dev_to_ux = FALSE)
})
selected_shock <- reactive({
  rename_string_vector(get_dropdown_choice("shock_scenario", input, session)(), words_class = "scenarios", dev_to_ux = FALSE)
})
selected_geography <- reactive({
  scenario_geography <- get_dropdown_choice("scenario_geography", input, session)
  ifelse(is.null(scenario_geography), "", scenario_geography)
})

    
    # Observe changes in possible_trisk_combinations and update baseline_scenario dropdown
    observe({
      
      update_baseline_dropdown(
          session,
          possible_trisk_combinations,
          hide_vars
      )
    })

    # Observe changes in baseline_scenario dropdown and update shock_scenario dropdown
    observeEvent(selected_baseline(), ignoreInit = TRUE, {
      selected_baseline <- rename_string_vector(input$baseline_scenario, words_class = "scenarios", dev_to_ux = FALSE)
        update_shock_dropdown(
          session,
          possible_trisk_combinations,
          hide_vars,
          selected_baseline = selected_baseline()
        )
    })

    # Observe changes in both baseline_scenario and shock_scenario dropdowns to update scenario_geography dropdown
    observeEvent(c(selected_baseline, selected_shock()), ignoreInit = TRUE, {

      update_geography_dropdown(
        session,
        possible_trisk_combinations,
        hide_vars,
        selected_baseline(),
        selected_shock()
      )

    })

# RETURN THE SCENARIOS
scenario_config_r <- reactive({
  reactiveValues(
        baseline_scenario = selected_baseline(),
        shock_scenario = selected_shock(),
        scenario_geography = selected_geography()
    )
    })
return(scenario_config_r)
  })
}



create_dropdown_input <- function(id){
  ns <- NS(id)
  tags$div(
    class = "ui fluid search selection dropdown", id = ns("choices_dropdown"),
    tags$i(class = "dropdown icon"),
    tags$div(class = "menu"),
    # this javascript udpates the dropdown with the new choices
    tags$script(HTML(paste0("
  $(document).ready(function() {
    $('#", ns("choices_dropdown"), "').dropdown(); // Initialize the dropdown

    Shiny.addCustomMessageHandler('", ns("updateDropdown"), "', function(newChoices) {
      var formattedChoices = newChoices.choices.map(function(choice) {
        return { name: choice, value: choice };
      });
      $('#", ns("choices_dropdown"), "').dropdown('change values', formattedChoices); // Directly update dropdown values
    });
  });
")))
  )
}


update_dropdown_input <- function(session, id, choices){
  session$sendCustomMessage(session$ns(paste0(id, "-updateDropdown")), list(id = session$ns(paste0(id, '-choices_dropdown')), choices = choices))
}

get_dropdown_choice <- function(id, input, session){
  return(reactive({
    input[[paste0(session$ns(id), "-choices_dropdown")]]
  }))
}


update_baseline_dropdown <- function(
  session,
  possible_trisk_combinations,
  hide_vars) {

  possible_baselines <- possible_trisk_combinations |>
  dplyr::distinct(.data$baseline_scenario) |>
  dplyr::filter(!is.na(.data$baseline_scenario)) |>
  dplyr::filter(!.data$baseline_scenario %in% hide_vars$hide_baseline_scenario) |>
  dplyr::pull()

  # rename the scenarios to front end appropriate name
  new_choices <- rename_string_vector(possible_baselines, words_class = "scenarios")

  # Update shock_scenario dropdown with unique values from the filtered data
  update_dropdown_input(session, "baseline_scenario", choices = new_choices)
}


update_shock_dropdown <- function(
  session,
  possible_trisk_combinations,
  hide_vars,
  selected_baseline) {

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
}


update_geography_dropdown <- function(
  session,
  possible_trisk_combinations,
  hide_vars,
  selected_baseline,
  selected_shock) {
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

}
