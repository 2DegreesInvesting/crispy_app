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
    selected_baseline_r <- reactive({
      choice_r <- get_dropdown_choice(
        id="baseline_scenario", input=input, session=session)
      renamed_choice <- rename_string_vector(choice_r(), words_class = "scenarios", dev_to_ux = FALSE)
      return(renamed_choice)
    })
    selected_shock_r <- reactive({
      choice_r <- get_dropdown_choice(id="shock_scenario", input=input, session=session)
      renamed_choice <- rename_string_vector(choice_r(), words_class = "scenarios", dev_to_ux = FALSE)
      return(renamed_choice)
    })
    selected_geography_r <- reactive({
      choice_r <- get_dropdown_choice(id="scenario_geography", input=input, session=session)
      clean_choice <- ifelse(is.null(choice_r()), "", choice_r())
      return(clean_choice)
    })

    # Observe changes in possible_trisk_combinations and update baseline_scenario dropdown

    update_baseline_dropdown(
      session = session,
      possible_trisk_combinations = possible_trisk_combinations,
      hide_vars = hide_vars
    )


    # Observe changes in baseline_scenario dropdown and update shock_scenario dropdown

    update_shock_dropdown(
      session = session,
      possible_trisk_combinations = possible_trisk_combinations,
      hide_vars = hide_vars,
      selected_baseline_r = selected_baseline_r
    )

    # Observe changes in both baseline_scenario and shock_scenario dropdowns to update scenario_geography dropdown

    update_geography_dropdown(
      session = session,
      possible_trisk_combinations = possible_trisk_combinations,
      hide_vars = hide_vars,
      selected_baseline_r = selected_baseline_r,
      selected_shock_r = selected_shock_r
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

# MODULES =========================

create_dropdown_input <- function(id) {
  ns <- NS(id)
  browser()
  tags$div(
    # this javascript udpates the dropdown with the new choices
    tags$head(tags$script(HTML(paste0("
      $(document).ready(function() {
        var allChoices = [];

        function initializeDropdown(choices) {
          $('#", ns('choices_dropdown'), "').dropdown({
            values: choices,
            forceSelection: false,
            onLabelCreate: function(value, text) {
              return $(this);
            }
          });
        }
      initializeDropdown(allChoices); // Initial dropdown initialization

        Shiny.addCustomMessageHandler('", ns("updateDropdown"), "', function(newChoices) {
          // Format the choices as required by the dropdown
          var formattedChoices = newChoices.choices.map(function(choice) {
            return { name: choice, value: choice };
          });
          // Directly update dropdown values
          initializeDropdown(formattedChoices);
        });
      });
    ")))),
    # selection dropdown
    class = "ui fluid search selection dropdown", id = ns("choices_dropdown"),
    tags$i(class = "dropdown icon"),
    tags$div(class="input", type = "hidden", id = ns("dropdown_choice")), # This will hold the selected value
    div(class = "default text", ""),
        tags$div(class = "menu")

  )
  
}

update_dropdown_input <- function(session, id, choices) {

 session$sendCustomMessage(
    session$ns(paste0(id, "-updateDropdown")), 
    list(id = session$ns(paste0(id, "-choices_dropdown")), choices = choices)
    )

}

get_dropdown_choice <- function(id, input, session) {
  return(reactive({
    browser()
    pick_id <- session$ns(paste0(id, "-dropdown_choice"))
    input[[pick_id]]
  }))
}


update_baseline_dropdown <- function(
    session,
    possible_trisk_combinations,
    hide_vars) {
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
}


update_shock_dropdown <- function(
    session,
    possible_trisk_combinations,
    hide_vars,
    selected_baseline_r) {
  observeEvent(selected_baseline_r(), ignoreInit = TRUE, {
    selected_baseline <- rename_string_vector(selected_baseline_r(), words_class = "scenarios", dev_to_ux = FALSE)
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
}


update_geography_dropdown <- function(
    session,
    possible_trisk_combinations,
    hide_vars,
    selected_baseline_r,
    selected_shock_r) {
  observeEvent(c(selected_baseline_r(), selected_shock_r()), ignoreInit = TRUE, {
    selected_baseline <- rename_string_vector(selected_baseline_r(), words_class = "scenarios", dev_to_ux = FALSE)
    selected_shock <- rename_string_vector(selected_shock_r(), words_class = "scenarios", dev_to_ux = FALSE)

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
