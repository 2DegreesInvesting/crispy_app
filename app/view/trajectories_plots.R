box::use(
  shiny[moduleServer, NS, req, div, plotOutput, uiOutput, eventReactive, observeEvent, renderUI, renderPlot, reactive],
  semantic.dashboard[box]
)

box::use(
  app / logic / constant[use_ald_sector],
  app / logic / scenario_time_plot[pipeline_scenario_time_plot]
)


####### UI

ui <- function(id) {
  ns <- NS(id)
  list(
    # Third row with 1 taking the entire page width
    box(width = 16, plotOutput(ns("baseline_scenario_plot"), height = "900px")),

    # Fourth row with 2 taking the entire page width
    box(width = 16, plotOutput(ns("shock_scenario_plot"), height = "900px"))
  )
}

####### Server


server <- function(id, trajectories_data_r) {
  moduleServer(id, function(input, output, session) {
    ### BASELINE SCENARIO

    observeEvent(trajectories_data_r(), ignoreInit = TRUE, {
      # Render plot
      scenario_time_plot <- pipeline_scenario_time_plot(trajectories_data_r(),
        y_var = "production_baseline_scenario"
      )
      output$baseline_scenario_plot <- renderPlot({
        scenario_time_plot +
          ggplot2::labs(title = "Production trajectories for the Baseline scenario")
      })
    })


    ### SHOCK SCENARIO

    observeEvent(trajectories_data_r(), ignoreInit = TRUE, {
      # Render plot

      scenario_time_plot <- pipeline_scenario_time_plot(trajectories_data_r(),
        y_var = "production_shock_scenario"
      )
      output$shock_scenario_plot <- renderPlot({
        scenario_time_plot +
          ggplot2::labs(title = "Production trajectories for the Shock scenario")
      })
    })
  })
}
