box::use(
  shiny[moduleServer, NS, req, div, plotOutput, uiOutput, eventReactive, observeEvent, renderUI, renderPlot, reactive],
  semantic.dashboard[box]
)

box::use(
  app/logic/plots/scenario_time_plot[pipeline_scenario_time_plot]
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


server <- function(id, trajectories_data_r, max_trisk_granularity) {
  moduleServer(id, function(input, output, session) {
    observeEvent(trajectories_data_r(), ignoreInit = TRUE, {
      granul_levels <- dplyr::intersect(colnames(trajectories_data_r()), names(max_trisk_granularity))
      granul_top_level <- names(max_trisk_granularity[granul_levels])[which.max(unlist(max_trisk_granularity[granul_levels]))]


      ### BASELINE SCENARIO

      # Render plot
      scenario_time_plot <- pipeline_scenario_time_plot(trajectories_data_r(),
        y_var = "production_baseline_scenario",
        linecolor = "ald_sector",
        facet_var = granul_top_level
      )

      output$baseline_scenario_plot <- renderPlot({
        scenario_time_plot +
          ggplot2::labs(title = "Production trajectories for the Baseline scenario")
      })


      ### SHOCK SCENARIO
      # Render plot
      scenario_time_plot <- pipeline_scenario_time_plot(trajectories_data_r(),
        y_var = "production_shock_scenario",
        linecolor = "ald_sector",
        facet_var = granul_top_level
      )
      output$shock_scenario_plot <- renderPlot({
        scenario_time_plot +
          ggplot2::labs(title = "Production trajectories for the Shock scenario")
      })
    })
  })
}
