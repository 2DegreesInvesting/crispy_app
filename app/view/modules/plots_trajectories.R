box::use(
  shiny[moduleServer, NS, req, tagList, div, plotOutput, uiOutput, eventReactive, observeEvent, renderUI, renderPlot, reactive],
  semantic.dashboard[box]
)

box::use(
  app/logic/plots/scenario_time_plot[pipeline_scenario_time_plot]
)


####### UI

ui <- function(id) {
  ns <- NS(id)
  tagList(
    # Third row with 1 taking the entire page width
    box(title = "Baseline Trajectories", width = 16, plotOutput(ns("baseline_scenario_plot"), height = "900px")),

    # Fourth row with 2 taking the entire page width
    box(title = "Shock Trajectories", width = 16, plotOutput(ns("shock_scenario_plot"), height = "900px"))
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
      scenario_time_plot_baseline <- pipeline_scenario_time_plot(trajectories_data_r(),
        y_var = "production_baseline_scenario",
        linecolor = "ald_sector",
        facet_var = granul_top_level
      )

      output$baseline_scenario_plot <- renderPlot({
        scenario_time_plot_baseline +
          ggplot2::labs(title = "Production trajectories for the Baseline scenario")
      })
    })
    observeEvent(trajectories_data_r(), ignoreInit = TRUE, {
      granul_levels <- dplyr::intersect(colnames(trajectories_data_r()), names(max_trisk_granularity))
      granul_top_level <- names(max_trisk_granularity[granul_levels])[which.max(unlist(max_trisk_granularity[granul_levels]))]

      ### SHOCK SCENARIO
      # Render plot
      scenario_time_plot_shock <- pipeline_scenario_time_plot(trajectories_data_r(),
        y_var = "production_shock_scenario",
        linecolor = "ald_sector",
        facet_var = granul_top_level
      )
      output$shock_scenario_plot <- renderPlot({
        scenario_time_plot_shock +
          ggplot2::labs(title = "Production trajectories for the Shock scenario")
      })
    })
  })
}
