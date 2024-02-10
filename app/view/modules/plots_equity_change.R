box::use(
  shiny[moduleServer, NS, plotOutput, renderPlot, observeEvent, fluidRow],
  semantic.dashboard[column, box]
)

box::use(
  app / logic / plots / exposure_change_plot[pipeline_exposure_change_plot],
  app / logic / plots / crispy_npv_change_plot[pipeline_crispy_npv_change_plot]
)


####### UI

ui <- function(id) {
  ns <- NS(id)
  fluidRow(
    box(title = "NPV Change", width = 8, plotOutput(ns("crispy_npv_change_plot"))),
    box(title = "Exposure Change", width = 8, plotOutput(ns("exposure_change_plot")))
  )
}

####### Server



server <- function(id, analysis_data_r, max_trisk_granularity) {
  moduleServer(id, function(input, output, session) {
    observeEvent(analysis_data_r(), {
      granul_levels <- dplyr::intersect(colnames(analysis_data_r()), names(max_trisk_granularity))
      granul_top_level <- names(max_trisk_granularity[granul_levels])[which.max(unlist(max_trisk_granularity[granul_levels]))]

      exposure_change_plot <- pipeline_exposure_change_plot(analysis_data_r(), x_var = granul_top_level)
      output$exposure_change_plot <- renderPlot({
        exposure_change_plot
      })

      crispy_npv_change_plot <- pipeline_crispy_npv_change_plot(analysis_data_r(), x_var = granul_top_level)
      output$crispy_npv_change_plot <- renderPlot({
        crispy_npv_change_plot
      })
    })
  })
}
