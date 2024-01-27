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
    box(width = 8, plotOutput(ns("crispy_npv_change_plot"))),
    box(width = 8, plotOutput(ns("exposure_change_plot")))
  )
}

####### Server



server <- function(id, analysis_data_r) {
  moduleServer(id, function(input, output, session) {
    observeEvent(analysis_data_r(), {
      
      exposure_change_plot <- pipeline_exposure_change_plot(analysis_data_r())
      output$exposure_change_plot <- renderPlot({
        exposure_change_plot
      })

      crispy_npv_change_plot <- pipeline_crispy_npv_change_plot(analysis_data_r())
      output$crispy_npv_change_plot <- renderPlot({
        crispy_npv_change_plot
      })
    })
  })
}
