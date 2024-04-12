box::use(
  shiny[moduleServer, NS, plotOutput, renderPlot, observeEvent, tags]
)


####### UI

ui <- function(id) {
  ns <- NS(id)
  shiny::fluidRow(
    semantic.dashboard::box(
      title = "PD Difference", width = 8, collapsible = FALSE,
      plotOutput(ns("pd_term_plot_output"), height = "100%")
    ),
    semantic.dashboard::box(
      title = "Exposure Change", width = 8, collapsible = FALSE,
      plotOutput(ns("expected_loss_plot_output"), height = "100%")
    )
  )
}

####### Server



server <- function(id, analysis_data_r, crispy_data_r, max_trisk_granularity) {
  moduleServer(id, function(input, output, session) {
    base_height_per_facet <- 150 # height in pixels # TODO GO IN CONF

    # PD PLOT

    observeEvent(c(crispy_data_r(), analysis_data_r()), {
      if ((nrow(crispy_data_r()) > 0) & (nrow(analysis_data_r()) > 0)) {
          granul_levels <- dplyr::intersect(colnames(analysis_data_r()), names(max_trisk_granularity))
          granul_top_level <- names(max_trisk_granularity[granul_levels])[which.max(unlist(max_trisk_granularity[granul_levels]))]

          num_facets <- length(unique(analysis_data_r()[[granul_top_level]]))

          pd_term_plot <- stress.test.plot.report::pipeline_crispy_pd_term_plot(
            crispy_data_agg = analysis_data_r(),
            facet_var = granul_top_level
          )
          # id value dynamically generated in the server, just above
          output$pd_term_plot_output <- shiny::renderPlot(
            {
              pd_term_plot
            },
            height = num_facets * base_height_per_facet
          )
      } else {
        output$pd_term_plot_output <- shiny::renderPlot(ggplot2::ggplot()+ggplot2::theme_void(), width=1, height=1)
      }
    })

    # EXPECTED LOSS PLOT

    observeEvent(analysis_data_r(), {
      if (nrow(analysis_data_r()) > 0) {
        # Then, prepare and render the plot
        granul_levels <- dplyr::intersect(colnames(analysis_data_r()), names(max_trisk_granularity))
        granul_top_level <- names(max_trisk_granularity[granul_levels])[which.max(unlist(max_trisk_granularity[granul_levels]))]

        analysis_data_all_granul_levels <- analysis_data_r() |>
          dplyr::right_join(crispy_data_r() |> dplyr::distinct_at(granul_top_level))

        num_facets <- length(unique(analysis_data_all_granul_levels[[granul_top_level]]))

        expected_loss_plot <- stress.test.plot.report::pipeline_crispy_expected_loss_plot(
          analysis_data = analysis_data_all_granul_levels,
          facet_var = granul_top_level
        )

        output$expected_loss_plot_output <- shiny::renderPlot(
          {
            expected_loss_plot
          },
          height = num_facets * base_height_per_facet
        )
      } else {
        output$expected_loss_plot_output <- shiny::renderPlot(ggplot2::ggplot()+ggplot2::theme_void(), width=1, height=1)
      }
    })
  })
}
