box::use(
  shiny[moduleServer, NS, renderUI, tags, uiOutput, eventReactive, observeEvent, div, a],
  shiny.semantic[semanticPage, segment],
  semantic.dashboard[dashboardPage, dashboardHeader, dashboardSidebar, dashboardBody, icon, box],
)
box::use(
  app / view / params_picker,
  app / view / portfolio_creator,
  app / view / equity_change_plots,
  app / view / trajectories_plots,
  app / logic / data_load[
    load_backend_crispy_data,
    load_backend_trajectories_data,
    load_backend_trisk_run_data
  ]
)

#######
####### UI
#######


#' @export
ui <- function(id) {
  ns <- NS(id)
  dashboardPage(
    title = "CRISPY",
    dashboardHeader(title = "Crispy app"),
    dashboardSidebar(
      params_picker$ui(ns("params_picker")),
      size = "very wide"
    ),
    dashboardBody(
      # First row with the left (1/3 width) the right (2/3 width)
      semanticPage(
        portfolio_creator$ui(ns("portfolio_creator")),
        equity_change_plots$ui(ns("equity_change_plots")),
        trajectories_plots$ui(ns("trajectories_plots"))
      )
    )
  )
}


#######
####### Server
#######

#' @export
server <- function(id) {
  moduleServer(id, function(input, output, session) {
    run_id_r <- params_picker$server("params_picker", backend_trisk_run_data)

    backend_crispy_data <- load_backend_crispy_data()
    backend_trajectories_data <- load_backend_trajectories_data()

    crispy_data_r <- eventReactive(run_id_r(), ignoreInit = TRUE, {
      if (length(run_id_r()) > 0) {
        backend_crispy_data |>
          dplyr::filter(.data$run_id == run_id_r())
      }
    })
    trajectories_data_r <- eventReactive(run_id_r(), ignoreInit = TRUE, {
      if (length(run_id_r()) > 0) {
        backend_trajectories_data |>
          dplyr::filter(.data$run_id == run_id_r())
      }
    })

    analysis_data_r <- portfolio_creator$server("portfolio_creator", crispy_data_r)

    equity_change_plots$server("equity_change_plots", analysis_data_r)
    trajectories_plots$server("trajectories_plots", trajectories_data_r)
  })
}
