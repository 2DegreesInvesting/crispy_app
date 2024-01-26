box::use(
  shiny[moduleServer, NS, renderUI, tags, uiOutput, reactive, observeEvent, div, a],
  shiny.semantic[semanticPage, segment],
  semantic.dashboard[dashboardPage, dashboardHeader, dashboardSidebar, dashboardBody, icon, box],
)
box::use(
  app / view / trisk_generator,
  app / view / portfolio_creator,
  app / view / equity_change_plots,
  app / view / trajectories_plots,
  app / logic / data_load[
    load_backend_trajectories_data,
    load_backend_crispy_data
  ],
  app / logic / constant[
    max_trisk_granularity,
    portfolio_crispy_merge_cols,
    available_vars,
    hide_vars,
    use_ald_sector
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
    dashboardHeader(title = "Crispy Equities"),
    dashboardSidebar(
      trisk_generator$ui(ns("trisk_generator"), available_vars),
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
    trisk_input_path <- Sys.getenv("TRISK_INPUT_PATH")
    backend_trisk_run_folder <- Sys.getenv("BACKEND_TRISK_RUN_FOLDER")

    run_id_r <- trisk_generator$server(
      "trisk_generator",
      backend_trisk_run_folder,
      trisk_input_path,
      available_vars,
      hide_vars,
      max_trisk_granularity,
      use_ald_sector
    )

    crispy_data_r <- reactive({
      if (!is.null(run_id_r())) {
        load_backend_crispy_data(backend_trisk_run_folder, max_trisk_granularity) |> 
          dplyr::filter(.data$run_id == run_id_r()) |>
          stress.test.plot.report::aggregate_crispy_facts(group_cols = max_trisk_granularity)

      }
    })

    trajectories_data_r <- reactive({
      if (!is.null(run_id_r())) {
        load_backend_trajectories_data(backend_trisk_run_folder) |> dplyr::filter(run_id == run_id_r()) |>
        stress.test.plot.report::aggregate_trajectories_facts(group_cols = max_trisk_granularity) |>
        stress.test.plot.report::convert_trajectories_as_percentages(group_cols = max_trisk_granularity) |>

      }
    })

    analysis_data_r <- portfolio_creator$server(
      "portfolio_creator", crispy_data_r, max_trisk_granularity,
      portfolio_crispy_merge_cols
    )

    equity_change_plots$server("equity_change_plots", analysis_data_r)#, portfolio_crispy_merge_cols)
    trajectories_plots$server("trajectories_plots", trajectories_data_r, portfolio_crispy_merge_cols)
  })
}
