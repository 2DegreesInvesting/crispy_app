# This file contains the main code for the CRISPY Shiny application. It defines the UI and server functions.

# Load required packages
box::use(
  shiny[moduleServer, NS, renderUI, tags, uiOutput, observe, observeEvent, div, a, reactiveVal, p, eventReactive],
  shiny.semantic[semanticPage, segment, slider_input, card],
  semantic.dashboard[dashboardPage, dashboardHeader, dashboardSidebar, dashboardBody, icon, box],
)

# Load required modules and logic files
box::use(
  app/view/trisk_generator,
  app/view/portfolio_creator,
  app/view/equity_change_plots,
  app/view/trajectories_plots,
  app/logic/data_load[
    load_backend_trajectories_data,
    load_backend_crispy_data
  ],
  app/logic/renamings[rename_string_vector],
  app/logic/constant[
    trisk_input_path,
    backend_trisk_run_folder,
    max_trisk_granularity,
    available_vars,
    hide_vars,
    use_ald_sector
  ]
)

# Define the UI function
#' @export
ui <- function(id) {
  ns <- NS(id)
  dashboardPage(
    title = "CRISPY",
    dashboardHeader(title = "Crispy Equities"),
    dashboardSidebar(
      div(
      shiny.semantic::segment(
                  div(class="content",
         div(class="header", "Granularity")
        ),
        div(class="content",
          slider_input(
            ns("granularity_switch"),
            custom_ticks = rename_string_vector(names(max_trisk_granularity), words_class = "analysis_columns"),
            value = rename_string_vector(names(which(max_trisk_granularity == 1)), words_class = "analysis_columns")
          ))
        ),
        trisk_generator$ui(ns("trisk_generator"), available_vars),
        shiny::img(
          src = "static/logo_1in1000.png",
          height = "20%", width = "auto",
          style = "
      display: block;
      margin-left: auto;
      margin-right: auto;
      margin-top: 10px;
      margin-bottom: 10px;"
        ))
      ,
      size = "wide"
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

# Define the server function
#' @export
server <- function(id) {
  moduleServer(id, function(input, output, session) {
    # get granularity columns
    trisk_granularity_r <- eventReactive(input$granularity_switch, ignoreNULL = TRUE, {
      granularity_picked <- input$granularity_switch |>
        rename_string_vector(words_class = "analysis_columns", dev_to_ux = FALSE)

      granularity_level <- max_trisk_granularity[granularity_picked]
      # Filter names based on values <= given_integer
      granularity_columns <- names(max_trisk_granularity)[sapply(max_trisk_granularity, function(value) value <= granularity_level)]

      return(granularity_columns)
    })


    # This section of code generates TRISK outputs and consumes them for analysis and visualization.

    # Generate TRISK outputs
    run_id_r <- trisk_generator$server(
      "trisk_generator",
      backend_trisk_run_folder = backend_trisk_run_folder,
      trisk_input_path = trisk_input_path,
      available_vars = available_vars,
      hide_vars = hide_vars,
      max_trisk_granularity = max_trisk_granularity,
      use_ald_sector = use_ald_sector
    )

    # Connect to the data sources, filter run perimter, and process to the appropriate granularity
    crispy_data_r <- reactiveVal()
    trajectories_data_r <- reactiveVal()

    observeEvent(c(run_id_r(), trisk_granularity_r()), ignoreInit = TRUE, {
      if (!is.null(run_id_r())){
      crispy_data_r(
        load_backend_crispy_data(backend_trisk_run_folder) |>
          dplyr::filter(.data$run_id == run_id_r()) |>
          stress.test.plot.report::main_load_multi_crispy_data(granularity = trisk_granularity_r())
      )

      trajectories_data_r(
        load_backend_trajectories_data(backend_trisk_run_folder) |>
          dplyr::filter(.data$run_id == run_id_r()) |>
          stress.test.plot.report::main_data_load_trajectories_data(granularity = trisk_granularity_r())
      )
    }}
    )

    # Manages the porfolio creator module
    # Create analysis data by merging crispy to portfolio
    analysis_data_r <- portfolio_creator$server(
      "portfolio_creator", crispy_data_r, trisk_granularity_r,
      max_trisk_granularity
    )

    # Consume TRISK outputs

    # Generate equity change plots
    equity_change_plots$server("equity_change_plots", analysis_data_r, max_trisk_granularity)

    # Generate trajectories plots
    trajectories_plots$server("trajectories_plots", trajectories_data_r, max_trisk_granularity)
  })
}
