# This file contains the main code for the CRISPY Shiny application. It defines the UI and server functions.

# Load required packages
box::use(
  shiny[moduleServer, NS, tags, HTML, reactiveVal, observeEvent],
  shiny.semantic[semanticPage, segment, slider_input, card],
  semantic.dashboard[dashboardPage, dashboardBody, icon, box],
  shinyjs[useShinyjs]
)

box::use(
  # modules
  app / view / modules / portfolio_mgmt,
  app / view / modules / equity_change_plots,
  app / view / modules / trajectories_plots,
  # logic
  app / logic / trisk_mgmt[
    run_trisk_with_params,
    append_st_results_to_backend_data,
    check_if_run_exists,
    get_run_data_from_run_id,
    format_error_message
  ],
  app / logic / data_load[
    load_backend_trajectories_data,
    load_backend_crispy_data
  ]
)

####### UI

ui <- function(id, max_trisk_granularity, available_vars) {
  ns <- NS(id)
  
  # dashboardBody
  dashboardBody(
      useShinyjs(), # Initialize shinyjs
      # Custom Semantic UI Modal
      tags$div(
        id = "mymodal",
        class = "ui modal",
        tags$div(class = "header", "Processing"),
        tags$div(
          class = "content",
          tags$p("Please wait...")
        )
      ),
  card(
  # Fomantic UI styled action button with custom class
  tags$button(
    id = ns("run_trisk"),
    class = "ui button flat-edgy-btn",  # Add custom class here
    "Run Trisk"
  )),
  
      portfolio_mgmt$ui(
        ns("portfolio_equities"),
        title = "Equities portfolio"
      ),
      equity_change_plots$ui(ns("equity_change_plots")),
      trajectories_plots$ui(ns("trajectories_plots"))
      
)
    
  
}

####### Server

server <- function(id, perimeter, backend_trisk_run_folder, trisk_input_path, max_trisk_granularity) {
  moduleServer(id, function(input, output, session) {
shiny::observeEvent(input$run_trisk, ignoreInit = TRUE,{
    browser()
    # SELECT PARAMETERS =========================
    trisk_granularity_r <- perimeter$trisk_granularity_r
trisk_run_params_r <- perimeter$trisk_run_params_r
    # TRISK COMPUTATION =========================

    # fetch or compute trisk
    run_id_r <- get_run_id(
      trisk_run_params_r = trisk_run_params_r,
      backend_trisk_run_folder = backend_trisk_run_folder,
      trisk_input_path = trisk_input_path,
      max_trisk_granularity = max_trisk_granularity
    )

    # load trisk outputs
    trisk_outputs <- fetch_crispy_and_trajectories_data(
      backend_trisk_run_folder = backend_trisk_run_folder,
      run_id_r = run_id_r,
      trisk_granularity_r = trisk_granularity_r
    )

    crispy_data_r <- trisk_outputs$crispy_data_r
    trajectories_data_r <- trisk_outputs$trajectories_data_r

    # MANAGE PORTFOLIO =========================

    display_columns_equities <- c(
      names(max_trisk_granularity),
      "exposure_value_usd",
      "crispy_perc_value_change",
      "crispy_value_loss"
    )


    
        # Manages the porfolio creator module
    # Create analysis data by merging crispy to portfolio, and aggrgating to the appropriate granularity
    analysis_data_r <- portfolio_mgmt$server(
      "portfolio_equities",
      crispy_data_r = crispy_data_r,
      trisk_granularity_r = trisk_granularity_r,
      max_trisk_granularity = max_trisk_granularity,
      display_columns = display_columns_equities,
      editable_columns_names = c("exposure_value_usd"),
      colored_columns_names = c("crispy_perc_value_change", "crispy_value_loss")
    )

    # CONSUME TRISK OUTPUTS =========================

    # Generate equity change plots
    equity_change_plots$server(
      "equity_change_plots",
      analysis_data_r = analysis_data_r,
      max_trisk_granularity = max_trisk_granularity
    )

    # Generate trajectories plots
    trajectories_plots$server(
      "trajectories_plots",
      trajectories_data_r = trajectories_data_r,
      max_trisk_granularity = max_trisk_granularity
    )

    })
      })
}



# Function to collect run parameters from the UI,
# and then generate or fetch a trisk run
get_run_id <- function(trisk_run_params_r,
                       backend_trisk_run_folder,
                       trisk_input_path,
                       max_trisk_granularity) {
  run_id_r <- reactiveVal(NULL)

  # Search for existing run, if not, run TRISK
  observeEvent(trisk_run_params_r(), {
    trisk_run_params <- shiny::reactiveValuesToList(trisk_run_params_r())


    all_input_params_initialized <- !any(sapply(trisk_run_params, function(x) {
      is.null(x)
    }))

    if (all_input_params_initialized) {
      if (trisk_run_params$carbon_price_model == "no_carbon_tax") {
        trisk_run_params$market_passthrough <- 0
      }

      run_id <- trisk_generator(
        backend_trisk_run_folder = backend_trisk_run_folder,
        trisk_input_path = trisk_input_path,
        trisk_run_params = trisk_run_params,
        max_trisk_granularity = max_trisk_granularity
      )
      run_id_r(run_id)
    }
  })
  return(run_id_r)
}

# fetch or create a trisk run
trisk_generator <- function(backend_trisk_run_folder, trisk_input_path, trisk_run_params, max_trisk_granularity) {
  run_id <- check_if_run_exists(trisk_run_params, backend_trisk_run_folder)

  if (is.null(run_id)) {
    shinyjs::runjs("$('#mymodal').modal({closable: false}).modal('show');")
    st_results_wrangled_and_checked <- tryCatch(
      {
        run_trisk_with_params(
          trisk_run_params,
          trisk_input_path
        )
      },
      error = function(e) {
        cat(e$message)
        format_error_message(trisk_run_params)
        NULL
      }
    )

    if (!is.null(st_results_wrangled_and_checked)) {
      # Close the modal dialog and re-enable UI
      append_st_results_to_backend_data(
        st_results_wrangled_and_checked,
        backend_trisk_run_folder,
        max_trisk_granularity
      )
    }
    shinyjs::runjs("$('#mymodal').modal('hide');")
    run_id <- check_if_run_exists(trisk_run_params, backend_trisk_run_folder)
  }


  return(run_id)
}



fetch_crispy_and_trajectories_data <- function(backend_trisk_run_folder,
                                               run_id_r,
                                               trisk_granularity_r) {
  # FETCH CRISPY AND TRAJECTORIES DATA =========================

  # Connect to the data sources, filter run perimter, and process to the appropriate granularity
  crispy_data_r <- reactiveVal()
  trajectories_data_r <- reactiveVal()

  observeEvent(c(run_id_r(), trisk_granularity_r()), ignoreInit = TRUE, {
    if (!is.null(run_id_r())) {
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
    }
  })

  trisk_outputs <- list(
    "crispy_data_r" = crispy_data_r,
    "trajectories_data_r" = trajectories_data_r
  )

  return(trisk_outputs)
}
