# Load required packages
box::use(
  shiny[moduleServer, NS, div, h1, tags, reactiveVal, observeEvent, reactive],
  shiny.semantic[semanticPage],
  semantic.dashboard[dashboardPage, dashboardBody, dashboardSidebar, dashboardHeader],
  shinyjs[useShinyjs]
)

box::use(
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

ui <- function(id) {
  ns <- NS(id)
  semantic.dashboard::box(
    width = 16,
    useShinyjs(), # Initialize shinyjs
    # Custom Semantic UI Modal
    tags$div(
      id = ns("mymodal"),
      class = "ui modal",
      tags$div(class = "header", "Processing"),
      tags$div(
        class = "content",
        tags$p("Please wait...")
      )
    ),
    tags$div(
      class = "ui fluid container",
      # Fomantic UI styled action button with custom class
      tags$button(
        id = ns("run_trisk"),
        class = "ui fluid button", # Add custom class here
        "Run Trisk"
      )
    )
  )
}

####### Server

server <- function(
    id,
    crispy_data_r,
    trisk_granularity_r,
    trisk_run_params_r,
    backend_trisk_run_folder,
    trisk_input_path,
    max_trisk_granularity) {
  moduleServer(id, function(input, output, session) {


    # TRISK COMPUTATION =========================
    run_id_r <- reactiveVal(NULL)

    # fetch or compute trisk on button click
    shiny::observeEvent(input$run_trisk, ignoreNULL = T, {
      shinyjs::runjs(
        paste0(
          "$('#", session$ns("mymodal"), "').modal({closable: true}).modal('show');"
        )
      )

      if (!is.null(trisk_run_params_r())) {
        trisk_run_params <- shiny::reactiveValuesToList(trisk_run_params_r())

        run_id <- get_run_id(
          ns=session$ns,
          trisk_run_params = trisk_run_params,
          backend_trisk_run_folder = backend_trisk_run_folder,
          trisk_input_path = trisk_input_path,
          max_trisk_granularity = max_trisk_granularity
        )
      }

      shinyjs::runjs(
        paste0(
          "$('#", session$ns("mymodal"), "').modal('hide');"
        )
      )
      run_id_r(run_id)
    })

    # load trisk outputs
    trisk_outputs <- fetch_crispy_and_trajectories_data(
      backend_trisk_run_folder = backend_trisk_run_folder,
      run_id_r = run_id_r,
      trisk_granularity_r = trisk_granularity_r
    )

    crispy_data_r <- trisk_outputs$crispy_data_r
    trajectories_data_r <- trisk_outputs$trajectories_data_r


    return(
      list(
        "crispy_data_r" = crispy_data_r,
        "trajectories_data_r" = trajectories_data_r
      )
    )
  })
}






# Function to collect run parameters from the UI,
# and then generate or fetch a trisk run
get_run_id <- function(ns, trisk_run_params,
                       backend_trisk_run_folder,
                       trisk_input_path,
                       max_trisk_granularity) {
  all_input_params_initialized <- !any(sapply(trisk_run_params, function(x) {
    is.null(x)
  }))
  if (all_input_params_initialized) {

    # hardcoded market passthrough value for no carbon tax price model
    if (trisk_run_params$carbon_price_model == "no_carbon_tax") {
      trisk_run_params$market_passthrough <- 0
    }

    run_id <- trisk_generator(
      ns=ns,
      backend_trisk_run_folder = backend_trisk_run_folder,
      trisk_input_path = trisk_input_path,
      trisk_run_params = trisk_run_params,
      max_trisk_granularity = max_trisk_granularity
    )
  } else{
    run_id <- NULL
  }
  return(run_id)
}

# fetch or create a trisk run
trisk_generator <- function(
  ns, 
  backend_trisk_run_folder, 
  trisk_input_path, 
  trisk_run_params, 
  max_trisk_granularity) {

  run_id <- check_if_run_exists(trisk_run_params, backend_trisk_run_folder)

  if (is.null(run_id)) {
    # shinyjs::runjs("$('#mymodal').modal({closable: false}).modal('show');")
    st_results_wrangled_and_checked <- tryCatch(
      {
        run_trisk_with_params(
          trisk_run_params,
          trisk_input_path
        )
      },
      error = function(e) {
        print(e$message)
        print("\n\n")
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
    # shinyjs::runjs("$('#mymodal').modal('hide');")
  }

  run_id <- check_if_run_exists(trisk_run_params, backend_trisk_run_folder)
  
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
