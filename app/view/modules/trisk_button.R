# Load required packages
box::use(
  shiny[moduleServer, NS, div, h1, tags, reactiveVal, observeEvent, reactive, observe],
  shiny.semantic[semanticPage],
  semantic.dashboard[dashboardPage, dashboardBody, dashboardSidebar, dashboardHeader],
  shinyjs[useShinyjs]
)

box::use(
  app/logic/trisk_button_logic[
    trisk_generator,
    check_if_run_exists
  ],
  app/logic/data_load[
    load_backend_crispy_data,
    load_backend_trajectories_data
  ]
)



####### UI

ui <- function(id) {
  ns <- NS(id)
  tags$div(
    useShinyjs(), # Initialize shinyjs
    # Custom Semantic UI Modal
    tags$div(
      id = ns("mymodal"),
      class = "ui modal",
      tags$div(class = "header", "Processing"),
      tags$div(
        class = "content",
        tags$p("Please wait while the model is being run with the chosen parameters. This may take up to 10 minutes.")
      )
    ),
    tags$div(
      id = ns("model_load_db"),
      class = "ui modal",
      tags$div(class = "header", "Fetching precomputed results from database..."),
      tags$div(
        class = "content",
        tags$p("This dialog should close automatically when the data is loaded. Click outside of it to close manually.")
      )
    ),
 
    
    tags$button(
      id = ns("run_trisk"),
      class = "ui fluid button ", # Added custom class for styling
      "Run Trisk (click again when switching tabs to refresh data)"
    )
  
  )
}

####### Server

server <- function(
    id,
    trisk_run_params_r,
    trisk_granularity_r,
    backend_trisk_run_folder,
    trisk_input_path,
    max_trisk_granularity) {
  moduleServer(id, function(input, output, session) {
    # TRISK COMPUTATION =========================
    run_id_r <- reactiveVal(NULL)

    # fetch or compute trisk on button click
    shiny::observeEvent(input$run_trisk, ignoreNULL = T, {
      if (!is.null(trisk_run_params_r())) {
        trisk_run_params <- shiny::reactiveValuesToList(trisk_run_params_r())


        all_input_params_initialized <- !any(sapply(trisk_run_params, function(x) {
          is.null(x)
        }))
        if (all_input_params_initialized) {
          # hardcoded market passthrough value for no carbon tax price model
          if (trisk_run_params$carbon_price_model == "no_carbon_tax") {
            trisk_run_params$market_passthrough <- 0
          }
          # Check if the run already exists (locally OR in database)
          run_id <- check_if_run_exists(trisk_run_params, backend_trisk_run_folder)

          if (is.null(run_id)) {
            # open the model dialog
            shinyjs::runjs(
              paste0(
                "$('#", session$ns("mymodal"), "').modal({closable: true}).modal('show');"
              )
            )

            # get run_id either by running locally, or by fetching from backend
            run_id <- trisk_generator(
              backend_trisk_run_folder = backend_trisk_run_folder,
              trisk_input_path = trisk_input_path,
              trisk_run_params = trisk_run_params,
              max_trisk_granularity = max_trisk_granularity
            )
          }
        }
      }

      # close the modal dialog
      shinyjs::runjs(
        paste0(
          "$('#", session$ns("mymodal"), "').modal('hide');"
        )
      )

      run_id_r(run_id)
    })

    # redudant reactive allowing to re-trigger trisk_outputs on run_id_r change
    run_id_rr <- reactive({
      input$run_trisk
      run_id_r()
    })

    # load trisk outputs either from local storage, or cloud backend
    trisk_outputs <- fetch_crispy_and_trajectories_data(
      session = session,
      backend_trisk_run_folder = backend_trisk_run_folder,
      run_id_r = run_id_rr,
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


fetch_crispy_and_trajectories_data <- function(session, backend_trisk_run_folder,
                                               run_id_r,
                                               trisk_granularity_r) {
  # FETCH CRISPY AND TRAJECTORIES DATA =========================

  # Connect to the data sources, filter run perimter, and process to the appropriate granularity
  raw_crispy_data_r <- reactiveVal()
  raw_trajectories_data_r <- reactiveVal()

  observe({
    if (!is.null(run_id_r())) {
      shinyjs::runjs(
        paste0(
          "$('#", session$ns("model_load_db"), "').modal({closable: true}).modal('show');"
        )
      )

      raw_crispy_data_r(
        load_backend_crispy_data(backend_trisk_run_folder, run_id = run_id_r())
      )
      raw_trajectories_data_r(
        load_backend_trajectories_data(backend_trisk_run_folder, run_id = run_id_r())
      )

      # close the modal dialog
      shinyjs::runjs(
        paste0(
          "$('#", session$ns("model_load_db"), "').modal('hide');"
        )
      )
    }
  })

  # preprocess the raw data to the appropriate granularity

  crispy_data_r <- reactiveVal()
  trajectories_data_r <- reactiveVal()

  observe({
    if (!is.null(trisk_granularity_r()) & !is.null(raw_crispy_data_r()) & !is.null(raw_trajectories_data_r())) {
      crispy_data_r(
        raw_crispy_data_r() |>
          stress.test.plot.report::main_load_multi_crispy_data(granularity = trisk_granularity_r())
      )
      trajectories_data_r(
        raw_trajectories_data_r() |>
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
