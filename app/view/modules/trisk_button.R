# Load required packages
box::use(
  shiny[moduleServer, NS, div, h1, tags, reactiveVal, observeEvent, reactive],
  shiny.semantic[semanticPage],
  semantic.dashboard[dashboardPage, dashboardBody, dashboardSidebar, dashboardHeader],
  shinyjs[useShinyjs]
)

box::use(
  app / logic / trisk_button_logic[
    trisk_generator  
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
      # open the model dialog
      shinyjs::runjs(
        paste0(
          "$('#", session$ns("mymodal"), "').modal({closable: true}).modal('show');"
        )
      )

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

          # get run_id either by running locally, or by fetching from backend
          run_id <- trisk_generator(
            backend_trisk_run_folder = backend_trisk_run_folder,
            trisk_input_path = trisk_input_path,
            trisk_run_params = trisk_run_params,
            max_trisk_granularity = max_trisk_granularity
          )
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

    # load trisk outputs either from local storage, or cloud backend
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
        load_backend_crispy_data(backend_trisk_run_folder, run_id == run_id_r()) |>
          stress.test.plot.report::main_load_multi_crispy_data(granularity = trisk_granularity_r())
      )

      trajectories_data_r(
        load_backend_trajectories_data(backend_trisk_run_folder, run_id == run_id_r()) |>
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
