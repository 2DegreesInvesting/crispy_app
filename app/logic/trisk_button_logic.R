box::use(
  app/logic/trisk_mgmt[
    run_trisk_with_params,
    format_error_message
  ],
  app/logic/cloud_logic[
    trigger_trisk_api_computation,
  ],
  app/logic/data_load[
    load_backend_trisk_run_metadata
  ],
  app/logic/data_write[
    append_st_results_to_backend_data
  ]
)


# Function to check if a run exists based on the given trisk run parameters
# Returns the run_id if the run exists, NULL otherwise
check_if_run_exists <- function(trisk_run_params, backend_trisk_run_folder) {
  # Load the backend trisk run metadata
  backend_trisk_run_metadata <- load_backend_trisk_run_metadata(backend_trisk_run_folder)

  # Filter the metadata based on the provided trisk run parameters
  df <- backend_trisk_run_metadata
  for (trisk_param in names(trisk_run_params)) {
    df <- df |> dplyr::filter(!!rlang::sym(trisk_param) == trisk_run_params[[trisk_param]])
  }

  # If a single run is found, return its run_id, otherwise return NULL or throw an error
  if (nrow(df) == 1) {
    run_id <- df |> dplyr::pull(run_id)
  } else if (nrow(df) == 0) {
    run_id <- NULL
  } else {
    stop("More than 1 run have been found with the provided trisk input parameters")
  }

  # Return the run_id
  return(run_id)
}



# fetch or create a trisk run
trisk_generator <- function(
    backend_trisk_run_folder,
    trisk_input_path,
    trisk_run_params,
    max_trisk_granularity) {
  if (Sys.getenv("CRISPY_APP_ENV") == "dev") {
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
      append_st_results_to_backend_data(
        st_results_wrangled_and_checked,
        backend_trisk_run_folder,
        max_trisk_granularity
      )
      run_id <- check_if_run_exists(trisk_run_params, backend_trisk_run_folder)
    } else {
      run_id <- NULL
    }
  } else if (Sys.getenv("CRISPY_APP_ENV") == "prod") {
    run_id <- trigger_trisk_api_computation(trisk_run_params)
  } else {
    stop("must set environment variable CRISPY_APP_ENV to 'dev' or 'prod'")
  }

  return(run_id)
}
