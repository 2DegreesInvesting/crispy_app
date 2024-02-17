run_trisk_and_upload_results_to_db_conn <- function(
    trisk_run_params,
    trisk_input_path,
    postgres_conn) {

  st_results_wrangled_and_checked <- run_trisk_with_params(
    trisk_run_params = trisk_run_params,
    trisk_input_path = trisk_input_path
  )
  if (!is.null(st_results_wrangled_and_checked)) {
    run_id <- upload_to_postgres(
      st_results_wrangled_and_checked = st_results_wrangled_and_checked,
      postgres_conn=postgres_conn
    )
    run_id <- unique(st_results_wrangled_and_checked$run_metadata$run_id)
  } else {
    run_id <- NULL
  }

  return(run_id)
}



# Function to run the trisk model with given parameters and input path
# Returns the wrangled and checked results
run_trisk_with_params <- function(trisk_run_params, trisk_input_path) {
  tryCatch(
    {
      # Run the trisk model with the provided parameters and input path
      # The results are returned and stored in st_results_wrangled_and_checked
      st_results_wrangled_and_checked <- do.call(
        r2dii.climate.stress.test::run_trisk,
        c(
          trisk_run_params,
          list(
            input_path = trisk_input_path,
            output_path = tempdir(),
            return_results = TRUE
          )
        )
      )

      # Extract the run metadata from the crispy_output
      run_metadata <- dplyr::distinct_at(
        st_results_wrangled_and_checked$crispy_output,
        c(names(trisk_run_params), "run_id")
      )

      # Add the run metadata to the results
      st_results_wrangled_and_checked$run_metadata <- run_metadata
    },
    error = function(e) {
      # This block will run if there's an error in the try block

      # Print the error message
      print(e$message)

      # Print the last error and trace using rlang
      print(rlang::last_error())
      print(rlang::last_trace())

      stop("Error running TRISK")
    }
  )

  # Return the results
  return(st_results_wrangled_and_checked)
}


# postgres_conn is a DBI connection
# postgres_conn <- DBI::dbConnect(RPostgres::Postgres(), dbname = dbname, host = host_db, port = db_port, user = db_user, password = db_password)
upload_to_postgres <- function(postgres_conn, st_results_wrangled_and_checked) {
  cat("Uploading results")
  # Iterate over the names of the results
  for (st_output_name in names(st_results_wrangled_and_checked)) {
    # Get the new data from the results
    st_output_df <- st_results_wrangled_and_checked[[st_output_name]]
    if (DBI::dbExistsTable(postgres_conn, st_output_name)) {
      DBI::dbAppendTable(postgres_conn, st_output_name, st_output_df)
    } else {
      DBI::dbCreateTable(postgres_conn, st_output_name, st_output_df)
    }
  }
  DBI::dbDisconnect(postgres_conn)
}

