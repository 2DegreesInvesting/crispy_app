run_trisk_and_upload_results <- function(
    trisk_run_params,
    trisk_input_path) {

  dbname <- Sys.getenv("ST_POSTGRES_DB")
  host_db <- Sys.getenv("ST_POSTGRES_HOST")
  db_port <- Sys.getenv("ST_POSTGRES_PORT")
  db_user <- Sys.getenv("ST_POSTGRES_USERNAME")
  db_password <- Sys.getenv("ST_POSTGRES_PASSWORD")


  trisk_run_params <- as.list(trisk_run_params)
  trisk_input_path <- as.character(trisk_input_path)

  st_results_wrangled_and_checked <- run_trisk_with_params(
    trisk_run_params = trisk_run_params,
    trisk_input_path = trisk_input_path
  )
  if (!is.null(st_results_wrangled_and_checked)) {
    run_id <- upload_to_postgres(
      st_results_wrangled_and_checked = st_results_wrangled_and_checked,
      dbname = dbname,
      host_db = host_db,
      db_port = db_port,
      db_user = db_user,
      db_password = db_password
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



upload_to_postgres <- function(st_results_wrangled_and_checked, dbname, host_db, db_port, db_user, db_password) {
  conn <- DBI::dbConnect(RPostgres::Postgres(), dbname = dbname, host = host_db, port = db_port, user = db_user, password = db_password)

  # Iterate over the names of the results
  for (st_output_name in names(st_results_wrangled_and_checked)) {
    # Get the new data from the results
    st_output_df <- st_results_wrangled_and_checked[[st_output_name]]
    if (DBI::dbExistsTable(conn, st_output_name)) {
      DBI::dbAppendTable(conn, st_output_name, st_output_df)
    } else {
      DBI::dbCreateTable(conn, st_output_name, st_output_df)
    }
  }
  DBI::dbDisconnect(conn)
}

