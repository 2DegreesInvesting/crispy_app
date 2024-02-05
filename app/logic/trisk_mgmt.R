# Import necessary libraries and functions
box::use(
  arrow[read_parquet, write_parquet],
  dplyr[bind_rows],
  r2dii.climate.stress.test[run_trisk],
  app / logic / data_load[load_backend_crispy_data, load_backend_trajectories_data, load_backend_trisk_run_metadata]
)


# Function to run the trisk model with given parameters and input path
# Returns the wrangled and checked results
run_trisk_with_params <- function(trisk_run_params, trisk_input_path) {
  # Run the trisk model with the provided parameters and input path
  # The results are returned and stored in st_results_wrangled_and_checked
  st_results_wrangled_and_checked <- do.call(
    run_trisk,
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
  run_metadata <- st_results_wrangled_and_checked$crispy_output |>
    dplyr::distinct_at(c(names(trisk_run_params), "run_id"))

  # Add the run metadata to the results
  st_results_wrangled_and_checked$run_metadata <- run_metadata

  # Return the results
  return(st_results_wrangled_and_checked)
}

# Function to append stress test results to backend data
# It iterates over the names of the results, processes the data accordingly and appends it to the existing data
# The updated data is then written back to the file
append_st_results_to_backend_data <- function(
    st_results_wrangled_and_checked,
    backend_trisk_run_folder,
    max_trisk_granularity) {
  # Iterate over the names of the results
  for (fname in names(st_results_wrangled_and_checked)) {
    # Get the new data from the results
    new_data <- st_results_wrangled_and_checked[[fname]]

    # Process the new data based on its name
    if (fname == "crispy_output") {
      new_data <- new_data |>
        stress.test.plot.report::main_load_multi_crispy_data(granularity = names(max_trisk_granularity)) |>
        dplyr::filter(.data$term == 1)
    } else if (fname == "company_trajectories") {
      new_data <- new_data |>
        stress.test.plot.report::main_data_load_trajectories_data(granularity = names(max_trisk_granularity))
    }

    # Get the path of the file to write the data to
    fpath <- fs::path(backend_trisk_run_folder, fname, ext = "parquet")

    # If the file exists, read the existing data, otherwise set the existing data to NULL
    if (file.exists(fpath)) {
      persistent_data <- read_parquet(fpath)
    } else {
      persistent_data <- NULL
    }

    # Append the new data to the existing data
    persistent_data <- bind_rows(persistent_data, new_data)

    # Write the updated data back to the file
    write_parquet(persistent_data, fpath)
  }
}

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

# Function to get run data from a given run_id
# Returns a list containing the crispy_output and company_trajectories data filtered by the run_id
get_run_data_from_run_id <- function(run_id, backend_trisk_run_folder) {
  # Load the crispy_output and company_trajectories data
  crispy_output <- load_backend_crispy_data(backend_trisk_run_folder)
  company_trajectories <- load_backend_trajectories_data(backend_trisk_run_folder)

  # Filter the data based on the provided run_id
  crispy_output <- crispy_output |> dplyr::filter(run_id == run_id)
  company_trajectories <- company_trajectories |> dplyr::filter(run_id == run_id)

  # Return the filtered data
  return(list(
    "crispy_output" = crispy_output,
    "company_trajectories" = company_trajectories
  ))
}



# function used to debug trisk_generator() , in the terminal.
# It will display a copy/pastable error message with the parameters that caused the error
format_error_message <- function(trisk_run_params) {
  cat("Failed with parameters : ")

  # Function to format each list element
  format_element <- function(name, value) {
    if (is.numeric(value)) {
      return(paste(name, "=", value, sep = ""))
    } else {
      return(paste(name, "=", sprintf('"%s"', value), sep = ""))
    }
  }

  # Apply the function to each element and concatenate them
  formatted_list <- sapply(names(trisk_run_params), function(name) {
    format_element(name, trisk_run_params[[name]])
  }, USE.NAMES = FALSE)

  # Print the formatted string
  cat(paste(formatted_list, collapse = ", "), "\n")
}
