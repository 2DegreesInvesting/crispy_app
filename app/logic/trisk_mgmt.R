# Import necessary libraries and functions
box::use(
  arrow[read_parquet, write_parquet],
  dplyr[bind_rows],
  r2dii.climate.stress.test[run_trisk],
  app / logic / data_load[load_backend_crispy_data, load_backend_trajectories_data, load_backend_trisk_run_metadata]
)


box::use(
  app / logic / trisk_mgmt[
      run_trisk_with_params,
      format_error_message
  ],
  app / logic / data_load[
    load_backend_trajectories_data,
    load_backend_crispy_data
  ]
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
