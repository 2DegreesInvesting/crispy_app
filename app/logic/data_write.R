# Local backend data writing function
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
        dplyr::mutate(company_id = as.character(company_id)) |>
        stress.test.plot.report::main_load_multi_crispy_data(granularity = names(max_trisk_granularity))
    } else if (fname == "company_trajectories") {
      new_data <- new_data |>
        stress.test.plot.report::main_data_load_trajectories_data(granularity = names(max_trisk_granularity))
    }

    # Get the path of the file to write the data to
    fpath <- fs::path(backend_trisk_run_folder, fname, ext = "parquet")

    # If the file exists, read the existing data, otherwise set the existing data to NULL
    if (file.exists(fpath)) {
      persistent_data <- arrow::read_parquet(fpath)
    } else {
      persistent_data <- NULL
    }

    # Append the new data to the existing data
    persistent_data <- dplyr::bind_rows(persistent_data, new_data)

    # Write the updated data back to the file
    arrow::write_parquet(persistent_data, fpath)
  }
}
