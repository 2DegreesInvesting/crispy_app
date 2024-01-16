box::use(
  arrow[read_parquet, write_parquet],
  dplyr[bind_rows],
  r2dii.climate.stress.test[run_trisk],
  app/logic/data_load[load_backend_crispy_data, load_backend_trajectories_data, load_backend_trisk_run_metadata]
)

run_trisk_with_params <- function(trisk_input_path, trisk_run_params) {
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

  run_metadata <- st_results_wrangled_and_checked$crispy_output |>
    dplyr::distinct_at(c(names(trisk_run_params), "run_id"))

  st_results_wrangled_and_checked$run_metadata <- run_metadata


  return(st_results_wrangled_and_checked)
}

append_st_results_to_backend_trisk_run_data <- function(st_results_wrangled_and_checked, backend_trisk_run_folder) {
  for (fname in names(st_results_wrangled_and_checked)) {
    fpath <- fs::path(backend_trisk_run_folder, fname, ext = "parquet")
    if (file.exists(fpath)) {
      file_data <- read_parquet(fpath)
    } else {
      file_data <- NULL
    }

    file_data <- bind_rows(file_data, st_results_wrangled_and_checked[[fname]])

    write_parquet(file_data, fpath)
  }
}

check_if_run_exists <- function(trisk_run_params, backend_trisk_run_folder) {
  backend_trisk_run_metadata <- load_backend_trisk_run_metadata(backend_trisk_run_folder)

  df <- backend_trisk_run_metadata
  for (trisk_param in names(trisk_run_params)) {
    df <- df |> dplyr::filter(!!rlang::sym(trisk_param) == trisk_run_params[[trisk_param]])
  }

  if (nrow(df) == 1) {
    run_id <- df |> dplyr::pull(run_id)
    }
  else if (nrow(df) == 0) {
    run_id <- NULL
  } 
  else{
    stop("More than 1 run have been found with the provided trisk input parameters")
  }

  return(run_id)
}

get_run_data_from_run_id <- function(run_id, backend_trisk_run_folder) {
  crispy_output <- load_backend_crispy_data(backend_trisk_run_folder)
  company_trajectories <- load_backend_trajectories_data(backend_trisk_run_folder)


  crispy_output <- crispy_output |> dplyr::filter(run_id == run_id)
  company_trajectories <- company_trajectories |> dplyr::filter(run_id == run_id)

  return(list(
    "crispy_output"=crispy_output,
    "company_trajectories"=company_trajectories
  ))

}
