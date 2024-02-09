load_backend_crispy_data <- function(backend_trisk_run_folder) {
  backend_crispy_data_path <- fs::path(backend_trisk_run_folder, "crispy_output", ext = "parquet")

  if (file.exists(backend_crispy_data_path)) {
    backend_crispy_data <- arrow::read_parquet(backend_crispy_data_path) 
  } else {
    backend_crispy_data <- tibble::tibble(
      run_id = character(),
      company_id = character(),
      ald_sector = character(),
      ald_business_unit = character(),
      term = numeric(),
      net_present_value_baseline = numeric(),
      net_present_value_shock = numeric(),
      pd_baseline = numeric(),
      pd_shock = numeric()
    )
  }
  return(backend_crispy_data)
}

load_backend_trajectories_data <- function(backend_trisk_run_folder) {
  backend_trajectories_data_path <- fs::path(backend_trisk_run_folder, "company_trajectories", ext = "parquet")
  if (file.exists(backend_trajectories_data_path)) {
    backend_trajectories_data <- arrow::read_parquet(backend_trajectories_data_path)
  } else {
    backend_trajectories_data <- tibble::tibble(
      run_id = character(),
      year = numeric(),
      company_id = character(),
      ald_sector = character(),
      ald_business_unit = character(),
      production_baseline_scenario = character(),
      production_target_scenario = numeric(),
      production_shock_scenario = numeric()
    )
  }

  return(backend_trajectories_data)
}

load_backend_trisk_run_metadata <- function(backend_trisk_run_folder) {
  backend_trisk_run_metadata_path <- fs::path(backend_trisk_run_folder, "run_metadata", ext = "parquet")
  if (file.exists(backend_trisk_run_metadata_path)) {
    backend_trisk_run_metadata <- arrow::read_parquet(backend_trisk_run_metadata_path)
  } else {
    backend_trisk_run_metadata <- tibble::tibble(
      run_id = character(),
      roll_up_type = character(),
      baseline_scenario = character(),
      shock_scenario = character(),
      scenario_geography = character(),
      risk_free_rate = numeric(),
      discount_rate = numeric(),
      dividend_rate = numeric(), # TODO remove
      growth_rate = numeric(),
      shock_year = numeric(),
      div_netprofit_prop_coef = numeric(),
      financial_stimulus = numeric(),
      carbon_price_model = character(),
      market_passthrough = numeric()
    )
  }
  return(backend_trisk_run_metadata)
}
