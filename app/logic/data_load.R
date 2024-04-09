box::use(
  app/logic/constant[
    DBNAME,
    HOST_DB,
    DB_PORT,
    DB_USER,
    DB_PASSWORD
  ],
  app/logic/cloud_logic[
    get_data_from_postgres
  ]
)

base_data_load <- function(table_name, run_id = NULL, backend_trisk_run_folder = NULL, default_tibble = NULL) {
  if (Sys.getenv("CRISPY_APP_ENV") == "local") {
    table_data_path <- fs::path(backend_trisk_run_folder, table_name, ext = "parquet")
    if (file.exists(table_data_path)) {
      if (!is.null(run_id)) {
        table_data <- arrow::read_parquet(table_data_path) |>
          dplyr::filter(.data$run_id == .env$run_id)
      } else {
        table_data <- arrow::read_parquet(table_data_path)
      }
    } else {
      table_data <- default_tibble
    }
  } else if (Sys.getenv("CRISPY_APP_ENV") == "cloud") {
    if (!is.null(run_id)) {
      query_filter <- paste0("run_id = '", run_id, "'")
    } else {
      query_filter <- NULL
    }

    table_data <- get_data_from_postgres(
      table_name = table_name,
      dbname = DBNAME,
      host_db = HOST_DB,
      db_port = DB_PORT,
      db_user = DB_USER,
      db_password = DB_PASSWORD,
      query_filter = query_filter,
      default_tibble = default_tibble
    )
  } else {
    stop("You must set the env variable CRISPY_APP_ENV to either 'local' or 'cloud'")
  }

  return(table_data)
}



load_backend_crispy_data <- function(backend_trisk_run_folder, run_id = NULL) {
  backend_crispy_data <- base_data_load(
    table_name = "crispy_output",
    run_id = run_id,
    backend_trisk_run_folder = backend_trisk_run_folder,
    default_tibble = tibble::tibble(
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
  )
  return(backend_crispy_data)
}


load_backend_trajectories_data <- function(backend_trisk_run_folder, run_id = NULL) {
  backend_trajectories_data <- base_data_load(
    table_name = "company_trajectories",
    run_id = run_id,
    backend_trisk_run_folder = backend_trisk_run_folder,
    default_tibble = tibble::tibble(
      run_id = character(),
      year = numeric(),
      company_id = character(),
      ald_sector = character(),
      ald_business_unit = character(),
      production_baseline_scenario = character(),
      production_target_scenario = numeric(),
      production_shock_scenario = numeric()
    )
  )
  return(backend_trajectories_data)
}


load_backend_trisk_run_metadata <- function(backend_trisk_run_folder, run_id = NULL) {
  backend_trisk_run_metadata <- base_data_load(
    table_name = "run_metadata",
    run_id = run_id,
    backend_trisk_run_folder = backend_trisk_run_folder,
    default_tibble = tibble::tibble(
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
  )

  return(backend_trisk_run_metadata)
}
