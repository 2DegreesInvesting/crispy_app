box::use(
  app / logic / trisk_mgmt[run_trisk_with_params, append_st_results_to_backend_data, check_if_run_exists, get_run_data_from_run_id],
  app / logic / constant[max_trisk_granularity]
)

TEST_TRISK_INPUT_PATH <- file.path("ST_INPUTS_DEV")

TEST_TRISK_PARAMS <- list(
  baseline_scenario = "Oxford2021_base",
  shock_scenario = "Oxford2021_fast",
  discount_rate = 0.1,
  risk_free_rate = 0.0,
  growth_rate = 0.01,
  shock_year = 2025,
  scenario_geography = "Global",
  carbon_price_model = "no_carbon_tax",
  market_passthrough = 0.0,
  lgd = 0.45,
  div_netprofit_prop_coef = 1.0,
  start_year = 2022,
  financial_stimulus = 1.0
)




test_that("run_trisk_with_params returns company_trajectories, crispy_output, and run_metadata", {
  st_results_wrangled_and_checked <- run_trisk_with_params(trisk_input_path = TEST_TRISK_INPUT_PATH, trisk_run_params = TEST_TRISK_PARAMS)
  expect_equal(names(st_results_wrangled_and_checked), c("company_trajectories", "crispy_output", "run_metadata"))
})


test_that("append_st_results_to_backend_data creates and increments the backend datasets properly", {
  TEST_BACKEND_TRISK_RUN_FOLDER <- file.path(tempdir(), uuid::UUIDgenerate())
  if (dir.exists(TEST_BACKEND_TRISK_RUN_FOLDER)) {
    unlink(TEST_BACKEND_TRISK_RUN_FOLDER, recursive = TRUE)
  }
  dir.create(TEST_BACKEND_TRISK_RUN_FOLDER)

  expect_true(length(list.files(TEST_BACKEND_TRISK_RUN_FOLDER)) == 0)

  st_results_wrangled_and_checked_1 <- run_trisk_with_params(trisk_input_path = TEST_TRISK_INPUT_PATH, trisk_run_params = TEST_TRISK_PARAMS)

  append_st_results_to_backend_data(
    st_results_wrangled_and_checked_1,
    backend_trisk_run_folder = TEST_BACKEND_TRISK_RUN_FOLDER,
    max_trisk_granularity = max_trisk_granularity
  )

  expect_equal(list.files(TEST_BACKEND_TRISK_RUN_FOLDER), c("company_trajectories.parquet", "crispy_output.parquet", "run_metadata.parquet"))


  st_results_wrangled_and_checked_2 <- run_trisk_with_params(trisk_input_path = TEST_TRISK_INPUT_PATH, trisk_run_params = TEST_TRISK_PARAMS)

  append_st_results_to_backend_data(
    st_results_wrangled_and_checked_2,
    backend_trisk_run_folder = TEST_BACKEND_TRISK_RUN_FOLDER,
    max_trisk_granularity = max_trisk_granularity
  )

  run_ids <- c(
    st_results_wrangled_and_checked_1$run_metadata |> dplyr::pull(run_id),
    st_results_wrangled_and_checked_2$run_metadata |> dplyr::pull(run_id)
  )

  run_metadata <- arrow::read_parquet(file.path(TEST_BACKEND_TRISK_RUN_FOLDER, "run_metadata.parquet"))
  expect_true(all(run_ids %in% (run_metadata |> dplyr::pull(run_id))))
})


test_that("check_if_run_exists finds expected run", {
  TEST_BACKEND_TRISK_RUN_FOLDER <- file.path(tempdir(), uuid::UUIDgenerate())
  if (dir.exists(TEST_BACKEND_TRISK_RUN_FOLDER)) {
    unlink(TEST_BACKEND_TRISK_RUN_FOLDER, recursive = TRUE)
  }
  dir.create(TEST_BACKEND_TRISK_RUN_FOLDER)

  st_results_wrangled_and_checked <- run_trisk_with_params(
    trisk_input_path = TEST_TRISK_INPUT_PATH,
    trisk_run_params = TEST_TRISK_PARAMS
  )
  append_st_results_to_backend_data(
    st_results_wrangled_and_checked,
    TEST_BACKEND_TRISK_RUN_FOLDER,
    max_trisk_granularity
  )
  run_id <- check_if_run_exists(
    trisk_run_params = TEST_TRISK_PARAMS,
    backend_trisk_run_folder = TEST_BACKEND_TRISK_RUN_FOLDER
  )

  expected_run_id <- st_results_wrangled_and_checked$run_metadata |> dplyr::pull(run_id)

  expect_equal(run_id, expected_run_id)
})


test_that("check_if_run_exists returns NULL if run does not exist", {
  TEST_BACKEND_TRISK_RUN_FOLDER <- file.path(tempdir(), uuid::UUIDgenerate())
  if (dir.exists(TEST_BACKEND_TRISK_RUN_FOLDER)) {
    unlink(TEST_BACKEND_TRISK_RUN_FOLDER, recursive = TRUE)
  }
  dir.create(TEST_BACKEND_TRISK_RUN_FOLDER)

  st_results_wrangled_and_checked <- run_trisk_with_params(
    trisk_input_path = TEST_TRISK_INPUT_PATH,
    trisk_run_params = TEST_TRISK_PARAMS
  )
  append_st_results_to_backend_data(
    st_results_wrangled_and_checked,
    backend_trisk_run_folder = TEST_BACKEND_TRISK_RUN_FOLDER,
    max_trisk_granularity = max_trisk_granularity
  )


  wrong_test_params <- TEST_TRISK_PARAMS
  wrong_test_params[["shock_year"]] <- 2030

  run_id <- check_if_run_exists(
    trisk_run_params = wrong_test_params,
    backend_trisk_run_folder = TEST_BACKEND_TRISK_RUN_FOLDER
  )

  expect_equal(run_id, NULL)
})




test_that("get_run_data_from_run_id returns the data associated to the run_id", {
  TEST_BACKEND_TRISK_RUN_FOLDER <- file.path(tempdir(), uuid::UUIDgenerate())
  if (dir.exists(TEST_BACKEND_TRISK_RUN_FOLDER)) {
    unlink(TEST_BACKEND_TRISK_RUN_FOLDER, recursive = TRUE)
  }
  dir.create(TEST_BACKEND_TRISK_RUN_FOLDER)

  st_results_wrangled_and_checked <- run_trisk_with_params(
    trisk_input_path = TEST_TRISK_INPUT_PATH,
    trisk_run_params = TEST_TRISK_PARAMS
  )
  append_st_results_to_backend_data(
    st_results_wrangled_and_checked,
    backend_trisk_run_folder = TEST_BACKEND_TRISK_RUN_FOLDER,
    max_trisk_granularity = max_trisk_granularity
  )
  run_id <- check_if_run_exists(
    trisk_run_params = TEST_TRISK_PARAMS,
    backend_trisk_run_folder = TEST_BACKEND_TRISK_RUN_FOLDER
  )

  run_data <- get_run_data_from_run_id(run_id, TEST_BACKEND_TRISK_RUN_FOLDER)

  crispy_output_run_id <- run_data$crispy_output |>
    dplyr::distinct(run_id) |>
    dplyr::pull()
  expect_equal(crispy_output_run_id, run_id)

  company_trajectories_run_id <- run_data$company_trajectories |>
    dplyr::distinct(run_id) |>
    dplyr::pull()

  expect_equal(company_trajectories_run_id, run_id)
})
