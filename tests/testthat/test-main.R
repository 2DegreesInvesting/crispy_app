box::use(
  shiny[testServer],
  testthat[...],
)
box::use(
  app / main[...],
)




test_that("main server works without error", {
  Sys.setenv(TRISK_INPUT_PATH = file.path("ST_INPUTS_DEV"))
  Sys.setenv(TRISK_OUTPUT_PATH = tempdir())
  Sys.setenv(CRISPY_APP_ENV = "dev")

  expect_error(testServer(server, {

  }), NA)
})
