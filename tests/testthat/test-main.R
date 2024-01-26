box::use(
  shiny[testServer],
  testthat[...],
)
box::use(
  app / main[...],
)




test_that("main server works without error", {
  Sys.setenv(TRISK_INPUT_PATH = file.path("ST_INPUTS_DEV"))
  Sys.setenv(BACKEND_TRISK_RUN_FOLDER = tempdir())

  expect_error(testServer(server, {

  }), NA)
})
