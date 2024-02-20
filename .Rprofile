if (file.exists("renv")) {
  source("renv/activate.R")
} else {
  # The `renv` directory is automatically skipped when deploying with rsconnect.
  message("No 'renv' directory found; renv won't be activated.")
}

# Allow absolute module imports (relative to the app root).
options(box.path = getwd())

Sys.setenv(TRISK_INPUT_PATH = file.path("app", "data", "st_inputs"))
Sys.setenv(TRISK_OUTPUT_PATH = file.path("app", "data", "backend_db"))
# 'prod' or 'dev' 
Sys.setenv(CRISPY_APP_ENV = "dev")
