if (file.exists("renv")) {
  source("renv/activate.R")
} else {
  # The `renv` directory is automatically skipped when deploying with rsconnect.
  message("No 'renv' directory found; renv won't be activated.")
}

# Allow absolute module imports (relative to the app root).
options(box.path = getwd())


# ENV VARIABLES

Sys.setenv(TRISK_INPUT_PATH = fs::path("app", "data", "st_inputs"))
Sys.setenv(BACKEND_TRISK_RUN_FOLDER = fs::path("app", "data", "backend_db"))
