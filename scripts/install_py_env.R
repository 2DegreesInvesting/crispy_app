## Deprecated // For reference, how to navigate python versions in reticulate

library(reticulate)

python_version <- Sys.getenv("CRISPY_LOAN_PYTHON_VERSION")

venv_dir <- file.path(getwd(), ".venv") # You can name this as you prefer
virtualenv_create(envname = venv_dir, python = python_version)

# Activate the virtual environment
Sys.unsetenv("RETICULATE_PYTHON")
use_virtualenv(venv_dir, required = TRUE)

system2("pip", args = c("install poetry"))
