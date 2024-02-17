# Define an endpoint that accepts POST requests
# Assume the JSON payload is directly analogous to the R list structure for trisk_run_param

source("./trisk_compute.R")

# Create a plumber router
pr <- plumber::Plumber$new()

validate_trisk_run_params <- function(trisk_run_params) {
  required_keys <- names(formals(r2dii.climate.stress.test::run_trisk))
  param_keys <- names(trisk_run_params)
  
  if (!all(names(param_keys) %in% required_keys)) {
    stop("trisk_run_params does not contain the correct keys")
  }
}

pr$handle("POST", "/compute_trisk", function(req, res){
  
  trisk_run_params <- jsonlite::fromJSON(req$postBody)$trisk_run_params
  validate_trisk_run_params(trisk_run_params)

  # hardcoded input fp while the data is still part of the docker image
  trisk_input_path <- file.path(".", "st_inputs")

  postgres_conn <- DBI::dbConnect(
    RPostgres::Postgres(), 
    dbname = Sys.getenv("ST_POSTGRES_DB"), 
    host = Sys.getenv("ST_POSTGRES_HOST"), 
    port = Sys.getenv("ST_POSTGRES_PORT"), 
    user = Sys.getenv("ST_POSTGRES_USERNAME"), 
    password = Sys.getenv("ST_POSTGRES_PASSWORD")
    )
  
  run_id <- run_trisk_and_upload_results_to_db_conn(
    trisk_run_params=trisk_run_params,
    trisk_input_path=trisk_input_path,
    postgres_conn=postgres_conn
  )
  
  print("TRISK run & upload complete")

  response <- list(trisk_run_id=run_id)
  response <- jsonlite::toJSON(response, auto_unbox = TRUE)
  return(response)
})

# Run the plumber API on port 8080
pr$run(port=8080, host="0.0.0.0")


