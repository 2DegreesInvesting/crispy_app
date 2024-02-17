trigger_trisk_api_computation <- function(trisk_run_params, api_endpoint) {
  # Define the URL
  # trisk_api_service <- "trisk-api-service"
  trisk_api_service <- Sys.getenv("TRISK_API_SERVICE")
  url <- paste0("http://", trisk_api_service, ":80/compute_trisk")
  # url <- "http://164.90.241.52:80/compute_trisk/"

  # Define the body of the request
  body <- list(
    trisk_run_params = trisk_run_params
  )
  # Convert the body to JSON
  body_json <- jsonlite::toJSON(body, auto_unbox = TRUE)

  # Define the headers, including Host
  headers <- c(
    `Host` = paste0("trisk-api.default.",trisk_api_service),
    `Content-Type` = "application/json"
  )

  # Make the POST request with a 6-minute timeout
  response <- httr::POST(url, body = body_json, httr::add_headers(.headers = headers), httr::timeout(360))

  # Check the response
  status_code <- httr::status_code(response)

  if (status_code != 200) {
    stop("The request failed with status code ", status_code)
  }

  content <- httr::content(response, "text", encoding = "UTF-8")
  # jsonlite::fromJSON has to be doubled bc of this content structure
  run_id <- jsonlite::fromJSON(jsonlite::fromJSON(content)$trisk_run_id)

  return(run_id)
}


get_data_from_postgres <- function(
    table_name,
    dbname,
    host_db,
    db_port,
    db_user,
    db_password,
    query_filter = NULL,
    default_tibble = tibble::tibble()) {
  # Create a connection string
  conn <- DBI::dbConnect(RPostgres::Postgres(),
    dbname = dbname,
    host = host_db,
    port = db_port,
    user = db_user,
    password = db_password
  )

  if (DBI::dbExistsTable(conn, table_name)) {
    # Construct the SQL query
    if (is.null(query_filter)) {
      query <- paste("SELECT * FROM", table_name)
    } else {
      query <- paste("SELECT * FROM", table_name, "WHERE", query_filter)
    }


    # Execute the query and fetch results
    table_data <- DBI::dbGetQuery(conn, query)
    table_data <- tibble::as_tibble(table_data)
  } else {
    table_data <- default_tibble
  }
  # Close the connection
  DBI::dbDisconnect(conn)

  return(table_data)
}
