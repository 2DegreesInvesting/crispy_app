trigger_trisk_api_computation <- function(trisk_run_params, api_endpoint){
    print("in api")
    browser()


    # curl -X 'POST' \
    # 'http://188.166.192.175/compute_trisk/' \
    # -H 'Content-Type: application/json' \
    # -d '{"trisk_run_params": {...}}'
}


get_data_from_postgres <- function(
  table_name, 
  dbname, 
  host_db, 
  db_port, 
  db_user, 
  db_password, 
  query_filter
) {
  browser()
  # Create a connection string
  conn <- DBI::dbConnect(RPostgres::Postgres(),
                    dbname = dbname,
                    host = host_db,
                    port = db_port,
                    user = db_user,
                    password = db_password)
  
  # Construct the SQL query
  query <- paste("SELECT * FROM", table_name, "WHERE", query_filter)
  
  # Execute the query and fetch results
  data <- DBI::dbGetQuery(conn, query)
  
  # Close the connection
  DBI::dbDisconnect(conn)
  
  return(data)
}
