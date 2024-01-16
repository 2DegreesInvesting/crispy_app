box::use(
  scripts / constant[trisk_input_path, trisk_output_path]
)

library(r2dii.climate.stress.test)


dir.create(fs::path(trisk_output_path), showWarnings = FALSE)
unlink(fs::path(trisk_output_path, "*"), recursive = TRUE)



# # Define the folder containing Parquet files
# folder_path <- trisk_input_path

# # List all Parquet files in the folder
# parquet_files <- list.files(folder_path, pattern = "\\.parquet$", full.names = TRUE)

# # Function to convert Parquet to CSV
# convert_parquet_to_csv <- function(parquet_file) {
#   # Read the Parquet file
#   data <- arrow::read_parquet(parquet_file)

#   # Create the CSV file name
#   csv_file_name <- gsub("parquet$", "csv", basename(parquet_file))
#   csv_file_path <- file.path(dirname(parquet_file), csv_file_name)

#   # Write to CSV
#   readr::write_csv(data, csv_file_path)
# }

# # Apply the function to each Parquet file
# purrr::map(parquet_files, convert_parquet_to_csv)


####### RUN TRISK

r2dii.climate.stress.test::run_trisk(
  input_path = trisk_input_path,
  output_path = trisk_output_path
)


r2dii.climate.stress.test::run_trisk(
  input_path = trisk_input_path,
  output_path = trisk_output_path,
  baseline_scenario = "NGFS2021_REMIND_NDC",
  shock_scenario = "NGFS2021_REMIND_NZ2050"
)

r2dii.climate.stress.test::run_trisk(
  input_path = trisk_input_path,
  output_path = trisk_output_path,
  baseline_scenario = "NGFS2021_REMIND_NDC",
  shock_scenario = "NGFS2021_REMIND_NZ2050",
  shock_year = 2030
)

r2dii.climate.stress.test::run_trisk(
  input_path = trisk_input_path,
  output_path = trisk_output_path,
  baseline_scenario = "NGFS2021_REMIND_NDC",
  shock_scenario = "NGFS2021_REMIND_NZ2050",
  shock_year = 2033
)

r2dii.climate.stress.test::run_trisk(
  input_path = trisk_input_path,
  output_path = trisk_output_path,
  baseline_scenario = "NGFS2021_REMIND_NDC",
  shock_scenario = "NGFS2021_REMIND_NZ2050",
  shock_year = 2035
)

r2dii.climate.stress.test::run_trisk(
  input_path = trisk_input_path,
  output_path = trisk_output_path,
  baseline_scenario = "NGFS2021_MESSAGE_NDC",
  shock_scenario = "NGFS2021_MESSAGE_B2DS"
)

r2dii.climate.stress.test::run_trisk(
  input_path = trisk_input_path,
  output_path = trisk_output_path,
  baseline_scenario = "IPR2021_baseline",
  shock_scenario = "IPR2021_FPS"
)



# # Function to convert a CSV file to a Parquet file
# convert_csv_to_parquet <- function(csv_file_path) {
#   # Check if the file exists
#   if (!file.exists(csv_file_path)) {
#     stop("File does not exist: ", csv_file_path)
#   }

#   # Read the CSV file
#   data <- readr::read_csv(csv_file_path)

#   # Create the Parquet file name
#   parquet_file_path <- sub("\\.csv$", ".parquet", csv_file_path)

#   # Write to Parquet
#   arrow::write_parquet(data, parquet_file_path)

#   # Return the path of the created Parquet file
#   return(parquet_file_path)
# }

# # Define the folder containing Parquet files
# folder_path <- file.path("scripts", "raw_data", "test_data", "synthetic_crispy")

# # List all Parquet files in the folder
# csv_files <- list.files(folder_path, pattern = "\\.csv$", full.names = TRUE, recursive = TRUE)


# purrr::map(csv_files, convert_csv_to_parquet)
