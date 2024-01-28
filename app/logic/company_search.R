match_company_input_to_backend <- function() {

}

fuzzy_match.top_five_matches <- function(
    input_string,
    reference_list) {
  # Source the Python script
  # Sys.unsetenv("RETICULATE_PYTHON")
  # reticulate::use_virtualenv(fs::path(getwd(),".venv"))

  reticulate::source_python(fs::path("scripts", "python", "fuzzy_match.py"))

  input_string <- tolower(input_string)
  reference_list <- sapply(reference_list, tolower)
  # Call the Python function
  matches <- top_five_matches(input_string, reference_list)

  matches_strings <- sapply(matches, function(x) x[[1]])
  # matches_scores <- sapply(matches, function(x) x[[2]])

  return(matches_strings)
}
