box::use(
  reticulate[source_python, use_virtualenv]
)

match_company_input_to_backend <- function() {

}

fuzzy_top_five_matches <- function(
    input_string,
    reference_list) {
  
  # Source the Python script
  use_virtualenv(fs::path(getwd(),".venv"))
  source_python(file.path("scripts", "python", "fuzzy_match.py"))
browser()
  input_string <- tolower(input_string)
  reference_list <- sapply(reference_list, tolower)
  # Call the Python function
  matches <- top_five_matches(input_string, reference_list)

  matches_strings <- sapply(matches, function(x) x[[1]])
  matches_scores <- sapply(matches, function(x) x[[2]]) # not used

  return(matches_strings)
}

# Function to return filtered choices based on input
get_filtered_choices <- function(search_term, all_choices) {
  choices <- all_choices
  if (nchar(search_term) > 0) {
    choices <- fuzzy_top_five_matches(search_term, choices)
  }

  # Select up to 5 random choices if more than 5 are available in the output of fuzzy_top_five_matches
  if (length(choices) > 5) {
    choices <- sample(choices, 5)
  }
  return(choices)
}
