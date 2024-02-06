match_company_input_to_backend <- function() {

}

fuzzy_top_five_matches <- function(
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



match_choices <- function(input_str, all_choices) {
  # Convert the input string to lower case and split into individual characters
  input_chars <- tolower(strsplit(input_str, "")[[1]])

  # Filter choices: include choice if it contains any of the characters in the input string
  filtered_choices <- all_choices[sapply(all_choices, function(choice) {
    any(sapply(input_chars, function(char) {
      grepl(char, tolower(choice))
    }))
  })]

  return(filtered_choices)
}

# Function to return filtered choices based on input
get_filtered_choices <- function(search_term, all_choices) {
  choices <- all_choices
  if (nchar(search_term) > 0) {
    choices <- match_choices(search_term, choices)
  }

  # Select up to 5 random choices if more than 5 are available
  if (length(choices) > 5) {
    choices <- sample(choices, 5)
  }
  return(choices)
}
