box::use(
  shiny[
    moduleServer, NS, observe, div, tags, reactiveVal, tagList, reactiveValues, eventReactive, p, observeEvent, img,
    HTML, conditionalPanel
  ],
  shiny.semantic[slider_input, dropdown_input, segment, update_dropdown_input, update_slider],
  shinyjs[useShinyjs],
  shiny.semantic[card, cards]
)

box::use(
  app/logic/renamings[rename_string_vector],
  app/logic/trisk_mgmt[run_trisk_with_params, append_st_results_to_backend_data, check_if_run_exists, get_run_data_from_run_id]
)


# function used for debug in the terminal
format_error_message <- function(trisk_run_params) {
  cat("Failed with parameters : ")

  # Function to format each list element
  format_element <- function(name, value) {
    if (is.numeric(value)) {
      return(paste(name, "=", value, sep = ""))
    } else {
      return(paste(name, "=", sprintf('"%s"', value), sep = ""))
    }
  }

  # Apply the function to each element and concatenate them
  formatted_list <- sapply(names(trisk_run_params), function(name) {
    format_element(name, trisk_run_params[[name]])
  }, USE.NAMES = FALSE)

  # Print the formatted string
  cat(paste(formatted_list, collapse = ", "), "\n")
}


trisk_generator <- function(backend_trisk_run_folder, trisk_input_path, trisk_run_params, max_trisk_granularity){
        
        run_id <- check_if_run_exists(trisk_run_params, backend_trisk_run_folder)
        
        if (is.null(run_id)) {
          shinyjs::runjs("$('#mymodal').modal({closable: false}).modal('show');")
          st_results_wrangled_and_checked <- tryCatch(
            {
              run_trisk_with_params(
                trisk_run_params,
                trisk_input_path
              )
            },
            error = function(e) {
              cat(e$message)
              format_error_message(trisk_run_params)
              NULL
            }
          )

          if (!is.null(st_results_wrangled_and_checked)) {
            # Close the modal dialog and re-enable UI
            append_st_results_to_backend_data(
              st_results_wrangled_and_checked,
              backend_trisk_run_folder,
              max_trisk_granularity
            )
          }
          shinyjs::runjs("$('#mymodal').modal('hide');")
          run_id <- check_if_run_exists(trisk_run_params, backend_trisk_run_folder)
        }
        

        return(run_id)
}

