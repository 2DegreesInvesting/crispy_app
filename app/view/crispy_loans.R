# Load required packages
box::use(
  shiny[moduleServer, NS, div, h1, tags, reactiveVal, observeEvent, observe, eventReactive, HTML, selectizeInput, updateSelectizeInput, updateTextInput],
  shiny.semantic[semanticPage, button, segment],
  semantic.dashboard[dashboardPage, dashboardBody, dashboardSidebar, dashboardHeader, box],
  DT[DTOutput, renderDT, datatable, JS, dataTableProxy, replaceData],
  shinyjs[runjs, useShinyjs]
)

box::use(
  app / view / modules / trisk_mgmt,
  app/view/modules/portfolio_mgmt
)



####### UI

ui <- function(id, max_trisk_granularity, available_vars) {
  ns <- NS(id)

    shiny::div(
    class = "pusher container", style = "min-height: 100vh;",
    shiny::div(
      class = "ui segment", style = "min-height: 100vh;",
        useShinyjs(), # Initialize shinyjs
        portfolio_mgmt$ui(ns("portfolio_mgmt"), "Loans Portfolio"),
        trisk_mgmt$ui(ns("trisk_mgmt")),
        div(
          class = "16 wide columns",
          div(
            class = "ui action input",
            selectizeInput(ns("category_input"), NULL,
              selected = NULL, choices = NULL,
              options = list(
                placeholder = "Enter Category",
                onInitialize = I("function() { this.setValue(''); }")
              )
            ),
            button(ns("add_btn"), "Add new row", class = "ui button")
          ),
          button(ns("delete_btn"), "Delete Selected Rows", class = "ui button"),
        )
      ))
}

####### Server

server <- function(id, perimeter, backend_trisk_run_folder, trisk_input_path, max_trisk_granularity) {
  moduleServer(id, function(input, output, session) {

    # SELECT PARAMETERS =========================
    trisk_granularity_r <- perimeter$trisk_granularity_r
    trisk_run_params_r <- perimeter$trisk_run_params_r

    results <- trisk_mgmt$server(
      "trisk_mgmt",
      crispy_data_r = crispy_data_r,
      trisk_granularity_r = trisk_granularity_r,
      trisk_run_params_r = trisk_run_params_r,
      backend_trisk_run_folder = backend_trisk_run_folder,
      trisk_input_path = trisk_input_path,
      max_trisk_granularity = max_trisk_granularity
    )

    crispy_data_r <- results$crispy_data_r
    trajectories_data_r <- results$trajectories_data_r


    # INIT PORTFOLIO ===============

    display_columns_loans <- c(
      names(max_trisk_granularity),
      "exposure_value_usd",
      "crispy_perc_value_change",
      "crispy_value_loss",
      "loss_given_default",
      "expiration_date"
    )

    editable_columns_names_loans <- c("exposure_value_usd", "loss_given_default", "expiration_date")

    colored_columns_names_loans <- c("crispy_perc_value_change", "crispy_value_loss")


    analysis_data_r <- portfolio_mgmt$server(
      "portfolio_mgmt",
      crispy_data_r = crispy_data_r,
      trisk_granularity_r = trisk_granularity_r,
      max_trisk_granularity = max_trisk_granularity,
      display_columns = display_columns_loans,
      editable_columns_names = editable_columns_names_loans,
      colored_columns_names = colored_columns_names_loans
    )

    # ADD ROW ============
    
    all_choices <- c('aa', 'aaa', 'ab', 'baa', 'abc', 'acb', 'bac', 'bca', 'cab', 'cba') # Extend as needed
    current_input <- reactiveVal("") # Reactive value to track the current input


    observe({
      # Update the selectize input with the new filtered choices
      updateSelectizeInput(
        session, 
        "category_input", 
        choices = get_filtered_choices(current_input(), all_choices), 
        selected = current_input(),
        server = TRUE)
    })

    # add a new row by creating it in the portfolio
    observeEvent(input$add_btn, {
      
      portfolio_data_r(
          dplyr::bind_rows(
            portfolio_data_r(),
            tibble::as_tibble(list(ald_sector= input$category_input))
          )
        )
      updateTextInput(session, "category_input", value = "")  # Clear the input
      current_input("") # Reset the current input tracker
    })

    # JavaScript to click add button on pressing Enter
    runjs("
        $('#category_input').on('keyup', function (e) {
          if (e.keyCode === 13) {
            $('#add_btn').click();
          }
        });
      ")


  # DELETE ROWS =============

    output$portfolio_table <- renderDT({
    datatable(portfolio_data_r(), selection = 'single')
  })

  proxy <- dataTableProxy('portfolio_table')

  observeEvent(input$delete_btn, {
    selected_row <- input$portfolio_table_rows_selected
    if(length(selected_row)) {
      my_data_data <- portfolio_data_r()
      my_data_data <- my_data_data[-selected_row, , drop = FALSE]
      portfolio_data_r(my_data_data)
      replaceData(proxy, my_data_data, resetPaging = FALSE)
    }
  })

  })
}




render_portfolio <- function(output, table_to_display) {
  output$portfolio_table <- renderDT(
    {
      datatable(table_to_display,
        editable = TRUE,
        options = list(
          lengthChange = FALSE, # Remove "Show XXX entries" option
          paging = FALSE, # Remove pagination
          searching = FALSE, # Remove search input
          info = FALSE # Remove "Showing N of X entries"
        )
      )
    },
    server = FALSE
  )
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

