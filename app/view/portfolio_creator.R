box::use(
  shiny[moduleServer, NS, reactiveVal, reactive, observeEvent, observe, eventReactive, div, reactiveValues],
  semantic.dashboard[box],
  DT[DTOutput, renderDT, datatable, JS]
)

box::use(
  app/logic/constant[max_trisk_granularity],
  app/logic/renamings[rename_tibble_columns]
)





####### UI

ui <- function(id) {
  ns <- NS(id)
  # First row with 3 taking the entire page width
  box(width = 16, DTOutput(outputId = ns("portfolio_table")))
}

####### Server


server <- function(id, crispy_data_r, trisk_granularity_r, max_trisk_granularity) {
  moduleServer(id, function(input, output, session) {
    # PORTFOLIO DATA =========================
    display_columns <- c(
      names(max_trisk_granularity),
      "exposure_value_usd",
      "crispy_perc_value_change",
      "crispy_value_loss"
    )



    # Create a reactiveValues object to store the portfolio states
    portfolio_states <- reactiveValues()

    # Initial portfolio data structure
    portfolio_data_r <- reactiveVal()

    observe({
      trisk_granularity_names <- paste0(trisk_granularity_r(), collapse = "-") # Convert to character vector

      # If the portfolio state for the current granularity doesn't exist, create it@
      if (!(trisk_granularity_names %in% names(portfolio_states))) {
        dynamic_cols <- stats::setNames(lapply(trisk_granularity_r(), function(x) character()), trisk_granularity_r())
        dynamic_cols <- dplyr::as_tibble(dynamic_cols)

        static_cols <- tibble::tibble(
          portfolio_id = character(), # is always 1 for App Crispy Equities
          asset_type = character(), # is always fixed_income for App Crispy Equities
          exposure_value_usd = numeric(),
          loss_given_default = numeric(), # is always NA
          expiration_date = character(), # is always NA
          pd_portfolio = numeric() # is always NA
        )
        portfolio_data <- dplyr::bind_cols(dynamic_cols, static_cols)
        portfolio_data_r(portfolio_data)

        # Save the new portfolio state in the reactiveValues object
        # Update the portfolio data to the state corresponding to the current granularity
        portfolio_states[[trisk_granularity_names]] <- portfolio_data_r()
      } else {
        portfolio_data_r(portfolio_states[[trisk_granularity_names]])
      }
    })

    # PREPARE ANALYSIS DATA ===================================

    analysis_data_r <- reactiveVal()

    observe({
      if (!is.null(portfolio_data_r()) & !is.null(crispy_data_r())) {
        granularity <- dplyr::intersect(colnames(portfolio_data_r()), colnames(crispy_data_r()))

        if (nrow(portfolio_data_r()) == 0) {
          # initialise the porfolio sector column
          portfolio_data <- portfolio_data_r()

          portfolio_data <- portfolio_data |>
            dplyr::right_join(
              crispy_data_r() |> dplyr::distinct_at(granularity)
            ) |>
            dplyr::mutate(
              portfolio_id = "1",
              asset_type = "fixed_income"
            )
          portfolio_data_r(portfolio_data)
        }

        analysis_data <- stress.test.plot.report:::load_input_plots_data_from_tibble(
          portfolio_data = portfolio_data_r(),
          multi_crispy_data = crispy_data_r(),
          granularity = granularity
        ) |>
          dplyr::mutate(
            crispy_perc_value_change = round(crispy_perc_value_change, digits = 4),
            crispy_value_loss = round(crispy_value_loss, digits = 2)
          )

        analysis_data_r(analysis_data)
      }
    })


    # WRANGLE ANALYSIS DATA ===================================

    observeEvent(analysis_data_r(), ignoreInit = TRUE, {
      table_to_display <- analysis_data_r() |>
        dplyr::select(
          dplyr::any_of(display_columns)
        )
      table_to_display <- rename_tibble_columns(table_to_display, class = "analysis_columns")


      # TABLE DISPLAY ===================================

      n_granul_cols <- length(trisk_granularity_r())
      # Render the editable table
      output$portfolio_table <- DT::renderDT(
        {
          DT::datatable(table_to_display,
            editable = list(target = "cell", disable = list(columns = c(1:n_granul_cols, n_granul_cols + 2, n_granul_cols + 3))),
            options = list(
              lengthChange = FALSE, # Remove "Show XXX entries" option
              paging = FALSE, # Remove pagination
              searching = FALSE, # Remove search input
              info = FALSE, # Remove "Showing N of X entries"
              columnDefs = list( # Change colors of text in cells
                list(targets = (n_granul_cols + 2):(n_granul_cols + 3), createdCell = JS(
                  "function(cell, cellData, rowData) {
              $(cell).css('color', cellData < 0 ? 'red' : 'green');
            }"
                ))
              )
            )
          )
        },
        server = FALSE
      )
    })

    # TABLE INPUTS MGMT ===================================

    # Update data structure on cell edit
    observeEvent(input$portfolio_table_cell_edit, {
      n_granul_cols <- length(trisk_granularity_r())
      info <- input$portfolio_table_cell_edit
      portfolio_data <- portfolio_data_r()
      # data can be edited only in the second column
      if (info$col == (n_granul_cols + 1)) {
        # update the portfolio data with UI cell change
        displayed_display_columns <- display_columns[display_columns %in% colnames(portfolio_data)]
        portfolio_data[info$row, displayed_display_columns[info$col]] <- info$value
        portfolio_data_r(portfolio_data)
      }
      # Save the new portfolio state in the reactiveValues object
      # Update the portfolio data to the state corresponding to the current granularity
      trisk_granularity_names <- dplyr::intersect(names(max_trisk_granularity), colnames(portfolio_data_r()))
      trisk_granularity_names <- paste0(trisk_granularity_names, collapse = "-") # Convert to character vector
      portfolio_states[[trisk_granularity_names]] <- portfolio_data_r()
    })

    return(analysis_data_r)
  })
}
