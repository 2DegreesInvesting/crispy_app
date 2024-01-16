box::use(
  shiny[moduleServer, NS, reactiveVal, observeEvent, observe, eventReactive, div],
  semantic.dashboard[box],
  DT[DTOutput, renderDT, datatable, JS]
)

box::use(
  app / logic / constant[max_crispy_granularity, portfolio_crispy_merge_cols],
  app / logic / ui_renaming[rename_tibble_columns]
)





####### UI

ui <- function(id) {
  ns <- NS(id)
  # First row with 3 taking the entire page width
  box(width = 16, DTOutput(outputId = ns("portfolio_table")))
}

####### Server


server <- function(id, crispy_data_r) {
  moduleServer(id, function(input, output, session) {
    # Initial portfolio data structure
    portfolio_data_r <- reactiveVal({
      tibble::tibble(
        ald_sector = character(),
        exposure_value_usd = numeric(),
        pd_portfolio = numeric(),
        loss_given_default = numeric()
      )
    })

    analysis_data_r <- eventReactive(c(
      portfolio_data_r(),
      crispy_data_r()
    ), ignoreInit = TRUE, {
      if (!is.null(portfolio_data_r()) & !is.null(crispy_data_r())) {
        if (nrow(portfolio_data_r()) == 0) {
          # initialise the porfolio sector column
          portfolio_data <- portfolio_data_r()
          portfolio_data <- portfolio_data |>
            dplyr::right_join(crispy_data_r() |>
              dplyr::distinct(.data$ald_sector))
          portfolio_data_r(portfolio_data)
        }

        stress.test.plot.report::main_load_analysis_data(
          portfolio_data = portfolio_data_r(),
          multi_crispy_data = crispy_data_r(),
          portfolio_crispy_merge_cols = portfolio_crispy_merge_cols
        ) |>
          dplyr::mutate(
            crispy_perc_value_change = round(crispy_perc_value_change, digits = 4),
            crispy_value_loss = round(crispy_value_loss, digits = 2)
          )
      }
    })

    observeEvent(analysis_data_r(), ignoreInit = TRUE, {
      table_to_display <- analysis_data_r() |>
        dplyr::select(
          .data$portfolio.ald_sector,
          .data$portfolio.exposure_value_usd,
          .data$crispy_perc_value_change,
          .data$crispy_value_loss
        )
      table_to_display <- rename_tibble_columns(table_to_display, class = "analysis_columns")

      # Render the editable table
      output$portfolio_table <- renderDT(
        {
          datatable(table_to_display,
            editable = list(target = "cell", disable = list(columns = c(1, 3, 4))),
            options = list(
              lengthChange = FALSE, # Remove "Show XXX entries" option
              paging = FALSE, # Remove pagination
              searching = FALSE, # Remove search input
              info = FALSE, # Remove "Showing N of X entries"
              columnDefs = list( # Change colors of text in cells
                list(targets = 3:4, createdCell = JS(
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

    # Update data structure on cell edit
    observeEvent(input$portfolio_table_cell_edit, {
      info <- input$portfolio_table_cell_edit
      portfolio_data <- portfolio_data_r()
      # data can be edited only in the second column
      if (info$col == 2) {
        if ((typeof(info$value) == "integer") |
          (typeof(info$value) == "double")) {
          portfolio_data[info$row, info$col] <- info$value
          portfolio_data_r(portfolio_data)
        }
      }
    })

    return(analysis_data_r)
  })
}
