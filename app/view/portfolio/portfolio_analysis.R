# Contains the portfolio display functions
# Is extended by portfolio_edition module
# merge the portfolio with crispy data, and aggregate to the
# granularity defined in the sidebar_parameter module, creating the analysis_data to feed plots

box::use(
  shiny[moduleServer, NS, reactiveVal, reactive, observeEvent, observe, selectizeInput, eventReactive, div, tags, reactiveValues],
  semantic.dashboard[box],
  DT[DTOutput, renderDT, datatable, JS],
  shiny.semantic[semanticPage, button, segment]
)

box::use(
  app / view / portfolio / portfolio_edition,
  app / logic / constant[max_trisk_granularity],
  app / logic / renamings[rename_tibble_columns]
)


##################### UI

ui <- function(id, title = "") {
  ns <- NS(id)
  box(
    title = title, width = 16, collapsible = FALSE,
    DTOutput(outputId = ns("portfolio_table")),
    if (title == "Loans Portfolio") {
      # show the row editing only on the Loans tab
      portfolio_edition$ui(ns("portfolio_table"))
    }
  )
}


##################### Server


server <- function(
    id,
    crispy_data_r,
    trisk_granularity_r,
    max_trisk_granularity,
    portfolio_asset_type, display_columns, editable_columns_names, colored_columns_names,
    possible_trisk_combinations = NULL,
    editable_rows = FALSE) {
  moduleServer(id, function(input, output, session) {
    # PORTFOLIO DATA =========================

    # Create a reactiveValues object to store the portfolio states
    # is used to keep track of the portfolio data for each granularity
    portfolio_states <- reactiveValues()

    portfolio_data_r <- initialize_portfolio(trisk_granularity_r = trisk_granularity_r, portfolio_states = portfolio_states)

    # ATTACH ROWS EDITION MODULE =========================

    # possible_trisk_combinations is not null for the Loans tab
    if (!is.null(possible_trisk_combinations)) {
      portfolio_data_r <- portfolio_edition$server(
        "portfolio_table",
        portfolio_data_r = portfolio_data_r,
        crispy_data_r = crispy_data_r,
        possible_trisk_combinations = possible_trisk_combinations
      )
    }

    # ANALYSIS DATA ===================================

    analysis_data_r <- generate_analysis_data(
      portfolio_data_r = portfolio_data_r, 
      crispy_data_r = crispy_data_r, 
      portfolio_asset_type = portfolio_asset_type
      )

    # TABLE DISPLAY IN UI ===================================

    display_analysis_data(
      output=output,
      analysis_data_r = analysis_data_r, 
      display_columns = display_columns, 
      editable_columns_names = editable_columns_names, 
      colored_columns_names = colored_columns_names, 
      trisk_granularity_r=trisk_granularity_r
      )

    # TABLE INPUTS MGMT ===================================

    update_portfolio_with_user_input(input, portfolio_data_r, trisk_granularity_r, display_columns, portfolio_states, max_trisk_granularity)

    return(analysis_data_r)
  })
}











##################### Modules


initialize_portfolio <- function(trisk_granularity_r, portfolio_states) {
  # Initial portfolio data structure
  portfolio_data_r <- reactiveVal()

  observe({
    trisk_granularity_names <- paste0(trisk_granularity_r(), collapse = "-") # Convert to character vector

    # If the portfolio state for the current granularity doesn't exist, create it
    if (!(trisk_granularity_names %in% names(portfolio_states))) {
      # Create the portfolio dynamic columns data structure
      # all dynamic columns will be character type
      dynamic_cols <- stats::setNames(lapply(trisk_granularity_r(), function(x) character()), trisk_granularity_r())
      dynamic_cols <- dplyr::as_tibble(dynamic_cols)

      # define static columns that are required at all times (for functional or computational purposes)
      static_cols <- tibble::tibble(
        portfolio_id = character(), # is always 1
        asset_type = character(), # fixed_income for Crispy Loans, equities for Crispy Equities
        exposure_value_usd = numeric(),
        loss_given_default = numeric(), # is always NA for Crispy Equities
        expiration_date = character(), # is always NA for Crispy Equities
        pd_portfolio = numeric() # is always NA for Crispy Equities
      )
      # creates the port
      portfolio_data <- dplyr::bind_cols(dynamic_cols, static_cols)
      portfolio_data_r(portfolio_data)

      # Save the new portfolio state in the reactiveValues object
      # Update the portfolio data to the state corresponding to the current granularity
      portfolio_states[[trisk_granularity_names]] <- portfolio_data_r()
    } else {
      portfolio_data_r(portfolio_states[[trisk_granularity_names]])
    }
  })

  return(portfolio_data_r)
}

generate_analysis_data <- function(portfolio_data_r, crispy_data_r, portfolio_asset_type) {
  analysis_data_r <- reactiveVal()

  observe({
    if (!is.null(portfolio_data_r()) & !is.null(crispy_data_r())) {
                  granularity <- dplyr::intersect(colnames(portfolio_data_r()), colnames(crispy_data_r()))

      

      
        # initialize the portfolio with a unique portfolio id (and it will always be unique in CRISPY)
        # unique per portfolio type and granularity
        portfolio_data <- portfolio_data_r()

        # populate the empty portfolio if in equities
        if (portfolio_asset_type == "equities") {
          if (nrow(portfolio_data_r()) == 0) {

            portfolio_data <- portfolio_data |>
              dplyr::right_join(
                crispy_data_r() |> dplyr::distinct_at(granularity)
              ) |>
              dplyr::mutate(
                term = 1
              )
            }
        }
          
        portfolio_data <- portfolio_data |>
          dplyr::mutate(
            # we don't compare portfolios in the app so it's always same id
            portfolio_id = "1",
            # portfolio_asset_type : equities or fixed_income
            asset_type = portfolio_asset_type
          )
        
        portfolio_data_r(portfolio_data)
      

if (nrow(portfolio_data_r() > 0)){
      analysis_data <- stress.test.plot.report:::load_input_plots_data_from_tibble(
        portfolio_data = portfolio_data_r(),
        multi_crispy_data = crispy_data_r(),
        granularity = granularity
      ) |>
        dplyr::mutate(
          crispy_perc_value_change = round(crispy_perc_value_change, digits = 4),
          crispy_value_loss = round(crispy_value_loss, digits = 2),
          crispy_pd_diff = round(pd_difference, digits = 4)
        )}
        else{
          analysis_data <- dplyr::inner_join(
            portfolio_data_r(),
            crispy_data_r(),
            by=granularity
          )
        }
      analysis_data_r(analysis_data)
    }
  })

  return(analysis_data_r)
}

display_analysis_data <- function(output, analysis_data_r, display_columns, editable_columns_names, colored_columns_names, trisk_granularity_r) {
  observeEvent(analysis_data_r(), ignoreInit = TRUE, {
    table_to_display <- analysis_data_r() |>
      dplyr::select(
        dplyr::any_of(display_columns)
      )

    disabled_columns <- which(!colnames(table_to_display) %in% editable_columns_names)
    colored_columns <- which(colnames(table_to_display) %in% colored_columns_names)

    table_to_display <- rename_tibble_columns(table_to_display, words_class = "analysis_columns")

    n_granul_cols <- length(trisk_granularity_r())
    # Render the editable table
    output$portfolio_table <- DT::renderDT(
      {
        DT::datatable(table_to_display,
          editable = list(target = "cell", disable = list(columns = disabled_columns)),
          options = list(
            lengthChange = FALSE, # Remove "Show XXX entries" option
            paging = FALSE, # Remove pagination
            searching = FALSE, # Remove search input
            info = FALSE, # Remove "Showing N of X entries"
            columnDefs = list( # Change colors of text in cells
              list(targets = colored_columns, createdCell = JS(
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
}

update_portfolio_with_user_input <- function(input, portfolio_data_r, trisk_granularity_r, display_columns, portfolio_states, max_trisk_granularity) {
  # Update data structure on cell edit
  observeEvent(input$portfolio_table_cell_edit, {
    n_granul_cols <- length(trisk_granularity_r())
    info <- input$portfolio_table_cell_edit
    portfolio_data <- portfolio_data_r()
    # data can be edited only in the second column
    if (info$col == (n_granul_cols + 1)) {
      # update the portfolio data with UI cell change
      displayed_display_columns <- display_columns[display_columns %in% colnames(portfolio_data)]
      if (is.numeric(info$value)) {
        portfolio_data[info$row, displayed_display_columns[info$col]] <- info$value
        portfolio_data_r(portfolio_data)
      }
    }

    # build name of portfolio in the reactiveValues object portfolio_states
    trisk_granularity_names <- dplyr::intersect(names(max_trisk_granularity), colnames(portfolio_data_r()))
    trisk_granularity_names <- paste0(trisk_granularity_names, collapse = "-") # Convert to character vector
    # Save the new portfolio state in portfolio_states
    # Update the portfolio data to the state corresponding to the current granularity
    portfolio_states[[trisk_granularity_names]] <- portfolio_data_r()
  })
}
