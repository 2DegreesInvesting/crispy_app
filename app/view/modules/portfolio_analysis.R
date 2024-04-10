box::use(
  shiny[
    moduleServer, NS, reactiveVal, reactive, observeEvent, observe,
    selectizeInput, updateSelectizeInput, eventReactive,
    div, tags, reactiveValues, HTML
  ],
  semantic.dashboard[box, icon],
  DT[dataTableProxy, DTOutput, renderDT, datatable, JS],
  shiny.semantic[semanticPage, segment, button],
  shinyjs[runjs, useShinyjs]
)



box::use(
  app/logic/constant[DEFAULT_ASSET_EXPIRATION_DATE, FILTER_CRISPY_OUTLIERS],
  app/logic/renamings[rename_tibble_columns],
  app/view/modules/portfolio_edition
)




##################### UI

ui <- function(id, portfolio_class = "") {
  ns <- NS(id)
  box(
    title = portfolio_class, width = 16, collapsible = FALSE,
    DTOutput(outputId = ns("portfolio_table")),
    if (portfolio_class == "Loans Portfolio") {
      # show the Row Edition only on the Loans tab
      portfolio_edition$ui(ns("portfolio_edition"))
    }
  )
}


##################### Server


server <- function(
    id,
    portfolio_class,
    portfolio_uploaded_r,
    crispy_data_r,
    trisk_granularity_r,
    max_trisk_granularity,
    portfolio_asset_type, display_columns, editable_columns_names, colored_columns_names,
    possible_trisk_combinations = NULL,
    editable_rows = FALSE) {
  moduleServer(id, function(input, output, session) {
    # PORTFOLIO DATA =========================

    portfolio_data_r <- initialize_portfolio(
      crispy_data_r=crispy_data_r,
      trisk_granularity_r = trisk_granularity_r,
      portfolio_uploaded_r = portfolio_uploaded_r
    )

    # ATTACH ROWS EDITION MODULE =========================

    # possible_trisk_combinations is not null for the Loans tab
    if (!is.null(possible_trisk_combinations)) {
      if (portfolio_class == "Loans Portfolio") {
        portfolio_data_r <- portfolio_edition$server("portfolio_edition",
          portfolio_data_r = portfolio_data_r,
          crispy_data_r = crispy_data_r,
          trisk_granularity_r = trisk_granularity_r,
          max_trisk_granularity = max_trisk_granularity,
          possible_trisk_combinations = possible_trisk_combinations
        )
      }
    }

    # ANALYSIS DATA ===================================


    out <- generate_analysis_data(
      portfolio_data_r = portfolio_data_r,
      crispy_data_r = crispy_data_r
      )

    analysis_data_r <- out$analysis_data_r
    crispy_data_agg_r <- out$crispy_data_agg_r

    # TABLE DISPLAY IN UI ===================================

    display_analysis_data(
      output = output,
      analysis_data_r = analysis_data_r,
      display_columns = display_columns,
      editable_columns_names = editable_columns_names,
      colored_columns_names = colored_columns_names,
      trisk_granularity_r = trisk_granularity_r
    )

    # TABLE INPUTS MGMT ===================================

    update_portfolio_with_user_input(
      input = input,
      analysis_data_r = analysis_data_r,
      portfolio_data_r = portfolio_data_r,
      trisk_granularity_r = trisk_granularity_r,
      display_columns = display_columns,
      editable_columns_names = editable_columns_names,
      max_trisk_granularity = max_trisk_granularity
    )

    return(list(
      "analysis_data_r" = analysis_data_r,
      "crispy_data_agg_r" = crispy_data_agg_r
    ))
  })
}











##################### Modules


initialize_portfolio <- function(trisk_granularity_r, portfolio_uploaded_r, crispy_data_r) {
  # Initial portfolio data structure
  portfolio_data_r <- reactiveVal()

  shiny::observeEvent(c(trisk_granularity_r(), portfolio_uploaded_r(), crispy_data_r()), {
    if (nrow(portfolio_uploaded_r()) > 0){
      portfolio_data_r(portfolio_uploaded_r())
    } else {
    
    trisk_granularity <- trisk_granularity_r()

    # Create the portfolio dynamic columns data structure
    # TODO test all dynamic columns have to be character type 
    dynamic_cols <- stats::setNames(lapply(trisk_granularity, function(x) character()), trisk_granularity)
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
    # creates the empty portfolio data
    portfolio_data <- dplyr::bind_cols(dynamic_cols, static_cols)


    if ((!"company_id" %in% trisk_granularity)) {
      if(!is.null(crispy_data_r())){
        granularity <- dplyr::intersect(colnames(portfolio_data), colnames(crispy_data_r()))

        # in equities , populate the portfolio if empty, and not company granularity
        portfolio_data <- portfolio_data |>
          dplyr::right_join(
            crispy_data_r() |> dplyr::distinct_at(granularity)
          ) |>
          dplyr::mutate(
            expiration_date = DEFAULT_ASSET_EXPIRATION_DATE,
            portfolio_id = 1
          )
          }
    }
    
    portfolio_data_r(portfolio_data)
    }
  })

  return(portfolio_data_r)
}

generate_analysis_data <- function(portfolio_data_r, crispy_data_r) {
  analysis_data_r <- reactiveVal()
  crispy_data_agg_r <- reactiveVal()

  observeEvent(c(portfolio_data_r(), crispy_data_r()), {
    if (!is.null(portfolio_data_r()) & !is.null(crispy_data_r())) {
      granularity <- dplyr::intersect(colnames(portfolio_data_r()), colnames(crispy_data_r()))
      # Creates and aggregate Analysis data without portfolio with stress.test.plot.report fun
      if (nrow(portfolio_data_r() > 0)) {
        analysis_data <- stress.test.plot.report:::load_input_plots_data_from_tibble(
          portfolio_data = portfolio_data_r(),
          multi_crispy_data = crispy_data_r(),
          granularity = granularity,
          filter_outliers = FILTER_CRISPY_OUTLIERS
        ) |>
          dplyr::mutate(
            crispy_perc_value_change = round(.data$crispy_perc_value_change, digits = 4),
            crispy_value_loss = round(.data$crispy_value_loss, digits = 2),
            pd_shock = round(.data$pd_shock, digits = 4),
            expected_loss_shock = round(.data$expected_loss_shock, digits = 2)
          )
      } else {
        analysis_data <- dplyr::inner_join(
          portfolio_data_r(),
          crispy_data_r(),
          by = granularity
        ) |>
          dplyr::mutate(
            crispy_perc_value_change = NA,
            crispy_value_loss = NA,
            pd_shock = NA,
            expected_loss_shock = NA
          )
      }

      # Aggregate Crispy data without portfolio with stress.test.plot.report fun
      crispy_data_agg <- stress.test.plot.report:::main_load_multi_crispy_data(
        multi_crispy_data = crispy_data_r(),
        granularity = granularity,
        filter_outliers = FILTER_CRISPY_OUTLIERS
      ) |> dplyr::mutate(
        pd_baseline = round(pd_baseline, digits = 4),
        pd_shock = round(pd_shock, digits = 4),
        pd_difference = pd_shock - pd_baseline
      )

      analysis_data_r(analysis_data)
      crispy_data_agg_r(crispy_data_agg)
    }
  })

  return(list(
    "analysis_data_r" = analysis_data_r,
    "crispy_data_agg_r" = crispy_data_agg_r
  ))
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
          ),
          class = "display compact" # fit table to container
        )
      },
      server = FALSE
    )
  })
}

update_portfolio_with_user_input <- function(
    input,
    analysis_data_r,
    portfolio_data_r,
    trisk_granularity_r,
    display_columns,
    editable_columns_names,
    max_trisk_granularity) {
  # Update data structure on cell edit
  observeEvent(input$portfolio_table_cell_edit, {
    n_granul_cols <- length(trisk_granularity_r())
    info <- input$portfolio_table_cell_edit
    portfolio_data <- portfolio_data_r()
    # update the portfolio data with UI cell change
    displayed_display_columns <- display_columns[display_columns %in% colnames(analysis_data_r())]

    if (is.numeric(info$value)) {
      portfolio_data[info$row, displayed_display_columns[info$col]] <- info$value
      portfolio_data_r(portfolio_data)
    }
  })
}
