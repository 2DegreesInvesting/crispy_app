box::use(
  shiny[
    moduleServer, NS, reactiveVal, reactive, observeEvent, observe,
    selectizeInput, updateSelectizeInput, eventReactive,
    div, tags, reactiveValues, HTML
  ],
  semantic.dashboard[box],
  DT[dataTableProxy],
  shiny.semantic[semanticPage, button, segment],
  shinyjs[runjs, useShinyjs]
)

box::use(
  app / view / portfolio / portfolio_search
)


####### UI

ui <- function(id) {
  ns <- NS(id)

  div(
    style = "display: flex; flex-wrap: wrap;", # Flex container
    portfolio_search$ui(ns("company_name_portfolio_search"), offer_options = FALSE),
    portfolio_search$ui(ns("ald_business_unit_portfolio_search"), offer_options = TRUE),
    portfolio_search$ui(ns("ald_sector_portfolio_search"), offer_options = TRUE),
    button(ns("add_row_btn"), "Add new row", class = "ui button"),
    button(ns("delete_row_btn"), "Delete Selected Rows", class = "ui button")
  )
}

####### Server

# id should be the exact same as the parent id !!
server <- function(id, trisk_granularity_r, portfolio_data_r, crispy_data_r, possible_trisk_combinations) {
  moduleServer(id, function(input, output, session) {
    # ADD ROW =================

    company_choices_r <- reactive({
      unique(crispy_data_r()$company_name)
    })
    bu_choices_r <- reactive({
      unique(crispy_data_r()$ald_business_unit)
    })

    picked_company_name_r <- portfolio_search$server("company_name_portfolio_search", variable_choices_r = company_choices_r)
    picked_ald_business_unit_r <- portfolio_search$server("ald_business_unit_portfolio_search", variable_choices_r = bu_choices_r)
    picked_ald_sector <- portfolio_search$server("ald_sector_portfolio_search", variable_choices_r = bu_choices_r)

    # EDIT ROWS =================

    # BUTTONS ADD ROWS
    # add a new row by creating it in the portfolio
    observeEvent(input$add_row_btn, {
      browser()
      if (!is.null(picked_company_name()) && !is.null(picked_ald_business_unit())) {
        user_defined_row <- tibble::as_tibble(list(
          company_name = ifelse(is.null(picked_company_name()), NA, picked_company_name()),
          ald_business_unit = ifelse(is.null(picked_ald_business_unit()), NA, picked_ald_business_unit())
        )) |>
          dplyr::select_at(dplyr::intersect(names(.data), names(portfolio_data_r())))

        updated_portfolio_data <- dplyr::bind_rows(
          portfolio_data_r(),
          user_defined_row
        )
        portfolio_data_r(updated_portfolio_data)
      }
    })



    # THIS ID REFERS TO THE TABLE ABOVE AND SHOULD BE USED SIMULTANEOUSLY
    # WITH ANOTHER MODULE IN CHARGE OF A PORTFOLIO
    # In order to do that, the id of the table should be passed as an argument to the module
    proxy <- dataTableProxy(id)

    # Delete row
    observeEvent(input$delete_row_btn, {
      selected_row <- input$portfolio_table_rows_selected
      if (length(selected_row)) {
        my_data_data <- portfolio_data_r()
        my_data_data <- my_data_data[-selected_row, , drop = FALSE]
        portfolio_data_r(my_data_data)
        replaceData(proxy, my_data_data, resetPaging = FALSE)
      }
    })
  })
}
