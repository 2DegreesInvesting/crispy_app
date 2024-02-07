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
  app/view/modules/search_module)


####### UI

ui <- function(id) {
  ns <- NS(id)

  tags$div(
    style = "display: flex; flex-wrap: nowrap; width: 100%; align-items: center;", # Flex container
    search_module$ui(ns("company_name_search_module"), offer_options = FALSE),
    search_module$ui(ns("ald_business_unit_search_module"), offer_options = TRUE),
    button(ns("add_row_btn"), "Add new row", class = "ui button"),
    button(ns("delete_row_btn"), "Delete Selected Rows", class = "ui button")
  )
}

####### Server

server <- function(id, parent_portfolio_id, portfolio_data_r, crispy_data_r, trisk_input_path) {
  moduleServer(id, function(input, output, session) {


    # ADD ROW =================

    possible_trisk_combinations <- r2dii.climate.stress.test::get_scenario_geography_x_ald_sector(trisk_input_path)

    company_choices_r <- reactive({
      unique(crispy_data_r()$company_name)
    })
    bu_choices_r <- reactive({
      unique(crispy_data_r()$ald_business_unit)
    })


    picked_company_name = search_module$server("company_name_search_module", variable_choices_r=company_choices_r)
    picked_ald_business_unit <- search_module$server("ald_business_unit_search_module", variable_choices_r=bu_choices_r)
    # EDIT ROWS =================


    # BUTTONS ADD ROWS
    # add a new row by creating it in the portfolio
    observeEvent(input$add_btn, {
      browser()
      user_defined_row <- tibble::as_tibble(list(
          company_name = picked_company_name(),
          ald_business_unit = picked_ald_business_unit()
          )) |>
          dplyr::select_at(dplyr::intersect(names(.data), names(portfolio_data_r())))
      
      updated_portfolio_data <- dplyr::bind_rows(
            portfolio_data_r(),
            user_defined_row()
      )
      portfolio_data_r(updated_portfolio_data)
    })


    
    # THIS ID REFERS TO THE TABLE ABOVE AND SHOULD BE USED SIMULTANEOUSLY 
    # WITH ANOTHER MODULE IN CHARGE OF A PORTFOLIO
    # In order to do that, the id of the table should be passed as an argument to the module
    proxy <- dataTableProxy(id) 
    
    # Delete row
    observeEvent(input$delete_btn, {
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

