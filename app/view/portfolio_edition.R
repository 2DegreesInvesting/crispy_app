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
  app/logic/constant[max_trisk_granularity, equity_portfolio_expiration_date, filter_crispy_outliers],
  app/logic/renamings[rename_tibble_columns]
)


##################### UI

ui <- function(id) {
  ns <- NS(id)

  tags$div(
    class = "ui grid container", # Main grid container for layout
    style = "padding: 20px;", # Add some padding around the container
    div(
      class = "row",
      div(
        class = "sixteen wide column",
        shiny.semantic::dropdown_input(
          ns("ald_sector_dropdown"),
          default_text = "Sector",
          choices = NULL # Populate with your choices
        ),
        shiny.semantic::dropdown_input(
          ns("ald_business_unit_dropdown"),
          default_text = "Business Unit",
          choices = NULL # Populate with your choices
        ),
        shiny.semantic::dropdown_input(
          ns("maturity_year"),
          default_text = "Year of maturity",
          choices = 2024:2034, # TODO GO IN CONF
          value = 2034
        )
      )
    ),
    div(
      class = "row",
      div(
        class = "eight wide column",
        shiny.semantic::button(
          ns("add_row_btn"),
          "Add new row",
          icon = icon("plus"), ,
          class = "ui button fluid"
        )
      ),
      div(
        class = "eight wide column",
        shiny.semantic::button(
          ns("delete_row_btn"),
          "Delete Selected Rows",
          icon = icon("delete"),
          class = "ui button fluid"
        )
      )
    )
  )
}


##################### Server


server <- function(
    id,
    portfolio_data_r,
    crispy_data_r,
    trisk_granularity_r,
    max_trisk_granularity,
    possible_trisk_combinations = NULL) {
  moduleServer(id, function(input, output, session) {
    # ADD ROW =================

    selected_maturity_year_r <- reactive({
      input$maturity_year
    })

    selected_ald_sector_r <- reactive({
      choice <- input$ald_sector_dropdown
      # renamed_choice <- rename_string_vector(choice, words_class = "scenarios", dev_to_ux = FALSE)
      # return(renamed_choice)
      return(choice)
    })
    selected_ald_business_unit_r <- reactive({
      choice <- input$ald_business_unit_dropdown
      # renamed_choice <- rename_string_vector(choice, words_class = "scenarios", dev_to_ux = FALSE)
      # return(renamed_choice)
      return(choice)
    })


    # synchronise dropdown choices  with the possible combinations
    # at the same time get the company query return value to update selected_company_name_r
    # TODO selected_company_name_r kept as legacy for later reactivation of the company
    selected_company_name_r <- update_ald_dropdowns(
      input = input,
      session = session,
      trisk_granularity_r = trisk_granularity_r,
      crispy_data_r = crispy_data_r
    )

    # EDIT ROWS =================

    # BUTTONS ADD ROWS

    portfolio_data_r <- rows_addition(
      input = input,
      portfolio_data_r = portfolio_data_r,
      selected_ald_business_unit_r = selected_ald_business_unit_r,
      selected_ald_sector_r = selected_ald_sector_r,
      selected_maturity_year_r = selected_maturity_year_r
    )

    # Delete row

    portfolio_data_r <- rows_deletion(
      input,
      portfolio_data_r = portfolio_data_r,
      selected_rows = selected_rows
    )
  })
}

rows_addition <- function(input, portfolio_data_r, selected_ald_business_unit_r, selected_ald_sector_r, selected_maturity_year_r) {
  # add a new row by creating it in the portfolio
  observeEvent(input$add_row_btn, {
    user_defined_row <- tibble::as_tibble(list(
      # company_id = ifelse(is.null(selected_company_name_r()), NA, selected_company_name_r()),
      ald_business_unit = ifelse(is.null(selected_ald_business_unit_r()), NA, selected_ald_business_unit_r()),
      ald_sector = ifelse(is.null(selected_ald_sector_r()), NA, selected_ald_sector_r()),
      expiration_date = paste0(as.character(selected_maturity_year_r()), "-01-01")
    ))

    use_columns <- dplyr::intersect(names(user_defined_row), names(portfolio_data_r()))
    user_defined_row <- user_defined_row |>
      dplyr::select_at(use_columns)

    updated_portfolio_data <- dplyr::bind_rows(
      portfolio_data_r(),
      user_defined_row
    )
    portfolio_data_r(updated_portfolio_data)
  })

  # proxy <- DT::dataTableProxy(id)
  return(portfolio_data_r)
}


rows_deletion <- function(
    input,
    portfolio_data_r,
    selected_rows) {
  observeEvent(input$delete_row_btn, {
    selected_rows <- input$portfolio_table_rows_selected

    if (length(selected_rows)) {
      my_data_data <- portfolio_data_r()
      my_data_data <- my_data_data[-selected_rows, , drop = FALSE]
      portfolio_data_r(my_data_data)
      # DT::replaceData(proxy, my_data_data, resetPaging = FALSE)
    }
  })
  return(portfolio_data_r)
}

# Synchronise the scenarios available depending on user scenario choice
update_ald_dropdowns <- function(input, session,
                                 crispy_data_r,
                                 trisk_granularity_r) {
  # Observe changes in possible_trisk_combinations and update baseline_scenario dropdown
  observeEvent(crispy_data_r(), ignoreInit = TRUE, {
    possible_sectors <- unique(crispy_data_r()$ald_sector)

    # rename the scenarios to front end appropriate name
    # new_choices <- rename_string_vector(possible_shocks, words_class = "scenarios")
    new_choices <- possible_sectors
    # Update shock_scenario dropdown with unique values from the filtered data
    shiny.semantic::update_dropdown_input(session, "ald_sector_dropdown", choices = new_choices)
  })

  # Observe changes in baseline_scenario dropdown and update shock_scenario dropdown
  observeEvent(c(input$ald_sector_dropdown, crispy_data_r()), ignoreInit = TRUE, {
    if (is.null(input$ald_sector_dropdown) || length(input$ald_sector_dropdown) == 0) {
      return() # Skip further execution if no selection is made
    }
    if ("ald_business_unit" %in% trisk_granularity_r()) {
      possible_ald_business_units <- crispy_data_r() |> dplyr::filter(ald_sector == input$ald_sector_dropdown)
      possible_ald_business_units <- unique(possible_ald_business_units$ald_business_unit)
    } else {
      possible_ald_business_units <- c("")
    }
    shiny.semantic::update_dropdown_input(
      session,
      "ald_business_unit_dropdown",
      choices = possible_ald_business_units
    )
  })
}
