box::use(
  shiny[
    moduleServer, NS, reactiveVal, reactive, observeEvent, observe,
    selectizeInput, updateSelectizeInput, eventReactive,
    div, tags, reactiveValues, HTML
  ],
  semantic.dashboard[box],
  DT[dataTableProxy],
  shiny.semantic[semanticPage, segment],
  shinyjs[runjs, useShinyjs]
)

box::use(
  app / view / portfolio / simple_search_dropdown
)


####### UI

ui <- function(id) {
  ns <- NS(id)

  div(
    style = "display: flex; flex-wrap: wrap;", # Flex container
    
    div(class = "ui grid",
      div(class = "sixteen wide column",

          shiny.semantic::dropdown_input(ns("ald_business_unit_dropdown"), 
            choices = NULL,
            default_text = "Sector"),
          shiny.semantic::dropdown_input(ns("ald_business_unit_dropdown"), 
            choices = NULL,
            default_text = "Business Unit"),

          simple_search_dropdown$ui(ns("company_name_simple_search_dropdown"))
      

        ),
      div(class = "row",
        div(class = "eight wide column",

          shiny.semantic::button(ns("add_row_btn"), "Add new row", class = "ui button"),
             ),
        div(class = "eight wide column",
          shiny.semantic::button(ns("delete_row_btn"), "Delete Selected Rows", class = "ui button")
        )
      )
    )          
  )
}

####### Server

# id should be the exact same as the parent id !!
server <- function(id, trisk_granularity_r, portfolio_data_r, crispy_data_r, possible_trisk_combinations) {
  moduleServer(id, function(input, output, session) {
    # ADD ROW =================

    selected_ald_sector <- reactive({
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
    update_ald_dropdowns(
      input=input,
      session=session,
      selected_company_name_r=selected_company_name_r,
      use_ald_sector=use_ald_sector, # TODO INTEGRATE USE_ALD_SECTOR INTO HIDE_VARS
      crispy_data_r=crispy_data_r
    )

    # EDIT ROWS =================

    # BUTTONS ADD ROWS
    # add a new row by creating it in the portfolio
    observeEvent(input$add_row_btn, {
      browser()
      if (!is.null(selected_ald_business_unit_r()) && !is.null(selected_ald_business_unit_r())) {
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


# Synchronise the scenarios available depending on user scenario choice
update_ald_dropdowns <- function(input, session,
                                       crispy_data_r,
                                       use_ald_sector,
                                        selected_company_name_r
                                       ) {
  
  # Observe changes in possible_trisk_combinations and update baseline_scenario dropdown
  observeEvent(crispy_data_r(), ignoreInit = TRUE, {
    possible_sectors <- crispy_data_r() |>
      dplyr::distinct(.data$ald_sector) |>
      dplyr::pull()

      # rename the scenarios to front end appropriate name
    # new_choices <- rename_string_vector(possible_shocks, words_class = "scenarios")
    new_choices <- possible_sectors
    # Update shock_scenario dropdown with unique values from the filtered data
    shiny.semantic::update_dropdown_input(session, "ald_sector_dropdown", choices = new_choices)
  })

  # Observe changes in baseline_scenario dropdown and update shock_scenario dropdown
  observeEvent(input$ald_sector_dropdown, ignoreInit = TRUE, {
    possible_ald_business_units <- crispy_data_r() |>
      dplyr::filter(,
        ald_sector == input$ald_sector_dropdown) |>
      dplyr::distinct(.data$ald_business_unit) |>
      dplyr::pull()
    
    shiny.semantic::update_dropdown_input(session, "ald_business_unit_dropdown", choices = possible_ald_business_units)
  })

  # Observe changes in both baseline_scenario and shock_scenario dropdowns to update scenario_geography dropdown
  observeEvent(input$shock_scenario, ignoreInit = TRUE, {
    
    company_choices_r <- reactive({
      if (!is.null(crispy_data_r())){
        filtered_crispy <- crispy_data_r() |>
          dplyr::filter(
            .data$ald_sector == ifelse(!is.null(selected_ald_sector()), selected_ald_sector(), NA),
            .data$ald_business_unit == ifelse(!is.null(
              selected_ald_business_unit_r()), 
              selected_ald_business_unit_r(), 
              NA)
          ) 
          browser()
          return(unique(filtered_crispy$company_id))
        }
    })

    selected_company_name_r <- simple_search_dropdown$server(
      "company_name_simple_search_dropdown", 
      variable_choices_r = company_choices_r
      )

  })
}



