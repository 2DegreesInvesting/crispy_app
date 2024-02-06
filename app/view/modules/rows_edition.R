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
  app/view/modules/search_module,
  app / logic / company_search[get_filtered_choices]
)


####### UI

ui <- function(id) {
  ns <- NS(id)

  tags$div(
    style = "display: flex; flex-wrap: nowrap; width: 100%; align-items: center;", # Flex container
    useShinyjs(),
    tags$head(
      tags$script(HTML("
      // Custom scoring function to return all choices irrespective of search input
      function returnAllScore() {
        return function(item) {
          return 1; // Return the same score for all items
        };
      }
    "))
    ),
      tags$head(
    tags$script(HTML(paste0("
      $(document).on('shiny:connected', function(event) {
        // Function to track input on 'company_input' selectize
        $('#",ns("pick_company_name"),"').on('keyup', function() {
          var value = $('#pick_company_name')[0].selectize.$control_input.val();
          Shiny.onInputChange('company_typing', value);
        });
      });
    ")))
  ),
    selectizeInput(ns("pick_company_name"),
      label = NULL,
      choices = NULL,
      selected = NULL,
      options = list(
        placeholder = "Search companies...",
        onInitialize = I("function() { this.setValue(''); }"),
        score = I("function(search) {
        if (!search || search.length === 0) {
          return returnAllScore();
        }
        return this.getScoreFunction(search); // Default behavior when there's input
      }")
      )
    ),
    # selectizeInput(ns("pick_business_unit"),
    #   label = NULL,
    #   choices = NULL,
    #   selected = NULL,
    #   options = list(
    #     placeholder = "Pick a business unit",
    #     onInitialize = I("function() { this.setValue(''); }"),
    #     score = I("function(search) {
    #     if (!search || search.length === 0) {
    #       return returnAllScore();
    #     }
    #     return this.getScoreFunction(search); // Default behavior when there's input
    #   }")
    #   )
    # ),
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
    # Reactive value to track the current input
    # used to filter the selectize input choices
    current_company_choices_input_r <- reactive({input$company_typing})
    picked_company_r <- reactiveVal("")
    
    observeEvent(current_company_choices_input_r(), {
      if (!is.null(company_choices_r())){
        company_choices <- company_choices_r()
              } else{
                company_choices <- c("No company found")
              }
              browser()
      # Update the selectize input with the new filtered choices
      updateSelectizeInput(
        session,
        "pick_company_name",
        choices = get_filtered_choices(search_term=current_company_choices_input_r(), company_choices),
        selected = current_input_r(),
        server = TRUE

      )
    })

    # Track 'pick_company_name' input
    observeEvent(input$pick_company_name, {
      current_company <- input$pick_company_name
      # Perform actions based on the selected company
      # For example, print the currently selected company
      picked_company_r(current_company)
    })

    # business_unit_choices_r <- reactive({
    #   unique(crispy_data_r()$business_unit)
    # })

    # # Track 'pick_business_unit' input
    # observeEvent(input$pick_business_unit, {
    #   current_business_unit <- input$pick_business_unit
    #   # Perform actions based on the selected business unit
    #   # For example, print the currently selected business unit
    #   print(paste("Current selected business unit is:", current_business_unit))
    # })
    # observeEvent(current_input_r(), {
    #         if (!is.null(business_unit_choices_r())){
    #     business_unit_choices <- business_unit_choices_r()
    #           } else{
    #             business_unit_choices <- c("No business unit found")
    #           }
    #   # Update the selectize input with the new filtered choices
    #   updateSelectizeInput(
    #     session,
    #     "pick_business_unit",
    #     choices = get_filtered_choices(search_term=current_input_r(), business_unit_choices),
    #     selected = current_input_r(),
    #     server = TRUE
    #   )      
    # })


    # EDIT ROWS =================


    # BUTTONS ADD ROWS
    # add a new row by creating it in the portfolio
    observeEvent(input$add_btn, {
      browser()
      portfolio_data_r(
        dplyr::bind_rows(
          portfolio_data_r(),
          tibble::as_tibble(list(ald_sector = input$category_input))
        )
      )
      updateTextInput(session, "category_input", value = "") # Clear the input
      current_input("") # Reset the current input tracker
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

