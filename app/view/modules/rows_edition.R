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
    selectizeInput(ns("pick_business_unit"),
      label = NULL,
      choices = NULL,
      selected = NULL,
      options = list(
        placeholder = "Pick a business unit",
        onInitialize = I("function() { this.setValue(''); }"),
        score = I("function(search) {
        if (!search || search.length === 0) {
          return returnAllScore();
        }
        return this.getScoreFunction(search); // Default behavior when there's input
      }")
      )
    )
    # ,
    # selectizeInput(ns("pick_country"),
    #   label = NULL,
    #   choices = NULL,
    #   selected = NULL,
    #   options = list(
    #     placeholder = "Pick country",
    #     onInitialize = I("function() { this.setValue(''); }"),
    #     score = I("function(search) {
    #     if (!search || search.length === 0) {
    #       return returnAllScore();
    #     }
    #     return this.getScoreFunction(search); // Default behavior when there's input
    #   }")
    #   )
    # ),
    # button(ns("add_row_btn"), "Add new row", class = "ui button"),
    # button(ns("delete_row_btn"), "Delete Selected Rows", class = "ui button")
  )
}

####### Server

server <- function(id, portfolio_data_r, crispy_data_r, trisk_input_path) {
  moduleServer(id, function(input, output, session) {
    # ADD ROW =================

    possible_trisk_combinations <- r2dii.climate.stress.test::get_scenario_geography_x_ald_sector(trisk_input_path)

    company_choices_r <- reactive({
      unique(crispy_data_r()$company_name)
    })
    business_unit_choices_r <- reactive({
      unique(crispy_data_r()$business_unit)
    })
    current_input_r <- reactiveVal("") # Reactive value to track the current input

    observe({
      # Update the selectize input with the new filtered choices
      updateSelectizeInput(
        session,
        "pick_company_name",
        choices = get_filtered_choices(current_input_r(), company_choices_r()),
        selected = current_input(),
        server = TRUE
      )
    })

    observe({
      # Update the selectize input with the new filtered choices
      updateSelectizeInput(
        session,
        "pick_business_unit",
        choices = get_filtered_choices(current_input_r(), business_unit_choices_r()),
        selected = current_input(),
        server = TRUE
      )
    })

    observe({
      # Update the selectize input with the new filtered choices
      updateSelectizeInput(
        session,
        "pick_country",
        choices = get_filtered_choices(current_input_r(), all_choices),
        selected = current_input(),
        server = TRUE
      )
    })

    # add a new row by creating it in the portfolio
    observeEvent(input$add_btn, {
      portfolio_data_r(
        dplyr::bind_rows(
          portfolio_data_r(),
          tibble::as_tibble(list(ald_sector = input$category_input))
        )
      )
      updateTextInput(session, "category_input", value = "") # Clear the input
      current_input("") # Reset the current input tracker
    })


    # DELETE ROWS =================

    proxy <- dataTableProxy("portfolio_table")

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
