box::use(
  shiny[
    moduleServer, NS, observe, div, tags, reactiveVal, reactiveValues, eventReactive, p, tagList, observeEvent, img,
    HTML, conditionalPanel
  ],
  shiny.semantic[slider_input, dropdown_input, segment, update_dropdown_input, update_slider],
  shinyjs[useShinyjs],
  semantic.dashboard[dashboardSidebar]
)

box::use(
  app/logic/renamings[rename_string_vector]
)


ui <- function(id, max_trisk_granularity) {
  ns <- NS(id)
  # First segment in the left half // Granularity

  shiny::tagList(
    tags$div(
      class = "description",
      tags$div(
        class = "ui buttons",
        shinyjs::useShinyjs(), # Initialize shinyjs
        tags$head(
          tags$style(HTML("
      .ui.buttons .button { margin: 0; }
    "))
        ),
        shiny.semantic::button(
          ns("granul_1"),
          rename_string_vector(names(which(max_trisk_granularity == 1)), words_class = "analysis_columns"),
          class = "ui secondary button fluid"
        ),
        shiny.semantic::button(
          ns("granul_2"),
          rename_string_vector(names(which(max_trisk_granularity == 2)), words_class = "analysis_columns"),
          class = "ui button fluid"
        )
        # ,shiny.semantic::button(
        #   ns("granul_3"),
        #   rename_string_vector(names(which(max_trisk_granularity == 3)), words_class = "analysis_columns"),
        #   class = "ui primary button fluid")
      )
    )
  )
}
# get the column names defining the displayed data granularity
server <- function(id, max_trisk_granularity) {
  moduleServer(id, function(input, output, session) {
    # initialize trisk_granularity_r with the highest coarseness
    trisk_granularity_r <- reactiveVal(
      get_trisk_granularity(max_trisk_granularity, 1)
    )


    observeEvent(input$granul_1, {
      update_class(session$ns("granul_1"), "ui secondary button fluid")
      update_class(session$ns("granul_2"), "ui button fluid")
      # update_class(session$ns("granul_3"), "ui primary button fluid")
      trisk_granularity_r(
        get_trisk_granularity(max_trisk_granularity, 1)
      )
    })

    observeEvent(input$granul_2, {
      update_class(session$ns("granul_1"), "ui button fluid")
      update_class(session$ns("granul_2"), "ui secondary button fluid")
      # update_class(session$ns("granul_3"), "ui primary button fluid")
      trisk_granularity_r(
        get_trisk_granularity(max_trisk_granularity, 2)
      )
    })

    # observeEvent(input$granul_3, {
    #   update_class(session, "granul_1", "ui primary button fluid")
    #   update_class(session, "granul_2", "ui primary button fluid")
    #   update_class(session, "granul_3", "ui green button fluid")
    #   trisk_granularity_r(names(which(max_trisk_granularity == 3)))
    # })


    return(trisk_granularity_r)
  })
}


# will return a vector of the column names defining the displayed data granularity
# or a single value if the granularity is the highest
get_trisk_granularity <- function(max_trisk_granularity, granularity_level) {
  granul_and_lower <- sapply(max_trisk_granularity, function(value) value <= granularity_level)
  trisk_granularity <- names(max_trisk_granularity)[granul_and_lower]
  return(trisk_granularity)
}

# Function to update button classes, now correctly utilized
update_class <- function(input_id, class) {
  shinyjs::runjs(sprintf("$('#%s').attr('class', '%s');", input_id, class))
}
