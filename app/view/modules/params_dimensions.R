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

  shiny::tagList(
    tags$head(
      tags$script(HTML(sprintf("
        $(document).ready(function() {
          $('#%s').css({'background-color': '#000000', 'color': '#FFFFFF'});
        });
      ", ns("granul_1"))))
    ),
    tags$div(
      class = "description",
      tags$div(
        class = "ui buttons",
        shinyjs::useShinyjs(),
        shiny.semantic::button(
          ns("granul_1"),
          rename_string_vector(names(which(max_trisk_granularity == 1)), words_class = "analysis_columns"),
          class = "ui button fluid"
        ),
        shiny.semantic::button(
          ns("granul_2"),
          rename_string_vector(names(which(max_trisk_granularity == 2)), words_class = "analysis_columns"),
          class = "ui button fluid"
        )
        # ,
        # shiny.semantic::button(
        #   ns("granul_3"),
        #   rename_string_vector(names(which(max_trisk_granularity == 3)), words_class = "analysis_columns"),
        #   class = "ui button fluid"
        # )
        
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
      update_button_style(session$ns("granul_1"), TRUE)
      update_button_style(session$ns("granul_2"), FALSE)
      # update_button_style(session$ns("granul_3"), FALSE)
      trisk_granularity_r(
        get_trisk_granularity(max_trisk_granularity, 1)
      )
    })

    observeEvent(input$granul_2, {
      update_button_style(session$ns("granul_1"), FALSE)
      update_button_style(session$ns("granul_2"), TRUE)
      # update_button_style(session$ns("granul_3"), FALSE)
      trisk_granularity_r(
        get_trisk_granularity(max_trisk_granularity, 2)
      )
    })

    # observeEvent(input$granul_3, {
    #   update_button_style(session$ns("granul_1"), FALSE)
    #   update_button_style(session$ns("granul_2"), FALSE)
    #   update_button_style(session$ns("granul_3"), TRUE)
    #   trisk_granularity_r(
    #     get_trisk_granularity(max_trisk_granularity, 3)
    #   )
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
# Updated function to toggle button styles through direct CSS manipulation
update_button_style <- function(input_id, clicked = FALSE) {
  js_code <- if(clicked) {
    # JavaScript to apply when button is clicked
    sprintf("
      $('#%s').css({
        'background-color': '#000000',
        'color': '#FFFFFF'
      });
    ", input_id)
  } else {
    # JavaScript to revert to original "ui button" style
    sprintf("
      $('#%s').css({
        'background-color': '',
        'color': ''
      }).removeClass('ui button').addClass('ui button');
    ", input_id)
  }
  
  shinyjs::runjs(js_code)
}
