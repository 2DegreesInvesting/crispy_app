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
        shiny::tags$head(
      shiny::tags$style(HTML("
        /* Existing styles here */
        .ui.button:active, .ui.button.clicked { 
          background-color: #000000; /* Clicked color */
          color: #ffffff; /* Clicked text color */
        }
      ")),
      shiny::tags$script(HTML(paste0("
        $(document).ready(function() {
          // Apply clicked styles on mousedown and revert on mouseup or mouseleave
          $('.ui.button').on('mousedown', function() {
            $(this).addClass('clicked');
          }).on('mouseup mouseleave', function() {
            $(this).removeClass('clicked');
          });
          
          // Apply the clicked style by default to a specific button
          $('#" , ns("granul_1") , "').addClass('clicked');
        });
      ")))
    ),
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
          class = "ui button fluid"
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
      update_button_style(session$ns("granul_1"), "#000000", "#FFFFFF")
      update_button_style(session$ns("granul_2"), "#d4d4d5", "#333")
      # update_button_style(session$ns("granul_3"), "#d4d4d5", "#FFFFFF")
      trisk_granularity_r(
        get_trisk_granularity(max_trisk_granularity, 1)
      )
    })

    observeEvent(input$granul_2, {
      update_button_style(session$ns("granul_1"), "#d4d4d5", "#333")
      update_button_style(session$ns("granul_2"), "#000000", "#FFFFFF")
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

# Function to directly update the CSS of buttons
update_button_style <- function(input_id, background_color = "#000000", text_color = "#FFFFFF") {
  js_code <- sprintf("$('#%s').css({'background-color': '%s', 'color': '%s'});", input_id, background_color, text_color)
  shinyjs::runjs(js_code)
}