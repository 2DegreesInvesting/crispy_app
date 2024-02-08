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
  app / logic / renamings[rename_string_vector]
)


ui <- function(id, max_trisk_granularity) {
  ns <- NS(id)
  # First segment in the left half // Granularity
  shiny.semantic::segment(
    div(class = "header", "Dashboard", style = "font-size: 150%;"),
    tags$hr(),
    div(
      class = "description",
      div(
        class = "content",
        p("Granularity"),
        slider_input(
          ns("granularity_switch"),
          custom_ticks = rename_string_vector(names(max_trisk_granularity), words_class = "analysis_columns"),
          value = rename_string_vector(names(which(max_trisk_granularity == 1)), words_class = "analysis_columns")
        )
      )
    )
  )
}
# get the column names defining the displayed data granularity
server <- function(id, max_trisk_granularity) {
  moduleServer(id, function(input, output, session) {
    trisk_granularity_r <- eventReactive(input$granularity_switch, ignoreNULL = TRUE, {
      granularity_picked <- input$granularity_switch |>
        rename_string_vector(words_class = "analysis_columns", dev_to_ux = FALSE)

      granularity_level <- max_trisk_granularity[granularity_picked]
      # Filter names based on values <= given_integer
      trisk_granularity <- names(trisk_granularity)[sapply(max_trisk_granularity, function(value) value <= granularity_level)]

      return(trisk_granularity)
    })
    return(trisk_granularity_r)
  })
}
