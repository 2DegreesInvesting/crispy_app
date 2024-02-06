# Load required packages
box::use(
  shiny[moduleServer, NS, div, h1, tags],
  shiny.semantic[semanticPage],
  semantic.dashboard[dashboardPage, dashboardBody, dashboardSidebar, dashboardHeader]
)



####### UI

ui <- function(id, max_trisk_granularity, available_vars) {
  ns <- NS(id)

  shiny::tagList(

  )


}

####### Server

server <- function(id) {
  moduleServer(id, function(input, output, session) {

  })
}
