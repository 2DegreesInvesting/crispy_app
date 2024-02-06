# Load required packages
box::use(
  shiny[moduleServer, NS, div, h1, tags],
  shiny.semantic[semanticPage],
  semantic.dashboard[dashboardPage, dashboardBody, dashboardSidebar, dashboardHeader]
)



####### UI

ui <- function(id) {
  ns <- NS(id)

  semanticPage(

  )

}

####### Server

server <- function(id) {
  moduleServer(id, function(input, output, session) {

  })
}
