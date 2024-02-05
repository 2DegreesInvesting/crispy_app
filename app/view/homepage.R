# Load required packages
box::use(
  shiny[moduleServer, NS, div, h1, tags],
  shiny.semantic[semanticPage],
  semantic.dashboard[dashboardPage, dashboardBody, dashboardSidebar, dashboardHeader]
)

box::use(
  app / view / modules / menus[dashboard_header_crispy]
)

####### UI

ui <- function(id) {
  ns <- NS(id)



  # dashboardBody
  dashboardBody(
    semanticPage()
  )
}

####### Server

server <- function(id) {
  moduleServer(id, function(input, output, session) {

  })
}
