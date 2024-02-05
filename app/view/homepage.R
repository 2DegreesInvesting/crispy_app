# Load required packages
box::use(
  shiny[moduleServer, NS, div, h1],
  shiny.semantic[semanticPage],
  semantic.dashboard[dashboardPage, dashboardBody,dashboardSidebar]
)

box::use(
  app / view / modules / menus[dashboard_header_crispy]
)

####### UI

ui <- function(id) {
  ns <- NS(id)


  dashboardPage(
    title = "Homepage",
    # dashboardHeader
    dashboard_header_crispy(id=ns("homepage_menu"), page_select = "Homepage"),
    # dashboardSidebar
    dashboardSidebar(
      div(
        class = "ui container",
        h1("Sidebar")
      )
    ),
    # dashboardBody
    dashboardBody(
      semanticPage(
        div(
          class = "ui container",
          h1("Welcome to the homepage")
        )
      )
    )
  )
}

####### Server

server <- function(id) {
  moduleServer(id, function(input, output, session) {

  })
}
