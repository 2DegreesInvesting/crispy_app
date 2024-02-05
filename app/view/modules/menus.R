box::use(
  shiny[div],
  semantic.dashboard[dashboardHeader, icon, dropdownMenu, menuItem]
)


dashboard_header_crispy <- function(id, page_select = "") {
  dashboardHeader(
    title = "Crispy App",
    # Create a right-side menu
    dropdownMenu(
      id=id,
      type = "messages",
      show_counter = FALSE,
      icon = icon("th"),
      .list = list(
        menuItem(
          "Homepage",
          icon = icon("home"),
          href = shiny.router::route_link("/"),
          selected = ifelse(page_select == "Homepage", TRUE, FALSE)
        ),
        div(class = "divider"),
        menuItem(
          "Crispy Equities",
          icon = icon("factory"),
          href = shiny.router::route_link("crispy_equities"),
          selected = ifelse(page_select == "Crispy Equities", TRUE, FALSE)
        ),
        menuItem(
          "Crispy Loans",
          icon = icon("university"),
          href = shiny.router::route_link("crispy_loan"),
          selected = ifelse(page_select == "Crispy Loans", TRUE, FALSE)
        )
      )
    )
  )
}
