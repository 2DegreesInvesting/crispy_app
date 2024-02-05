box::use(
  shiny[ div ],
  semantic.dashboard[dashboardHeader, icon, dropdownMenu, menuItem]
)


dashboard_header_crispy <- function(page_select = "") {
        dashboardHeader(
            title = "My Dashboard",
            # Create a right-side menu
            dropdownMenu(
                type = "messages",
                show_counter = FALSE,
                icon = icon("th"),
                .list = list(
                    menuItem("Home", icon = icon("home"), href = shiny.router::route_link("/"), selected = ifelse(page_select == "Home", TRUE, FALSE)),
                    div(class = "divider"),
                    menuItem("Portfolio", icon = icon("table"), href = shiny.router::route_link("portfolio"), selected = ifelse(page_select == "Portfolio", TRUE, FALSE)),
                    menuItem("Crispy Equities", icon = icon("factory"), href = shiny.router::route_link("crispy_equities"), selected = ifelse(page_select == "Crispy Equities", TRUE, FALSE)),
                    menuItem("Crispy Loans", icon = icon("university"), href = shiny.router::route_link("crispy_loan"), selected = ifelse(page_select == "Crispy Loans", TRUE, FALSE))
                )
            )
        )
}

