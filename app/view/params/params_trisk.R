box::use(
  shiny[
    moduleServer, NS, observe, div, tags, reactiveVal, reactiveValues, eventReactive, p, tagList, observeEvent, img,
    HTML, conditionalPanel, reactive
  ],
  shiny.semantic[slider_input, dropdown_input, segment, update_dropdown_input, update_slider],
  shinyjs[useShinyjs],
  semantic.dashboard[dashboardSidebar]
)

box::use(
  app / logic / renamings[rename_string_vector]
)


ui <- function(id, available_vars) {
  ns <- NS(id)
  segment(
    div(
      class = "content",
      div(class = "header", "TRISK parameters", style = "font-size: 150%;"),
      tags$hr(), # esthetic separation
      p("Shock Year"),
      slider_input(
        ns("shock_year"),
        custom_ticks = available_vars$available_shock_year,
        value = NULL
      ),
      p("Risk-Free Rate"),
      slider_input(
        ns("risk_free_rate"),
        custom_ticks = available_vars$available_risk_free_rate,
        value = NULL
      ),
      p("Discount Rate"),
      slider_input(
        ns("discount_rate"),
        custom_ticks = available_vars$available_discount_rate,
        value = NULL
      ),
      p("Growth Rate"),
      slider_input(
        ns("growth_rate"),
        custom_ticks = available_vars$available_growth_rate,
        value = NULL
      ),
      p("Dividend Rate"),
      slider_input(
        ns("div_netprofit_prop_coef"),
        custom_ticks = available_vars$available_dividend_rate,
        value = NULL
      ),
      p("Carbon Price Model"),
      dropdown_input(ns("carbon_price_model"),
        choices = available_vars$available_carbon_price_model,
        value = "no_carbon_tax"
      ),
      conditionalPanel(
        condition = "input.carbon_price_model != 'no_carbon_tax'",
        p("Market Passthrough"),
        slider_input(
          ns("market_passthrough"),
          custom_ticks = available_vars$available_market_passthrough,
          value = NULL
        ),
        ns = ns
      )
    )
  )
}


server <- function(id, available_vars) {
  moduleServer(id, function(input, output, session) {
    # synchronise discount and growth rates sliders, to always keep growth rate < discount rate
    # When growth rate changes, check if growth rate is higher and adjust if necessary
    observeEvent(c(input$growth_rate, input$discount_rate), {
      if (input$growth_rate >= input$discount_rate) {
        # Find the closest smaller value in 'available_growth_rate'
        smaller_values <- available_vars$available_growth_rate[available_vars$available_growth_rate < input$discount_rate]
        closest_smaller_value <- sort(smaller_values)[length(smaller_values)]

        # Update growth_rate slider
        update_slider(session, "growth_rate", value = as.character(closest_smaller_value))
      }
    })
trisk_config_r <- reactive({reactiveValues(
        shock_year = as.numeric(input$shock_year),
        discount_rate = as.numeric(input$discount_and_growth$discount_rate),
        risk_free_rate = as.numeric(input$risk_free_rate),
        growth_rate = as.numeric(input$discount_and_growth$growth_rate),
        div_netprofit_prop_coef = as.numeric(input$div_netprofit_prop_coef),
        carbon_price_model = input$carbon_price_model,
        market_passthrough = as.numeric(input$market_passthrough)
)})
    return(
      trisk_config_r
    )
  })
}
