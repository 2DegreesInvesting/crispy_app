box::use(
  shiny[
    moduleServer, NS, observe, div, tags, reactiveVal, reactiveValues, eventReactive, p, tagList, observeEvent, img,
    HTML, reactive
  ],
  shiny.semantic[update_slider]
  )

box::use(
  app/logic/renamings[rename_string_vector]
)


ui <- function(id, available_vars) {
  ns <- NS(id)
  shiny::tagList(
    tags$head(
      tags$style(HTML("
        /* Target the slider track more specifically */
        .ss-slider .track {
          background-color: #FFFFFF !important; /* Whiter track, ensure override with !important */
        }

        /* Target the slider thumb (handle) more specifically */
        .ss-slider .thumb {
          background-color: #000000 !important; /* Black handle, ensure override with !important */
          height: 10px !important; /* Smaller height */
          width: 10px !important; /* Smaller width */
          top: 50% !important; /* Center vertically */
          transform: translateY(-50%) !important; /* Adjust for exact centering */
        }

        /* Optional: Style the part of the track before the handle */
        .ss-slider .track-fill {
          background-color: #CCCCCC !important; /* Lighter fill color */
        }
      "))
    ),
    p("Shock Year"),
    shiny.semantic::slider_input(
      ns("shock_year"),
      custom_ticks = available_vars$available_shock_year,
      value = available_vars$available_shock_year[3]
    ),
    p("Risk-Free Rate"),
    shiny.semantic::slider_input(
      ns("risk_free_rate"),
      custom_ticks = available_vars$available_risk_free_rate,
      value = available_vars$available_risk_free_rate[3]
    ),
    p("Discount Rate"),
    shiny.semantic::slider_input(
      ns("discount_rate"),
      custom_ticks = available_vars$available_discount_rate,
      value = available_vars$available_discount_rate[4]
    ),
    p("Growth Rate"),
    shiny.semantic::slider_input(
      ns("growth_rate"),
      custom_ticks = available_vars$available_growth_rate,
      value = available_vars$available_growth_rate[2]
    ),
    p("Dividend Rate"),
    shiny.semantic::slider_input(
      ns("div_netprofit_prop_coef"),
      custom_ticks = available_vars$available_dividend_rate,
      value = available_vars$available_dividend_rate[3]
    ),
    p("Carbon Price Model"),
    shiny.semantic::dropdown_input(ns("carbon_price_model"),
      choices = available_vars$available_carbon_price_model,
      value = "no_carbon_tax"
    ),
    shiny::conditionalPanel(
      condition = "input.carbon_price_model != 'no_carbon_tax'",
      p("Market Passthrough"),
      shiny.semantic::slider_input(
        ns("market_passthrough"),
        custom_ticks = available_vars$available_market_passthrough,
        value = NULL
      ),
      ns = ns
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

    trisk_config_r <- reactive({
      reactiveValues(
        shock_year = as.numeric(input$shock_year),
        discount_rate = as.numeric(input$discount_rate),
        risk_free_rate = as.numeric(input$risk_free_rate),
        growth_rate = as.numeric(input$growth_rate),
        div_netprofit_prop_coef = as.numeric(input$div_netprofit_prop_coef),
        carbon_price_model = input$carbon_price_model,
        market_passthrough = as.numeric(input$market_passthrough)
      )
    })
    return(
      trisk_config_r
    )
  })
}
