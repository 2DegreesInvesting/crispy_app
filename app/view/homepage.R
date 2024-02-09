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
    tags$head(
      # Include any additional CSS or JS if needed
    ),
    tags$div(
      class = "ui raised very padded text container segment",
      tags$div(
        class = "ui two cards",
        tags$div(
          class = "card",
          tags$div(
            class = "content",
            tags$div(class = "header", "CRISPY Equities"),
            tags$div(class = "description", "The Equities tab allows users to access sector and business unit granularities, providing insights into portfolio alignment with the Paris Agreement. Users can input exposure values to assess financial impact.")
          )
        ),
        tags$div(
          class = "card",
          tags$div(
            class = "content",
            tags$div(class = "header", "CRISPY Loans"),
            tags$div(class = "description", "The Loans tab is equipped to assess loan portfolio alignment with the Paris Agreement. Users can add rows by inputting values into search dropdowns and fill in data for exposure, loss given default, and term.")
          )
        )
      ),
      tags$div(class = "ui dividing header", "Understanding TRISK Parameters"),
      tags$p("TRISK parameters play a crucial role in assessing the sustainability and risk factors of financial portfolios. Below is a detailed table of each parameter."),
      tags$table(
        class = "ui celled table",
        tags$thead(
          tags$tr(
            tags$th("Name"),
            tags$th("Description"),
            tags$th("Impact on Outputs")
          )
        ),
        tags$tbody(
          tags$tr(
            tags$td("Parameter 1"),
            tags$td("This parameter helps in ..."),
            tags$td("Affects the output by ...")
          ),
          tags$tr(
            tags$td("Parameter B"),
            tags$td("This parameter measures ..."),
            tags$td("Influences the result by ...")
          )
          # More rows can be added as needed
        )
      ),
      tags$div(class = "ui divider"),
      tags$div(class = "ui dividing header", "How to Use CRISPY"),
      tags$p("To start using CRISPY, select either the Equities or Loans tab. Input the required data into the respective fields and parameters to view the tailored analysis and financial impact. Detailed instructions and tips can be found within each tab."),
      tags$h3(class = "ui header", "Navigating the Dimensions"),
      tags$p("The granularity parameter allows users to dissect and analyze data across different dimensions. Adjusting this parameter will dynamically update the results, enabling a multi-faceted view of the financial impact within each tab."),
      tags$h3(class = "ui header", "Plots Explanation"),
      tags$p("Our interactive plots provide a visual representation of the data, highlighting key metrics and trends."),
      tags$div(
        class = "ui two column very relaxed grid",
        tags$div(
          class = "column",
          tags$img(src = "path/to/plot/image.png", class = "ui large bordered rounded image") # Replace with the path to your image
        ),
        tags$div(
          class = "column",
          tags$p("The plots offer insights into the performance and risk factors associated with the financial instruments in your portfolio. Understanding these visualizations is crucial for making informed decisions.")
        )
      ),
      tags$p("Explore the detailed legends and hover-over tooltips within each plot to gain more information about the data points."),
      tags$div(class = "ui divider"),
      tags$p("Dive into our GitHub repository to explore the source code, contribute, or learn more about the technology behind our tools."),
      tags$a(href = "LINK TO GITHUB", "Visit our GitHub"),
      # Placeholder for GitHub-related imagery

      tags$div(class = "ui divider"),
      tags$div(
        tags$p(
          "© ",
          tags$a(href = "LINK TO YOUR HOMEPAGE", "COMPANY NAME"),
          ". All rights reserved."
        )
      )
    )
  )
}

####### Server

server <- function(id) {
  moduleServer(id, function(input, output, session) {})
}
