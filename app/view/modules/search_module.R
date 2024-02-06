# Load required packages
box::use(
  shiny[moduleServer, NS, div, h1, tags],
  shiny.semantic[semanticPage],
  semantic.dashboard[dashboardPage, dashboardBody, dashboardSidebar, dashboardHeader]
)

####### UI

ui <- function(id) {
  ns <- NS(id)

div(class="fluid container", 
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "https://cdn.jsdelivr.net/npm/fomantic-ui@2.8.8/dist/semantic.min.css"),
    tags$script(src = "https://cdn.jsdelivr.net/npm/fuse.js/dist/fuse.basic.min.js"),
    tags$script(src = "https://code.jquery.com/jquery-3.6.0.min.js"),
    tags$script(src = "https://cdn.jsdelivr.net/npm/fomantic-ui@2.8.8/dist/semantic.min.js")
  ),
  tags$div(style = "width: 300px; margin: 20px auto;",
      tags$div(class = "ui fluid search selection dropdown", id = ns("search-dropdown"),
          tags$input(type = "hidden", name = "company"),
          tags$i(class = "dropdown icon"),
          tags$div(class = "default text", "Select a company"),
          tags$div(class = "menu")
      )
  ),
  tags$script(HTML(paste0("
     Shiny.addCustomMessageHandler('updateDropdown', function(message) {
      const newChoices = message.map(choice => ({ name: choice, value: choice }));
      const dropdownId = '#",ns("search-dropdown"),"'; // Construct the ID using the namespace
      const dropdown = $(dropdownId).dropdown({
        values: newChoices,
        forceSelection: false,
        minCharacters: 1,
        onLabelCreate: function(value, text) {
          return $(this);
        }
      });
      
      // Initialize or update the dropdown with new choices
      dropdown.dropdown('change values', newChoices);
    });
  "
  ))))

}


server <- function(id, variable_choices_r) {
  moduleServer(id, function(input, output, session) {
    
    observe({
      # Vector of new choices
      newChoices <- variable_choices_r()
      # Send new choices to the dropdown using the namespace
      session$sendCustomMessage(type = 'updateDropdown', message = newChoices)
    })
  })
}
