# Load required packages
box::use(
  shiny[moduleServer, NS, div, h1, tags, HTML, observe, reactive],
  shiny.semantic[semanticPage],
  semantic.dashboard[dashboardPage, dashboardBody, dashboardSidebar, dashboardHeader],
  shinyjs[useShinyjs]
)

####### UI

# offer_option : don't wait for first keystroke to show dropdown
ui <- function(id, offer_options=FALSE) {
  ns <- NS(id)

tags$div(
useShinyjs(),
  tags$head(
    tags$script(src = "https://cdn.jsdelivr.net/npm/fuse.js/dist/fuse.basic.min.js"),
  ),
  tags$div(style = "width: 300px; margin: 20px auto;",
      tags$div(class = "ui fluid search selection dropdown", id = ns("search-dropdown"),
          tags$input(type = "hidden", name = ns("picked_choice")),
          tags$div(class = "default text", paste0("Select a ",id,"")),
          tags$i(class = "dropdown icon"),
          tags$div(class = "menu")
      )
  ),

  # TODO js scripts shouldn't be in the html source code, to move to js folder
  # TODO FIX THE ifelse DOESNT WORK
  tags$script(HTML(paste0("
      $(document).ready(function() {
        var allChoices = [];

        function initializeDropdown(choices) {
          $('#", ns("search-dropdown"), "').dropdown({
            values: choices,
            forceSelection: false,
            minCharacters: ",ifelse(offer_options, "0", "1"),",
            onLabelCreate: function(value, text) {
              return $(this);
            },
            apiSettings: {
              responseAsync: function(settings, callback) {
                setTimeout(function() {
                  const searchTerm = settings.urlData.query;
                  const options = {
                    includeScore: false,
                    keys: ['name']
                  };
                  const fuse = new Fuse(choices, options);
                  const result = fuse.search(searchTerm).slice(0, 5);

                  const response = {
                    success: true,
                    results: result.map(match => ({ name: match.item.name, value: match.item.name }))
                  };
                  callback(response);
                }, 300);
              }
            }
          });
        }

        initializeDropdown(allChoices); // Initial dropdown initialization

        Shiny.addCustomMessageHandler('", ns("updateDropdown"), "', function(newChoices) {
          allChoices = [...allChoices, ...newChoices.map(choice => ({ name: choice, value: choice }))];
          initializeDropdown(allChoices); // Reinitialize dropdown with new choices
        });
      });
    "
  ))))

}


server <- function(id, variable_choices_r) {
    moduleServer(id, function(input, output, session) {
    
      ns <- session$ns      

    observe({
      newChoices <- variable_choices_r()
      # Send new choices to the dropdown using the namespace
      session$sendCustomMessage(ns("updateDropdown"), newChoices)
    })

    # Accessing the selected value
    picked_choice <- reactive({ input[[ns("picked_choice")]] })
    return(picked_choice)
  })

}