# Load required packages
box::use(
  shiny[moduleServer, NS, div, h1, tags, HTML, observe, reactive],
  shiny.semantic[semanticPage],
  semantic.dashboard[dashboardPage, dashboardBody, dashboardSidebar, dashboardHeader],
  shinyjs[useShinyjs]
)

####### UI


ui <- function(id) {
  create_dropdown_input(id)
}

server <- function(id, variable_choices_r) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # Update the dropdown choices
    observe({
      newChoices <- variable_choices_r()
      
      # Send new choices to the dropdown using the namespace
      update_dropdown_input(session, newChoices)
    })

    # Accessing the selected value
    dropdown_choice_r <- reactive({
      input$dropdown_choice
    })
    return(dropdown_choice_r)
  })
}


create_dropdown_input <- function(id){
  ns <- NS(id)

  tags$div(
    useShinyjs(),
    tags$head(
      tags$script(src = "https://cdn.jsdelivr.net/npm/fuse.js/dist/fuse.basic.min.js"),
    ),
    tags$div(
        class = "ui fluid search selection dropdown", id = ns("search_dropdown"),
        tags$div(class = "default text", paste0("Select a ", id, "")),
        tags$i(class = "dropdown icon"),
        tags$div(class = "menu")
      ),
      tags$input(type = "hidden", name = ns("dropdown_choice")),

    # TODO js scripts shouldn't be in the html source code, to move to js folder
    # TODO FIX THE ifelse DOESNT WORK
    tags$script(HTML(paste0("
   $(document).ready(function() {
      var fuse; // Initialize Fuse.js later
      var allChoices = []; // Placeholder for dropdown choices

      // Function to initialize or update the dropdown and Fuse.js
      function updateDropdown(choices) {
        allChoices = choices.map(choice => ({ name: choice, value: choice }));
        fuse = new Fuse(allChoices, { keys: ['name'], includeScore: false });

        $('#",ns("search_dropdown"),"').dropdown({
          values: allChoices,
          forceSelection: false,
          minCharacters: 0,
          fullTextSearch: 'exact',
          onChange: function(value, text, $selectedItem) {
            
            Shiny.setInputValue('",ns("dropdown_choice"),"', value); // Send the selected value to server

          },
          apiSettings: {
            responseAsync: function(settings, callback) {
              const searchTerm = settings.urlData.query;
              const results = fuse.search(searchTerm).slice(0, 5);
              callback({
                success: true,
                results: results.map(match => match.item)
              });
            }
          }
        });
      }
    $(document).on('shiny:connected', function() {
          // Listen for updates from Shiny to update dropdown choices
          Shiny.addCustomMessageHandler('",ns("updateChoices"),"', function(message) {
            updateDropdown(message.choices);
          });

          // Initialize the dropdown with empty choices
          updateDropdown([]);
        });
    });
    ")))
  )
}

    update_dropdown_input <- function(session, choices) {
      # Send choices to UI for dropdown update
    session$sendCustomMessage(session$ns("updateChoices"), list(choices = choices))
    }