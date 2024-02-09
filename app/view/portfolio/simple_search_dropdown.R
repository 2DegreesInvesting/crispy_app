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
      browser()
      session$sendCustomMessage(
        session$ns("updateDropdown"),
        list(choices = newChoices)
      )
    })

    # Accessing the selected value
    dropdown_choice <- reactive({
      pick_id <- session$ns("dropdown_choice")
      input[[pick_id]]
    })
    return(dropdown_choice)
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
      tags$input(type = "hidden", name = ns("dropdown_choice")),
      tags$div(class = "default text", paste0("Select a ", id, "")),
      tags$i(class = "dropdown icon"),
      tags$div(class = "menu")
    ),

    # TODO js scripts shouldn't be in the html source code, to move to js folder
    # TODO FIX THE ifelse DOESNT WORK
    tags$script(HTML(paste0("
      $(document).ready(function() {

        function initializeDropdown(choices) {
          $('#", ns("search_dropdown"), "').dropdown({
            values: choices,
            forceSelection: false,
            minCharacters: 1,
            onChange: function(value, text, $choice) {
              // This line sends the selected value to Shiny server
              Shiny.setInputValue('", ns("dropdown_choice"), "', value);
            },
            onLabelCreate: function(value, text) {
              return $(this);
            },
            // The API settings is where is defined the fuzzy search
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
        
        var allChoices = [];
        initializeDropdown(allChoices); // Initial dropdown initialization

       Shiny.addCustomMessageHandler('", ns("updateDropdown"), "', function(newChoices) {
          // Format the choices as required by the dropdown
          var formattedChoices = newChoices.choices.map(function(choice) {
            return { name: choice, value: choice };
          });
          // Update dropdown values
          // Update dropdown values
          $('#", ns("choices_dropdown"), "').dropdown('clear');
          $('#", ns("choices_dropdown"), "').dropdown('setup menu', {values: formattedChoices});
        });
      });
    ")))
  )
}

