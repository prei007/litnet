
server <- function(input, output, session) {
  # Manage hidden panels in the tabset panel
  observeEvent(input$template, {
    updateTabsetPanel(inputId = "templates", selected = input$template)
  }) 
}

server
