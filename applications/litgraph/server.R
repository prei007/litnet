server <- function(input, output, session) {
  scheme_name <- reactive(input$scheme)
  # compute the dynamic part of the UI
  output$fillScheme <- renderUI({
    tagList(
      # add input fields
      # To get unique field names, we create <scheme>_<property> IDs:
      lapply(arg_schemes[[scheme_name()]], function(x)
        textAreaInput(paste0(
          names(arg_schemes[scheme_name()]), "_", x
        ), x)),
      # and a save button
      actionButton("b1", "save"))
    
  })
  
  # Read the field values on button click
  observeEvent(input$b1, {
    # re onstruct the field names
    fields <- lapply(arg_schemes[scheme_name()], function(x)
      paste0(scheme_name(), "_", x))
    # read their values. The static input$field syntax is
    # replaced with the dynamic input[] syntax.
    # Replace this with addStatement() when AG server part is ready.
    output$placeholder <- renderText(as.character(lapply(fields[[1]], function(y)
      paste("field: ",
            y,
            " value: ",
            input[[y]]))))
  })
}

server
