library(shiny)
library(dqshiny)
# create 100k random words
# opts <- sapply(1:1000, function(i) paste0(sample(letters, 7), collapse=""))

opts <- c("lo:object1", "lo:object2", "env:option1", "dummy")

shinyApp(
  ui = fluidPage(
    fluidRow(
      column(3,
             autocomplete_input("auto1", "Unnamed:", opts, max_options = 10, create = TRUE),
             autocomplete_input("auto2", "Named:", max_options = 10,
                                structure(opts, names = opts[order(opts)]))
             # feel free to test this with select... and may get yourself a coffee
             # , selectInput("sel", "Select:", opts)
      ), column(3,
                tags$label("Value:"), verbatimTextOutput("val1", placeholder = TRUE),
                tags$label("Value:"), verbatimTextOutput("val2", placeholder = TRUE)
      )
    )
  ),
  server = function(input, output) {
    output$val1 <- renderText(as.character(input$auto1))
    output$val2 <- renderText(as.character(input$auto2))
  }
)