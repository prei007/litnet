# https://github.com/dreamRs/shinytreeview/



library(shiny)
library(shinytreeview)

data("cities")

ui <- fluidPage(
  tags$h3("treeviewInput cities example"),
  treeviewInput(
    inputId = "tree",
    label = "Choose a city:",
    choices = make_tree(
      cities, c("continent", "country", "city")
    ),
    multiple = FALSE,
    prevent_unselect = TRUE,
    width = "100%"
  ),
  verbatimTextOutput(outputId = "result")
)

server <- function(input, output, session) {
  output$result <- renderPrint({
    input$tree
  })
}

if (interactive())
  shinyApp(ui, server)