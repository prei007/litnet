library(shiny)

ui <- fluidPage(
  fluidRow(
    column(width = 3,
           selectInput("min", "Min Price", 1:4)
    ),
    column(width = 3, 
           selectInput("max", "Max Price", 1:4)
    )
  ),
  fluidRow(
    column(width = 3,
           selectInput("min2", "Property type", letters[1:4])
    ),
    column(width = 3, 
           selectInput("max2", "Bedrooms", 1:4)
    )
  )
)

shinyApp(ui, server = function(input, output){ })
