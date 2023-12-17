library(NestedMenu)
library(shiny)

resmethods <<- list(
  quantitative = list(
    name = "Quantitative",
    items = list(
      survey = list(
        name = "Survey",
        items = list(
          s1 = list(name = "S1"),
          s2 = list(name = "S2"),
          'meth:Survey' = list(name = "Survey")
        )
      ),
      experiment = list(
        name = "Experiment",
        items = list(
          quasi = list(name = "Quasiexperiment"),
          scdr = list(name = "SCDR")
        )
      )
    )
  ),
  qualitative = list(
    name = "Qualitative",
    items = list(
      interview = list(
        name = "Interview",
        items = list(
          focusgroup = list(
            name = "Focusgroup"
          ))),
      observation = list(
        name = "Observation",
        items = list(
          field = list(name = "FieldObseration"),
          video = list(name = "VideoObservation")
        )
      )
    )
  )
)

CodeLabels  <- list(
  LearningOutcomes = list(
    name = "LearningOucomes", 
    items = list(
      "lo:LO1" = list(name = "LO1"),
      "lo:LO2" = list(name = "LO2")
    )
  ),
  Pedagogies = list(
    name = "Pedagogies",
    items = list(
      "envped:Ped1" = list(name = "Ped1"),
      "enved:Ped2" = list(name = "Ped2")
    )
  )
)

ui <- fluidPage(
  br(),
  NestedMenuOutput("menu", height = "auto"),
  br(),
  verbatimTextOutput("clicked")
)

server <- function(input, output, session){
  
  output[["menu"]] <- renderNestedMenu({
    NestedMenu(
      "CodeLabels", items = CodeLabels
    )
  })
  
  output[["clicked"]] <- renderPrint({
    input[["menu"]]
  })
  
}
# Run the application 
shinyApp(ui = ui, server = server)
