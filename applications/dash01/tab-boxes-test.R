# Tab boxes


body <- dashboardBody(
  fluidRow(
    tabBox(
      title = "First tabBox",
      # The id lets us use input$tabset1 on the server to find the current tab
      id = "tabset1", height = "500px",
      tabPanel("Tab1", "Donec in efficitur massa, vel lobortis est. Etiam vitae gravida nibh. Aliquam erat volutpat. Fusce feugiat velit suscipit rhoncus congue. Proin faucibus, leo a condimentum venenatis, est nulla sodales lorem, nec ultrices est nulla vitae metus. Duis porttitor convallis dui, egestas mattis enim vestibulum et. Ut aliquam lobortis porta. Aliquam eu ipsum eget turpis pharetra vehicula ac vitae ex. Cras vitae neque laoreet, interdum augue sed, vulputate tellus. Pellentesque vehicula rutrum odio sit amet aliquam."),
      tabPanel("Tab2", "Suspendisse et ornare sapien, non fringilla ligula. Pellentesque at nisl vel felis fringilla faucibus. Donec sollicitudin vehicula fringilla. Duis aliquam congue risus, ut molestie nunc dapibus eget. Proin dignissim euismod sem, id ornare velit. Donec at mi id metus sagittis accumsan a in libero. Sed quis dui tortor.")
    ),
    tabBox(
      side = "right", height = "250px",
      selected = "Tab3",
      tabPanel("Tab1", "Tab content 1"),
      tabPanel("Tab2", "Tab content 2"),
      tabPanel("Tab3", "Note that when side=right, the tab order is reversed.")
    )
  ),
  fluidRow(
    tabBox(
      # Title can include an icon
      title = tagList(shiny::icon("gear"), "tabBox status"),
      tabPanel("Tab1",
               "Currently selected tab from first box:",
               verbatimTextOutput("tabset1Selected")
      ),
      tabPanel("Tab2", "Tab content 2")
    )
  )
)

shinyApp(
  ui = dashboardPage(
    dashboardHeader(title = "tabBoxes"),
    dashboardSidebar(),
    body
  ),
  server = function(input, output) {
    # The currently selected tab from the first box
    output$tabset1Selected <- renderText({
      input$tabset1
    })
  }
)