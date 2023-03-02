# 3-columns layout
# https://rstudio.github.io/shinydashboard/structure.html#layouts

library(shiny)
library(shinydashboard)
library(shinydashboardPlus)

body <- dashboardBody(
  fluidRow(
    box(title = "Box title", "Lorem ipsum dolor sit amet, consectetur adipiscing elit. Sed non alienum est, quo facilius vis verbi intellegatur, rationem huius verbi faciendi Zenonis exponere. Cum id fugiunt, re eadem defendunt, quae Peripatetici, verba. Videmus igitur ut conquiescere ne infantes quidem possint. "),
    box(status = "warning", "Lorem ipsum dolor sit amet, consectetur adipiscing elit. Sed non alienum est, quo facilius vis verbi intellegatur, rationem huius verbi faciendi Zenonis exponere. Cum id fugiunt, re eadem defendunt, quae Peripatetici, verba. Videmus igitur ut conquiescere ne infantes quidem possint. ")
  ),
  fluidRow(
    box(
      title = "Title 1", width = 4, solidHeader = TRUE, status = "primary",
      "Box content"
    ),
    box(
      title = "Title 2", width = 4, solidHeader = TRUE,
      "Box content"
    ),
    box(
      title = "Title 1", width = 4, solidHeader = TRUE, status = "warning",
      "Box content"
    )
  ),
  
  fluidRow(
    box(
      width = 4, background = "black",
      "A box with a solid black background"
    ),
    box(
      title = "Title 5", width = 4, background = "light-blue",
      "A box with a solid light-blue background"
    ),
    box(
      title = "Title 6",width = 4, background = "maroon",
      "A box with a solid maroon background"
    )
  )
)

# We'll save it in a variable `ui` so that we can preview it in the console
ui <- dashboardPage(skin = "purple", 
  dashboardHeader(title = "Row layout"),
  dashboardSidebar(),
  body
)

# Preview the UI in the console
shinyApp(ui = ui, server = function(input, output) { })
