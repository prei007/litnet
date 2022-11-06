library(shiny)
library(shinydashboard)
library(shinydashboardPlus)

shinyApp(
  ui = dashboardPage(
    header = dashboardHeader(),
    sidebar = dashboardSidebar(),
    body = dashboardBody(
      carousel(
        id = "mycarousel",
        carouselItem(
          caption = "Item 1",
          tags$img(src = "https://adminlte.io/themes/AdminLTE/dist/img/photo1.png")
        ),
        carouselItem(
          caption = "Item 2",
          tags$img(src = "https://adminlte.io/themes/AdminLTE/dist/img/photo2.png")
        )
      )
    ),
    title = "Carousel"
  ),
  server = function(input, output) { }
)

