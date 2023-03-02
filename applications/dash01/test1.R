library(shiny)
library(shinydashboard)
library(shinydashboardPlus)


ui <- dashboardPage(skin = "yellow", 
  dashboardHeader(title = "Basic Dashboard"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
      menuItem("Widgets", tabName = "widgets", icon = icon("th"))
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "dashboard", 
    fluidRow(
      box(plotOutput("plot1", height = 250)),
      box(title = "controls", 
          sliderInput("slider", "Number of observations", 1, 100, 50)
      ))),
    
    tabItem(tabName = "widgets",
            fluidRow(
              flipBox(
                id = 1,
                width = 12,
                front = tagList(
                  "FRONT: Lorem ipsum dolor sit amet, consectetur adipiscing elit, 
    sed do eiusmod tempor incididunt ut labore et dolore magna 
    aliqua. Ut enim ad minim veniam, quis nostrud exercitation 
    ullamco laboris nisi ut aliquip ex ea commodo consequat. 
    Duis aute irure dolor in reprehenderit in voluptate velit 
    esse cillum dolore eu fugiat nulla pariatur. Excepteur sint 
    occaecat cupidatat non proident, sunt in culpa qui officia 
    deserunt mollit anim id est laborum"
                ),
                back = tagList("BACK: Lorem ipsum dolor sit amet, consectetur adipiscing elit, 
    sed do eiusmod tempor incididunt ut labore et dolore magna 
    aliqua. Ut enim ad minim veniam, quis nostrud exercitation 
    ullamco laboris nisi ut aliquip ex ea commodo consequat. 
    Duis aute irure dolor in reprehenderit in voluptate velit 
    esse cillum dolore eu fugiat nulla pariatur. Excepteur sint 
    occaecat cupidatat non proident, sunt in culpa qui officia 
    deserunt mollit anim id est laborum")
              ))))))

server <- function(input, output) {
  set.seed(122)
  histdata <- rnorm(500)
  output$plot1 <- renderPlot({data <- histdata[seq_len(input$slider)]
    hist(data)
  })
}

shinyApp(ui, server)