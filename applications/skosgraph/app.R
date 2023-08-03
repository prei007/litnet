#
# skos-based interface
library(shiny)
library(shinyjs)
library(visNetwork)
library(allegRo)




ui <- fluidPage(
  useShinyjs(), 
  titlePanel("LitGraph"), 
             sidebarLayout(
               sidebarPanel(
                 textInput("userName", "User Name:"), 
                 passwordInput("pwd", "Password:"),
                 actionButton("loginButton", "Submit")
               ),
               mainPanel(
                 tabsetPanel(type = "tabs", 
                             tabPanel("Graph", 
                                      # show user name
                                      actionButton("showMapButton",  "show map view "),
                                      p(" Network here "),
                                      # visNetworkOutput("Map", width = "1000px", height = "600px")
                                      #    DTOutput('tbl')
                             ),
                             tabPanel("Table", 
                                      p(" Table here"))
                             
                 )
               )
             )
  )


# Define server logic required to draw a histogram
server <- function(input, output) {

}

# Run the application 
shinyApp(ui = ui, server = server)
