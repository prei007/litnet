# Tab boxes

library(shiny)
library(shinydashboard)
library(shinydashboardPlus)
library(allegRo)
library(urltools)

# Functions 

stripOffNS <- function(df) {
  df <- lapply(df, function(x) {gsub(">", "", x)})
  df <- lapply(df, function(x) {gsub("<", "", x)})
  df
}


# Set up connection: 

url = "http://learn-web.com"
user = "anonymous"
password = ""
service = service(url, user, password, testConnection = TRUE)

# connect to compumod repo. 

cat = catalog(service, "coolfutures")
rep = repository(cat, "compumod")
addNameSpace(repo = rep, prefix= "edtech", nsURI = "http://coolfutures.net/rdf/2021/edtech#")
addNameSpace(repo = rep, prefix= "arg", nsURI = "http://www.coolfutures.net/rdf/Design_Conjectures/toulminarg#")

# get the works 

query = "SELECT ?work
	{ ?work a dc:BibliographicResource } ORDER BY ?work "
dfworks <- evalQuery(rep,
                     query = query, returnType = "dataframe",
                     cleanUp = TRUE, limit = 1000)
dfworks <- as.data.frame(dfworks[1])
colnames(dfworks) <- "works"

# Stripp off URIs
dfworks <- stripOffNS(dfworks)
dfworks$works <- fragment(dfworks$works)
# Note that the works have no leading colon but that these are needed for the search later

### Build the sidebar 

sidebar <- dashboardSidebar(
## Search Menu
# accessible via input$workSearch and input$workSearchButton
  sidebarSearchForm(textId = "workSearch", 
                    buttonId = "workSearchButton", 
                    label =  "searchText", 
                    icon = icon("search")), 
  sidebarMenu(id = "tabs", 
              menuItem("studyInfo", tabName = "studyInfo"),
              menuItem("statistics", tabName = "Statistics")
              )
)

### build the body of the UI

body <- dashboardBody(
  tabItems(
    tabItem(
      tabName = "studyInfo",
      fluidRow(
        selectInput(
          inputId = "firstWork", 
          label = "Select first study",
          choices = c("Dickes2019", "Dickes2016", "option3")),
        selectInput(
          inputId = "secondWork", 
          label = "Select second study",
          choices = c("option1", "Dickes2016", "option3")),
        tabBox(
          title = textOutput("firstStudy"),
          # The id lets us use input$tabset1 on the server to find the current tab
          id = "tabset1",
          height = "500px",
          tabPanel("Bibliography", htmlOutput('bibText')),
          tabPanel(
            "Pedagogy",
            h3("Suspendisse"),
            "et ornare sapien, non fringilla ligula. ."
          ),
          tabPanel("Design", "design goes here"),
          tabPanel(
            "Research",
            "Suspendisse et ornare sapien, non fringilla ligula. ."
          ),
          tabPanel(
            "Evaluation",
            "Suspendisse et ornare sapien, ."
          ),
        ),
        tabBox(
          title = "Second Study",
          height = "500px",
          selected = "Tab1",
          tabPanel("Tab1", "Tab content 1"),
          tabPanel("Tab2", "Tab content 2"),
          tabPanel("Tab3", "Note that when side=right, the tab order is reversed.")
        )
      )),
    (tabItem(tabName = "Statistics", 
             h2("Statistics Tab")))
  ))

#### compose the app 

shinyApp(
  ui = dashboardPage(
    dashboardHeader(title = "Study information"),
    sidebar,
    body
  ),
  server = function(input, output) {
    output$firstStudy <- reactive({ input$firstWork })
    # The first tab
    output$bibText <- renderText({
      query = paste0('select ?title ?date ?doi 
                  {?id a dc:BibliographicResource ; 
                   dc:title ?title; 
                               dc:date ?date; 
                  			dc:identifier ?doi.
                  FILTER (?id IN (:', input$firstWork, ')).} LIMIT 1')
      dfout <- evalQuery(rep,
                         query = query, returnType = "dataframe",
                         cleanUp = TRUE, limit = 1000)
      dfout <- dfout[["return"]]
      paste0("<p>", "<strong>", "Title: ", "</strong>", dfout[[1:1]], "</p>" , 
               "<p>", "<strong>", "Year: ", "</strong>", dfout[[2:2]], "</p>", 
               "<p>", "<strong>", "DOI: ", "</strong>" , dfout[[3:3]], "</p>"
               )
        })
  })