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
        tabBox(
          title = "Dickes2019 ",
          # The id lets us use input$tabset1 on the server to find the current tab
          id = "tabset1",
          height = "500px",
          tabPanel("Bibliography", tableOutput('bibTable')),
          tabPanel(
            "Pedagogy",
            h3("Suspendisse"),
            "et ornare sapien, non fringilla ligula. Pellentesque at nisl vel felis fringilla faucibus. Donec sollicitudin vehicula fringilla. Duis aliquam congue risus, ut molestie nunc dapibus eget. Proin dignissim euismod sem, id ornare velit. Donec at mi id metus sagittis accumsan a in libero. Sed quis dui tortor."
          ),
          tabPanel("Design", htmlOutput('bibText')),
          tabPanel(
            "Research",
            "Suspendisse et ornare sapien, non fringilla ligula. Pellentesque at nisl vel felis fringilla faucibus. Donec sollicitudin vehicula fringilla. Duis aliquam congue risus, ut molestie nunc dapibus eget. Proin dignissim euismod sem, id ornare velit. Donec at mi id metus sagittis accumsan a in libero. Sed quis dui tortor."
          ),
          tabPanel(
            "Evaluation",
            "Suspendisse et ornare sapien, non fringilla ligula. Pellentesque at nisl vel felis fringilla faucibus. Donec sollicitudin vehicula fringilla. Duis aliquam congue risus, ut molestie nunc dapibus eget. Proin dignissim euismod sem, id ornare velit. Donec at mi id metus sagittis accumsan a in libero. Sed quis dui tortor."
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
    dashboardHeader(title = "tabBoxes"),
    sidebar,
    body
  ),
  server = function(input, output) {
    # The first tab
    output$bibTable <- renderTable({
      query = 'select ?title ?date ?doi 
                  {?id a dc:BibliographicResource ; 
                   dc:title ?title; 
                               dc:date ?date; 
                  			dc:identifier ?doi.
                  FILTER (?id IN (:Dickes2019)) .} '
      dfout <- evalQuery(rep,
                         query = query, returnType = "dataframe",
                         cleanUp = TRUE, limit = 1000)
      dfout <- dfout[["return"]]
      dfout
    })
    # Table to text 
    output$bibText <- renderText({
      query = 'select ?title ?date ?doi 
                  {?id a dc:BibliographicResource ; 
                   dc:title ?title; 
                               dc:date ?date; 
                  			dc:identifier ?doi.
                  FILTER (?id IN (:Dickes2019)) .} '
      dfout <- evalQuery(rep,
                         query = query, returnType = "dataframe",
                         cleanUp = TRUE, limit = 1000)
      dfout <- dfout[["return"]]
      paste0("<p>", "<strong>", "Title: ", "</strong>", dfout[[1:1]], "</p>" , 
               "<p>", "<strong>", "Year: ", "</strong>", dfout[[2:2]], "</p>", 
               "<p>", "<strong>", "DOI: ", "</strong>" , dfout[[3:3]], "</p>"
               )
        })
  }
)