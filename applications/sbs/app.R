###########################
# Side by Side comparison #
###########################

# This app shows 2-3 studies side by side. 

library(shiny)
library(allegRo)
library(tidyverse)
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
addNameSpace(repo = rep, prefix= "lrmi", nsURI = "http://purl.org/dcx/lrmi-terms/")
addNameSpace(repo = rep, prefix= "arg", nsURI = "http://www.coolfutures.net/rdf/Design_Conjectures/toulminarg#")


# get the data 
# This needs to be moved into the UI with values for 
# the studies passed as parameters 

query = "SELECT ?paper ?property ?value 
WHERE {
  ?paper a dc:BibliographicResource . 
  ?value a skos:Concept . 
  ?paper ?property ?value .
  FILTER (?paper IN (:Dickes2016, :Dickes2019)) .} "

# Run query and store in dfr
dfr <- evalQuery(rep,
                query = query, returnType = "dataframe",
                cleanUp = TRUE, limit = 1000)

# turn into 
studies <- as.data.frame(dfr[1])
colnames(studies) <- c('paper', 'property', 'pvalue')
studies <- stripOffNS(studies)
studies$paper <- fragment(studies$paper)
studies$property <- fragment(studies$property)
studies$pvalue <- fragment(studies$pvalue)


studies <- as_tibble(studies)
studies  <- studies %>%
  pivot_longer(names_from = paper, values_from = pvalue)





# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Old Faithful Geyser Data"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            sliderInput("bins",
                        "Number of bins:",
                        min = 1,
                        max = 50,
                        value = 30)
        ),

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("distPlot")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$distPlot <- renderPlot({
        # generate bins based on input$bins from ui.R
        x    <- faithful[, 2]
        bins <- seq(min(x), max(x), length.out = input$bins + 1)

        # draw the histogram with the specified number of bins
        hist(x, breaks = bins, col = 'darkgray', border = 'white',
             xlab = 'Waiting time to next eruption (in mins)',
             main = 'Histogram of waiting times')
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
