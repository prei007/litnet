#
# skos-based interface
library(shiny)
library(shinyjs)
library(visNetwork)
library(allegRo)
library(shinytreeview)


# https://rdrr.io/github/dreamRs/shinytreeview/


l1 <- c("QualitativeMethod", "QualitativeMethod", "QualitativeMethod" ,
        "QuantitativeMethod", "QuantitativeMethod", "QuantitativeMethod",
        "QuantitativeMethod", "QuantitativeMethod")
l2 <- c("ObservationMethod", "InterviewMethod", "CaseStudyMethod",
        "PerformanceAnalyis", "SurveyMethod", "InferentialStatistics",
        "InferentialStatistics", "InferentialStatistics")
l3 <- c("NA","NA","NA",
        "NA" ,"NA", "NA",
        "T-Test", "ANOVA")

schemes <- data.frame(l1, l2, l3)


ui <- fluidPage(
  useShinyjs(), 
  titlePanel("LitGraph"), 
             sidebarLayout(
               sidebarPanel(
                 textInput("userName", "User Name:"), 
                 passwordInput("pwd", "Password:"),
                 actionButton("loginButton", "Submit"),
                 p(" "),
                 # selectInput("Schemes", "Input category:", choices = NULL),
                 # textInput("Subject", "Subject:"),
                 # selectInput("Predicate", "Predicate", choices = c("A", "B", "C")),
                 # textInput("Object", "Object:"),
                 # actionButton("SubmitButton", "Submit")
                 treeviewInput(
                   inputId = "tree",
                   label = "Choose a category:",
                   choices = make_tree(
                     schemes, c("l1", "l2", "l3")
                   ),
                   multiple = FALSE,
                   prevent_unselect = TRUE,
                   width = "100%"
                 ),
                 verbatimTextOutput(outputId = "result")
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
server <- function(input, output, session) {
  
  defaultNS <- "http://www.learn-web.com/litgraph/"
  citoNS <- "http://purl.org/spar/cito/"
  fabioNS <- "http://purl.org/spar/fabio/"
  dcNS <- "http://purl.org/dc/terms/"
  rdfNS <- "http://www.w3.org/1999/02/22-rdf-syntax-ns#"
  rdsNS <- "http://www.w3.org/2000/01/rdf-schema#"
  foafNS <- "http://xmlns.com/foaf/0.1/"
  oaNS <- "http://www.w3.org/ns/oa#"
  skosNS <- "http://www.w3.org/2004/02/skos/core#"
  
  ns_list <<- c(defaultNS, citoNS, fabioNS, dcNS, rdfNS, rdsNS, foafNS, oaNS, skosNS)
  
  
  # -------------------------------
  # Login and database connection
  # -------------------------------
  observeEvent(input$loginButton, {
    # Configure AG connection
    # Using assign() from base R to make variables known outside this reactive context. 
    assign("url", "http://learn-web.com/", envir = globalenv())
    assign("userName", input$userName, envir = globalenv())
    assign("service", service(url, input$userName, input$pwd, testConnection = FALSE),
           envir = globalenv())
    assign("cat", catalog(service, "perei"), envir = globalenv())
    assign("userList", paste0("User", 1:10), envir = globalenv())
    
    if (userName %in% c("perei", "yucui")) {
      assign("rep", repository(cat, "skosgraph"), envir = globalenv())
    }  else if (exists('userName') & userName %in% userList) {
      assign("rep", repository(cat, "skosgraph"), envir = globalenv())
    } else {
      # still needed: a  way to destroy globalenv vars
      alert("Error: repository could not be allocated. Please log in again.")
      validate("Missing or wrong username.")
    }
    validate(need(input$pwd, "Provide a password" ))
    # add namespaces because they are "private" to the logged in user! 
    addNameSpace(repo = rep, prefix = "", nsURI = defaultNS)
    addNameSpace(repo = rep, prefix = "cito", nsURI = citoNS)
    addNameSpace(repo = rep, prefix = "foaf", nsURI = foafNS)
    addNameSpace(repo = rep, prefix = "oa", nsURI = oaNS)
    addNameSpace(repo = rep, prefix = "fabio", nsURI =  fabioNS)
    addNameSpace(repo = rep, prefix = "skos", nsURI =  skosNS)
    
    # Reset pwd field
    updateTextInput(session, "pwd", value = NA)
    showNotification("You are logged in")
    
    #  fetch the name of the skos themes in the database
    # cat_schemes <- fetch_cat_schemes()
    # updateSelectInput(session, "Schemes", choices = cat_schemes)
 
  })
  
  # treeview
  output$result <- renderPrint({
    input$tree
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
