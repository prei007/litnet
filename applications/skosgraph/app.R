#
# skos-based interface
library(shiny)
library(shinyjs)
library(visNetwork)
library(allegRo)
library(NestedMenu)

resmethods <- list(
  quantitative = list(
    name = "Quantitative",
    items = list(
      survey = list(
        name = "Survey",
        items = list(
          s1 = list(name = "S1"),
          s2 = list(name = "S2")
        )
      ),
      experiment = list(
        name = "Experiment",
        items = list(
          quasi = list(name = "Quasiexperiment"),
          scdr = list(name = "SCDR")
        )
      ),
      quant_other = list(name = "Other_quantitative")
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


ui <- fluidPage(
  useShinyjs(), 
  titlePanel("LitGraph"), 
             sidebarLayout(
               sidebarPanel(
                 textInput("userName", "User Name:"), 
                 passwordInput("pwd", "Password:"),
                 actionButton("loginButton", "Submit"),
                 p(" "),
                 selectInput("scheme", "Select an aspect:", choices = NULL),
                 textInput("subjectInput", "Subject:"),
                 selectInput("predicateInput", "Predicate:", choices = NULL),
                 # placeholder for dynamically created menu button:
                 uiOutput("predicateMenu"),
                # Further with rendering input elements:
                 textInput("objectInput", "Object:"),
                 actionButton("SubmitButton", "Submit")
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
  
# scheme_name <- reactive(input$scheme)
  observeEvent(input$scheme, {
    # update predicate field. 
    if (input$scheme != "") {
      updateSelectInput(session, "predicateInput", choices = fill_predicate_input_slot(input$scheme))
    }
    # Place a nested menu close to predicate input for hierachical options. 
    # This is done creating the menu button dynamically (in the end). 
    #
    # output$statementInput <- renderUI({
    #   tagList(
    #     NestedMenuOutput("predicateMenu", height = "auto"))
    # })
    # output[["predicateMenu"]] <- renderNestedMenu({
    #   NestedMenu("researchMethod", items = resmethods)
    # })
  })
  
  
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
    cat_schemes <- fetch_cat_schemes()
    updateSelectInput(session, "scheme", choices = cat_schemes)
 
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
