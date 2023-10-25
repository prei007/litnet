#
# skos-based interface
library(shiny)
library(shinyjs)
library(visNetwork)
library(allegRo)
library(readr)
# library(NestedMenu)

###  setup global vars

defaultNS <<- "http://www.learn-web.com/litgraph/"
defaultPrefix <<- "litgraph:"
instanceNS <<- "http://www.learn-web.com/litgraph/"
instancePrefix <<- "litgraph:"
modelNS <<- "http://www.learn-web.com/2023/litrev/"
citoNS <<- "http://purl.org/spar/cito/"
fabioNS <<- "http://purl.org/spar/fabio/"
biboNS <<- "http://purl.org/ontology/bibo/"
dcNS <<- "http://purl.org/dc/terms/"
rdfNS <<- "http://www.w3.org/1999/02/22-rdf-syntax-ns#"
rdsNS <<- "http://www.w3.org/2000/01/rdf-schema#"
foafNS <<- "http://xmlns.com/foaf/0.1/"
oaNS <<- "http://www.w3.org/ns/oa#"
skosNS <<- "http://www.w3.org/2004/02/skos/core#"

# For the elements in ns_list the namespace will not be displayed in tables 
ns_list <<- c(instanceNS, defaultNS, citoNS, fabioNS, biboNS, dcNS, rdfNS, rdsNS, foafNS, oaNS, skosNS)

# aspects should be read from predicates I reckon. 
aspects <<-
  c(
    'ScholarlyWork',
    'Author',
    'Citation',
    'Claim',
    'LearningOutcome',
    'BloomLearningOutcome', 
    'Pedagogy', 
    'EducationLevel', 
    'Science', 
    'Technology', 
    'ResearchApproach'
  )

# Read in info about predicates; declare in global env.
predicates <<- read_csv("predicates.csv", show_col_types = FALSE)
# access a row like so: predicates[ predicates$label == 'creator', ]
# and a particular cell: predicates[ predicates$label == 'creator', 'uri']
# This syntax will always return a tibble. To get to the value do:
# sel <- predicates[ predicates$label == 'creator', 'uri']; then sel[[1]] will yield the value.

# Append the predicate URIs to ns_list. 

ns_list <<- unique(append(ns_list, predicates$uri))

### Build the app 

ui <- fluidPage(useShinyjs(),
                titlePanel("LitGraph"),
                sidebarLayout(
                  sidebarPanel(
                    textInput("userName", "User Name:"),
                    passwordInput("pwd", "Password:"),
                    actionButton("loginButton", "Submit"),
                    p(" "),
                    selectInput("aspect", "Select an aspect:", choices = NULL),
                    selectInput("predicateInput", "Predicate:", choices = NULL),
                    selectizeInput(
                      "subjectInput",
                      "Subject:",
                      multiple = FALSE,
                      choices = NULL,
                      options = list(create = TRUE)
                    ),
                    selectizeInput(
                      "objectInput",
                      "Object:",
                      multiple = FALSE,
                      choices = NULL,
                      options = list(create = TRUE)
                    ),
                    actionButton("saveButton", "Save"),
                    actionButton("deleteButton", "Delete")
                  ),
                  mainPanel(tabsetPanel(
                    type = "tabs",
                    tabPanel("Tables",
                             tableOutput("detailsTable")),
                    tabPanel(
                      "Graph",
                      checkboxGroupInput("linksDisplayed", "Link types to include:",
                                       #  c("ScholarlyWork", "Citation", "Claim"), 
                                         aspects, 
                                         selected = "ScholarlyWork", 
                                         inline = TRUE), 
                      actionButton("showMapButton",  "show map view "),
                      visNetworkOutput("Map", width = "1000px", height = "600px")
                    )
                  ))
                ))

### The server code 

server <- function(input, output, session) {
  
  # -------------------------------
  # Login and database connection
  # -------------------------------
  observeEvent(input$loginButton, {
    # Configure AG connection
    # Using assign() from base R to make variables known outside this reactive context.
    assign("url", "http://learn-web.com/", envir = globalenv())
    assign("userName", input$userName, envir = globalenv())
    assign(
      "service",
      service(url, input$userName, input$pwd, testConnection = FALSE),
      envir = globalenv()
    )
  #  assign("cat", catalog(service, "perei"), envir = globalenv())
    assign("cat", catalog(service, "coolfutures"), envir = globalenv())
    assign("userList", paste0("User", 1:10), envir = globalenv())
    
    if (userName %in% c("perei", "yucui")) {
    #  assign("rep", repository(cat, "skosgraph"), envir = globalenv())
      assign("rep", repository(cat, "compumod2"), envir = globalenv())
    }  else if (exists('userName') & userName %in% userList) {
      assign("rep", repository(cat, "skosgraph"), envir = globalenv())
    } else {
      # still needed: a  way to destroy globalenv vars
      alert("Error: repository could not be allocated. Please log in again.")
      validate("Missing or wrong username.")
    }
    validate(need(input$pwd, "Provide a password"))
    # add namespaces because they are "private" to the logged in user!
    addNameSpace(repo = rep,
                 prefix = "",
                 nsURI = defaultNS)
    addNameSpace(repo = rep,
                 prefix = "litgraph",
                 nsURI = defaultNS)
    addNameSpace(repo = rep,
                 prefix = "foaf",
                 nsURI = foafNS)
    addNameSpace(repo = rep,
                 prefix = "oa",
                 nsURI = oaNS)
    addNameSpace(repo = rep,
                 prefix = "fabio",
                 nsURI =  fabioNS)
    addNameSpace(repo = rep,
                 prefix = "skos",
                 nsURI =  skosNS)
    addNameSpace(repo = rep,
                 prefix = "litrev",
                 nsURI =  "http://www.learn-web.com/2023/litrev/")
    addNameSpace(repo = rep,
                 prefix = "bibo",
                 nsURI =  biboNS)
    
    
    # Reset pwd field
    updateTextInput(session, "pwd", value = NA)
    showNotification("You are logged in")
    
    #  First action in interface: fetch aspects and add to menu for selection
    
    # add namespaces for predicates on server
    add_name_spaces(rep, predicates)
    
    # The list of aspects that will be shown in the aspects selection. 
    # At the same time, they provide rdf:type information for resources in the subject position. 
    
    # first action in interface: show the aspect options for selection
    updateSelectInput(session, "aspect", choices = aspects)
    
  })
  
  # -------------------------------
  # Initialize input fields
  # -------------------------------
  
  # This is the second action: Fill the predicate input field.
  observeEvent(input$aspect, {
    # update predicate field.
    if (input$aspect != "") {
      fill_predicate_input_slot(session, input$aspect)
    }
  })
  
  observeEvent(input$predicateInput, {
    # update subject field field.
    if (input$predicateInput != "") {
      fill_subject_input_slot(session, input$aspect, input$predicateInput)
    }
  })
  
  observeEvent(input$subjectInput, {
    # update object field.
    if (input$subjectInput != "") {
      details_table <<- NULL
      fill_object_input_slot(session,
                             input$aspect,
                             input$predicateInput,
                             input$subjectInput)
      output$detailsTable <- renderTable(details_table) # details_table gets a value from the fill function
    }
  })
  
  #we could also think about:
  # observeEvent(input$objectInput)
  # for analysing the input as to problems.
  
  
  
  # -------------------------------
  # Push a statement
  # -------------------------------
  
  observeEvent(input$saveButton,
               {
                 #subject and predicate are straightforwqrd:
                 subjectNS <- ns_from_input(input$subjectInput)
                 predicateNS <- ns_from_input(input$predicateInput)
                 subjectURL <-
                   paste0("<", subjectNS, remove_prefix(input$subjectInput), ">")
                 predURL <-
                   paste0("<",
                          predicateNS,
                          remove_prefix(input$predicateInput),
                          ">")
                 
                 # Object statements need to be computed in multiple steps
                 # first, there may be no prefix because it's a string input.
                 
                 if (has_prefix(input$objectInput)) {
                   objectNS <- ns_from_input(input$objectInput)
                 } else {
                   objectNS <- ""
                 }
                 
                 # Second, literals needed to formatted differently from objects.
                 # We use the existence of a prefix to decide it it's an object or literal value.
                 objValue <- remove_prefix(input$objectInput)
                 if (has_prefix(input$objectInput)) {
                   objectURL <- paste0("<", objectNS, objValue, ">")
                 } else {
                   objectURL <- paste0('"',  objValue, '"')
                 }
                 
                 cat("\n", "pushing to server: ", "\n") # dev
                 print(c(subjectURL, predURL, objectURL)) # dev
                 
                 addStatement(rep,
                              subj = subjectURL,
                              pred = predURL,
                              obj = objectURL)
                 
                 # Add type info
                 # (Works like so because duplicate statements are surpressed on server)
                 # Needs to be generalized to use the appropriate name spaces. 
                 # https://github.com/prei007/litrev/issues/10
                 
                 # addStatement(
                 #   rep,
                 #   subj = subjectURL,
                 #   pred = "<http://www.w3.org/1999/02/22-rdf-syntax-ns#type>",
                 #   obj = paste0(
                 #     "<",
                 #     modelNS,
                 #     input$aspect,
                 #     ">"
                 #   )
                 # )
                 
                 # Notify user and update table
                 showNotification("Your input is saved.")
                 # update table display
                 details_table <<- show_attributes(input$subjectInput) # note the top level env
                 output$detailsTable <- renderTable(details_table)
             #    click("showMapButton")
               })
  
  # -------------------------------
  # Delete a statement
  # -------------------------------
  
  observeEvent(input$deleteButton, {
    
    subjNS = ns_from_input(input$subjectInput)
    predNS = ns_from_input(input$predicateInput)
    objNS = ns_from_input(input$objectInput)
    subj = paste0('<', subjNS, remove_prefix(input$subjectInput), '>')
    pred  = paste0('<', predNS, remove_prefix(input$predicateInput), '>')
    obj = paste0('<', objNS, remove_prefix(input$objectInput), '>')
    
    cat("\n", "Deleting from database: ", "\n", subj, "\n", pred, "\n", obj, "\n" ) #dev
    deleteStatements(rep, subj = subj, pred = pred, obj = obj)
    # update table display
    details_table <<- show_attributes(input$subjectInput) # note the top level env
    output$detailsTable <- renderTable(details_table)
    # Notify user and offer undo
    showNotification("The statement has been deleted from the database. 
                     To undo, click 'Save'. To save a modified version, enter new value(s) and save.")
    
  })
  
  
  # -------------------------------
  # Show map/network
  # -------------------------------
  
  observeEvent(input$showMapButton, {
    # fetch the properties to be displayed by looking up their value in the 
    # variable predicates based on the selection(s) in the checkbox group. 
    linkList <- NULL
    for (link_type in input$linksDisplayed) {
      linkList1 <- predicates[predicates$aspect == link_type, 'label']
      linkList1 <- linkList1[[1]]
      linkList <- append(linkList, linkList1)
    }

    # Use the fact that paste0() is vectorised to turn the vector 
    # into a SPARQL list as different from a list data structure in R. 
    # That is to say, list() and as.list() will not do the job.
    linkList <- paste0(linkList, collapse = ', ')
    
    query <- paste0('SELECT ?s ?p ?o { ?s ?p ?o . FILTER (?p IN (', linkList, ')) }')
    graphDF <- fetch_plan_sparql(query)
    # improve the test because it comes too late. ag_data already throws an error before
    # we get here. 
    if (graphDF[1] != "query failed" && length(graphDF) > 1) {
      # render map
      output$Map <- renderVisNetwork(do_network(graphDF))
      #  output$Map <- renderVisNetwork(do_network_2(graphDF))
    } else {
      showNotification("The database does not contain (sufficient) information.",
                       type = "warning")
    }
  })
  
  # Show information for selected node in panel.
  # The value for this query comes from visNetwork .
  
  # observeEvent(input$current_node_id$node, {
  #   render_plan_node(input$current_node_id$node)
  # })
  #
  # observeEvent(input$current_edge_id$node, {
  #   render_network_edge(input$current_edge_id$edge)
  # })
  
}

# Run the application
shinyApp(ui = ui, server = server)
