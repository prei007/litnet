#
# skos-based interface
library(shiny)
library(shinyjs)
library(visNetwork)
library(allegRo)
library(readr)
# library(NestedMenu)

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
                    actionButton("saveButton", "Submit (new)"),
                    actionButton("modifyButton", "Modify")
                  ),
                  mainPanel(tabsetPanel(
                    type = "tabs",
                    tabPanel("Tables",
                             tableOutput("detailsTable")),
                    tabPanel(
                      "Graph",
                      # show user name
                      actionButton("showMapButton",  "show map view "),
                      p(" Network here "),
                      visNetworkOutput("Map", width = "1000px", height = "600px")
                    )
                  ))
                ))


server <- function(input, output, session) {
  defaultNS <<- "http://www.learn-web.com/litgraph/"
  defaultPrefix <<- "litgraph:"
  instanceNS <<- "http://www.learn-web.com/litgraph/"
  instancePrefix <<- "litgraph:"
  modelNS <<- "http://www.learn-web.com/2023/litrev/"
  citoNS <<- "http://purl.org/spar/cito/"
  fabioNS <<- "http://purl.org/spar/fabio/"
  dcNS <<- "http://purl.org/dc/terms/"
  rdfNS <<- "http://www.w3.org/1999/02/22-rdf-syntax-ns#"
  rdsNS <<- "http://www.w3.org/2000/01/rdf-schema#"
  foafNS <<- "http://xmlns.com/foaf/0.1/"
  oaNS <<- "http://www.w3.org/ns/oa#"
  skosNS <<- "http://www.w3.org/2004/02/skos/core#"
  
  # For the elements in ns_list the namespace will not be displayed in tables 
  ns_list <<- c(defaultNS, citoNS, fabioNS, dcNS, rdfNS, rdsNS, foafNS, oaNS, skosNS)
  
  
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
    
    
    # Reset pwd field
    updateTextInput(session, "pwd", value = NA)
    showNotification("You are logged in")
    
    #  First action in interface: fetch aspects and add to menu for selection
    # aspects <- fetch_one_column('PREFIX litrev: <http://www.learn-web.com/2023/litrev/>
    #         SELECT ?aspect WHERE {
    #         ?aspect rdfs:subClassOf litrev:ReviewAspect
    #       }')
    
    # Read in info about predicates; declare in global env.
    predicates <<- read_csv("predicates.csv")
    # access a row like so: predicates[ predicates$label == 'creator', ]
    # and a particular cell: predicates[ predicates$label == 'creator', 'uri']
    # This syntax will always return a tibble. To get to the value do:
    # sel <- predicates[ predicates$label == 'creator', 'uri']; then sel[[1]] will yield the value.
    
    # add namespaces for predicates on server
    add_name_spaces(rep, predicates)
    
    aspects <-
      c(
        'ScholarlyWork',
        'Author',
        'Citation',
        'Claim',
        'LearningOutcome',
        'ResearchApproach', 
        'BloomLearningOutcome', 
        'Pedagogy', 
        'EducationLevel'
      )
    
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
                 
                 addStatement(
                   rep,
                   subj = subjectURL,
                   pred = "<http://www.w3.org/1999/02/22-rdf-syntax-ns#type>",
                   obj = paste0(
                     "<",
                     modelNS,
                     input$aspect,
                     ">"
                   )
                 )
                 # Notify user and save
                 showNotification("Your input is saved.")
                 click("showMapButton")
               })
  
  
  
  # -------------------------------
  # Show map/network
  # -------------------------------
  observeEvent(input$showMapButton, {
    query <- 'PREFIX litrev: <http://www.learn-web.com/2023/litrev/>
    SELECT ?s ?p ?o {
         ?s a litrev:ScholarlyWork .
         ?s ?p ?o .
         FILTER (!(?p IN (:addedDate, :addedBy, rdf:type))) }'
    graphDF <- fetch_plan_sparql(query)
    if (graphDF[1] != "query failed" & length(graphDF) > 1) {
      # render map
      output$Map <- renderVisNetwork(do_network(graphDF))
      #  output$Map <- renderVisNetwork(do_network_2(graphDF))
    } else {
      showNotification("The plan does not contain (sufficient) information.",
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
