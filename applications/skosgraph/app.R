#
# skos-based interface
library(shiny)
library(shinyjs)
library(visNetwork)
library(allegRo)
library(readr)
# library(NestedMenu)

ui <- fluidPage(
  useShinyjs(), 
  titlePanel("LitGraph"), 
             sidebarLayout(
               sidebarPanel(
                 textInput("userName", "User Name:"), 
                 passwordInput("pwd", "Password:"),
                 actionButton("loginButton", "Submit"),
                 p(" "),
                 selectInput("aspect", "Select an aspect:", choices = NULL),
                 selectInput("predicateInput", "Predicate:", choices = NULL),
                 selectizeInput("subjectInput", "Subject:", multiple = FALSE, 
                                  choices = NULL, 
                                options = list(create = TRUE)),
                 selectizeInput("objectInput", "Object:", multiple = FALSE, 
                                choices = NULL, 
                                options = list(create = TRUE)),
                 actionButton("submitButton", "Submit")
               ),
               mainPanel(
                 tabsetPanel(type = "tabs", 
                             tabPanel("Table", 
                                      tableOutput("work")),
                             tabPanel("Graph", 
                                      # show user name
                                      actionButton("showMapButton",  "show map view "),
                                      p(" Network here "),
                                      visNetworkOutput("Map", width = "1000px", height = "600px")
                                      #    DTOutput('tbl')
                             )
                 )
               )
             )
  )


server <- function(input, output, session) {
  
  defaultNS <<- "http://www.learn-web.com/litgraph/"
  defaultPrefix <<- "litgraph:"
  instanceNS <<- "http://www.learn-web.com/litgraph/"
  instancePrefix <<- "litgraph:"
  modelNS <<- "http://www-learnweb.com/2023/litrev/"
  citoNS <<- "http://purl.org/spar/cito/"
  fabioNS <<- "http://purl.org/spar/fabio/"
  dcNS <<- "http://purl.org/dc/terms/"
  rdfNS <<- "http://www.w3.org/1999/02/22-rdf-syntax-ns#"
  rdsNS <<- "http://www.w3.org/2000/01/rdf-schema#"
  foafNS <<- "http://xmlns.com/foaf/0.1/"
  oaNS <<- "http://www.w3.org/ns/oa#"
  skosNS <<- "http://www.w3.org/2004/02/skos/core#"
  
  # ns_list <<- c(defaultNS, citoNS, fabioNS, dcNS, rdfNS, rdsNS, foafNS, oaNS, skosNS)
  
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
    addNameSpace(repo = rep, prefix = "litgraph", nsURI = defaultNS)
    addNameSpace(repo = rep, prefix = "foaf", nsURI = foafNS)
    addNameSpace(repo = rep, prefix = "oa", nsURI = oaNS)
    addNameSpace(repo = rep, prefix = "fabio", nsURI =  fabioNS)
    addNameSpace(repo = rep, prefix = "skos", nsURI =  skosNS)
    addNameSpace(repo = rep, prefix = "litrev", nsURI =  "http://www.learn-web.com/2023/litrev/")
    
    
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
    
    aspects <- c('ScholarlyWork', 'Author', 'Citation', 'Claim', 'LearningOutcome', 'ResearchApproach')
    
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
    if(input$predicateInput != "") {
      fill_subject_input_slot(session, input$aspect, input$predicateInput)
    }
  })

  observeEvent(input$subjectInput, {
    # update object field.
    if(input$subjectInput != "") {
      # work_table <<- NULL
      fill_object_input_slot(session, input$aspect, input$predicateInput, input$subjectInput)
      output$work <- renderTable(work_table) # work_table gets a value from the fill function
    }
  })
  
  
  
 
  # -------------------------------
  # Push a statement 
  # -------------------------------
  
  observeEvent(input$submitButton, { 
    subjectURL <- paste0("<", defaultNS, input$subjectInput, ">")
    predURL <- paste0("<", modelNS, input$predicateInput, ">")
    objectScheme <- find_scheme_from_predicate(input$aspect) # check this. 
    objectNS <- lookup_namespace(objectScheme)
    # This is too simple. Does not consider the range. For instance, 
    # literals versus other. 
    objectURL <- paste0("<", defaultNS, input$objectInput, ">")
    addStatement(rep,
                 subj = subjectURL,
                 pred = predURL,
                 obj = objectURL)
    cat("\n", "pushed to server: ", "\n") # dev 
    print(c(subjectURL, predURL, objectURL)) # dev 
    # Add type info 
    # (Works like so because duplicate statements are surpressed on server)
    addStatement(
      rep,
      subj = subjectURL,
      pred = "<http://www.w3.org/1999/02/22-rdf-syntax-ns#type>",
      obj = paste0(
        "<",
        "http://www.learn-web.com/2023/litrev/",
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
                       type = "warning") }
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
