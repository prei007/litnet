#
# skos-based interface
library(shiny)
library(shinyjs)
library(visNetwork)
library(allegRo)
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
                             tabPanel("Graph", 
                                      # show user name
                                      actionButton("showMapButton",  "show map view "),
                                      p(" Network here "),
                                      visNetworkOutput("Map", width = "1000px", height = "600px")
                                      #    DTOutput('tbl')
                             ),
                             tabPanel("Table", 
                                      p(" Table here"))
                             
                 )
               )
             )
  )


server <- function(input, output, session) {
  
  defaultNS <- "http://www.learn-web.com/litgraph/"
  modelNS <- "http://www-learnweb.com/2023/litrev/"
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
    addNameSpace(repo = rep, prefix = "litrev", nsURI =  "http://www.learn-web.com/2023/litrev/")
    
    
    # Reset pwd field
    updateTextInput(session, "pwd", value = NA)
    showNotification("You are logged in")
    
    #  fetch aspects and add to menu for selection
    aspects <- fetch_one_column('PREFIX litrev: <http://www-learnweb.com/2023/litrev/>
            SELECT ?aspect WHERE {
            ?aspect rdfs:subClassOf litrev:ReviewAspect 
            }')
    # The above needs to be generalised to included rdf classes as well. 
    # Schemes are classes and properties in the doman model rather than concept scheme definitions. 
    add_thesaurus_namespace() 
    updateSelectInput(session, "aspect", choices = aspects)
    
  })
  
  # -------------------------------
  # Initialize input fields
  # -------------------------------
  
# scheme_name <- reactive(input$scheme)
  observeEvent(input$aspect, {
    # update predicate field. 
    if (input$aspect != "") {
      updateSelectInput(session, "predicateInput", 
                        choices = fill_predicate_input_slot(input$aspect),
                        selected  = NULL)
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
  
  # When a predicate is selected, update the subjectInput: 
  observeEvent(input$predicateInput, {
    if (input$predicateInput != "") {
      updateSelectizeInput(session, "subjectInput", choices = fill_subject_input_slot(input$aspect, input$predicateInput),
                           options = list(create = TRUE), selected = NULL)
       updateSelectizeInput(session, "objectInput", choices = fill_object_input_slot(input$aspect, input$predicateInput),
                         options = list(create = TRUE), selected = NULL)
    }
  })
  
 
  # -------------------------------
  # Push a statement 
  # -------------------------------
  
  observeEvent(input$submitButton, {
    subjectURL <- paste0("<", defaultNS, input$subjectInput, ">")
    predURL <- paste0("<", modelNS, input$predicateInput, ">")
       objectScheme <- find_scheme_from_predicate(input$aspect)
       objectNS <- lookup_namespace(objectScheme)
    objectURL <- paste0("<", objectNS, input$objectInput, ">")
    addStatement(rep, subj=subjectURL, pred=predURL, obj=objectURL)
      
      # Notify user and save 
      showNotification("Your input is saved.")
      click("showMapButton")
  })
  

  
  # -------------------------------
  # Show map/network
  # -------------------------------
  observeEvent(input$showMapButton, {
    query <- 'PREFIX litrev: <http://www-learnweb.com/2023/litrev/> 
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
