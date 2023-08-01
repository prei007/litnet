
server <- function(input, output, session) {
  
  # namespaces 
  defaultNS <- "http://www.learn-web.com/litgraph/"
  citoNS <- "http://purl.org/spar/cito/"
  fabioNS <- "http://purl.org/spar/fabio/"
  dcNS <- "http://purl.org/dc/terms/"
  rdfNS <- "http://www.w3.org/1999/02/22-rdf-syntax-ns#"
  rdsNS <- "http://www.w3.org/2000/01/rdf-schema#"
  foafNS <- "http://xmlns.com/foaf/0.1/"
  oaNS <- "http://www.w3.org/ns/oa#"
  
  ns_list <- c(defaultNS, citoNS, fabioNS, dcNS, rdfNS, rdsNS, foafNS, oaNS)
  
  # -------------------------------
  # Manage hidden panels in the tabset panel
  # -------------------------------
  observeEvent(input$template, {
    updateTabsetPanel(inputId = "templates", selected = input$template)
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
      assign("rep", repository(cat, "litgraph"), envir = globalenv())
    }  else if (exists('userName') & userName %in% userList) {
      assign("rep", repository(cat, "litgraph"), envir = globalenv())
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
    
    # Reset input fields
    updateTextInput(session, "pwd", value = NA)
    output$userName <- renderText( { paste0("Logged-in user: ", input$userName) } )
    click("showMapButton")
    
    # Notify user
    showNotification("You are logged in")
    
  })
  
  # -------------------------------
  # Push publication
  # -------------------------------
  observeEvent(input$savePubButton, {
    if (node_exists(input$pubID) == "false") {
      # ID for all statements in this scheme
      subjectURL <- paste0("<", defaultNS, input$pubID, ">")
      updateTextInput(session, "pubID", value = NA)
      # rdf type
      predURL <- paste0("<", rdfNS, "type", ">" )
      objectURL <- paste0("<", fabioNS, "JournalArticle", ">" )
      addStatement(rep, subj=subjectURL, pred=predURL, obj=objectURL)
      # author (currently one only)
      predURL <- paste0("<", dcNS, "creator", ">" )
      objectURL <- paste0("<", defaultNS, input$pubAuthor, ">" )
      addStatement(rep, subj=subjectURL, pred=predURL, obj=objectURL)
      updateTextInput(session, "pubAuthor", value = NA)
      # title
      predURL <- paste0("<", fabioNS, "title", ">" )
      objectURL <- paste0('"',  input$pubTitle, '"')
      addStatement(rep, subj=subjectURL, pred=predURL, obj=objectURL)
      updateTextInput(session, "pubTitle", value = NA)
      # year/date
      predURL <- paste0("<", dcNS, "created", ">" )
      objectURL <- paste0('"',  input$pubYear, '"')
      addStatement(rep, subj=subjectURL, pred=predURL, obj=objectURL)
      updateTextInput(session, "pubYear", value = NA)
      # citation (DOI)
      predURL <- paste0("<", dcNS, "identifier", ">" )
      objectURL <- paste0('"',  input$pubIdentifier, '"')
      addStatement(rep, subj=subjectURL, pred=predURL, obj=objectURL)
      updateTextInput(session, "pubIdentifier", value = NA)
      # add user and time
      predURL <- paste0("<", defaultNS, "addedBy", ">" )
      objectURL <- paste0("<", defaultNS, userName, ">")
      addStatement(rep, subj=subjectURL, pred=predURL, obj=objectURL)
      predURL <- paste0("<", defaultNS, "addedDate", ">" )
      objectURL <- paste0('"',  Sys.Date(), '"')
      addStatement(rep, subj=subjectURL, pred=predURL, obj=objectURL)
      
      # Notify user and save 
      showNotification("Your input is saved.")
      click("showMapButton")
    } else {
      alert("This element already exists. Click 'Update' instead.")
    }
  })
  
  # -------------------------------
  # Push CITO citation 
  # -------------------------------
  
  observeEvent(input$saveCitationButton, {
    if (node_exists(input$citationID) == "false") {
      # ID for all statements in this scheme
      subjectURL <- paste0("<", defaultNS, input$citationID, ">")
      updateTextInput(session, "citationID", value = random_name('CITE-'))
      # rdf type
      predURL <- paste0("<", rdfNS, "type", ">" )
      objectURL <- paste0("<", citoNS, "Citation", ">" )
      addStatement(rep, subj=subjectURL, pred=predURL, obj=objectURL)
      # citing entity (not a literal!)
      predURL <- paste0("<", citoNS, "hasCitingEntity", ">" )
      objectURL <- paste0("<", defaultNS, input$citingEntity, ">" )
      addStatement(rep, subj=subjectURL, pred=predURL, obj=objectURL)
      updateTextInput(session, "citingEntity", value = NA)
      # citation type (not a literal!)
      predURL <- paste0("<", citoNS, "hasCitationCharacterization", ">" )
      objectURL <- paste0("<",  citoNS, input$citoType, ">")
      addStatement(rep, subj=subjectURL, pred=predURL, obj=objectURL)
      updateTextInput(session, "citoType", value = NA)
      # cited entity (not a literal!)
      predURL <- paste0("<", citoNS, "hasCitedEntity", ">" )
      objectURL <- paste0("<",  defaultNS, input$citedEntity, ">")
      addStatement(rep, subj=subjectURL, pred=predURL, obj=objectURL)
      updateTextInput(session, "citedEntity", value = NA)
      # add user (as object) and time
      predURL <- paste0("<", defaultNS, "addedBy", ">" )
      objectURL <- paste0("<", defaultNS, userName, ">")
      addStatement(rep, subj=subjectURL, pred=predURL, obj=objectURL)
      predURL <- paste0("<", defaultNS, "addedDate", ">" )
      objectURL <- paste0('"',  Sys.Date(), '"')
      addStatement(rep, subj=subjectURL, pred=predURL, obj=objectURL)
      # Notify user and save 
      showNotification("Your input is saved.")
      click("showMapButton")
    } else {
      alert("This element already exists. Click 'Update' instead, or rename and save.")
    }
  })
  
  # -------------------------------
  # Push a free annotation citation 
  # -------------------------------
  observeEvent(input$saveAnnotationButton, {
    if (node_exists(input$annoID) == "false") {
      # ID for all statements in this scheme
      subjectURL <- paste0("<", defaultNS, input$annoID, ">")
      updateTextInput(session, "annoID", value = random_name('ANNO-'))
      # rdf type
      predURL <- paste0("<", rdfNS, "type", ">" )
      objectURL <- paste0("<", oaNS, "Annotation", ">" )
      addStatement(rep, subj=subjectURL, pred=predURL, obj=objectURL)
      # annotation target (not a literal)
      predURL <- paste0("<", oaNS, "hasTarget", ">" )
      objectURL <- paste0("<", defaultNS, input$annoTarget, ">" )
      addStatement(rep, subj=subjectURL, pred=predURL, obj=objectURL)
      updateTextInput(session, "annoTarget", value = NA)
      # annotation body (literal)
      predURL <- paste0("<", oaNS, "hasBody", ">" )
      objectURL <- paste0('"', input$annoBody, '"')
      addStatement(rep, subj=subjectURL, pred=predURL, obj=objectURL)
      updateTextInput(session, "annoBody", value = NA)
      # annotation motivation
      predURL <- paste0("<", oaNS, "motivatedBy", ">" )
      objectURL <- paste0("<",  oaNS, input$annoMotivation, ">")
      addStatement(rep, subj=subjectURL, pred=predURL, obj=objectURL)
      updateTextInput(session, "annoMotivation", value = NA)
      # add user (as object) and time
      predURL <- paste0("<", defaultNS, "addedBy", ">" )
      objectURL <- paste0("<", defaultNS, userName, ">")
      addStatement(rep, subj=subjectURL, pred=predURL, obj=objectURL)
      predURL <- paste0("<", defaultNS, "addedDate", ">" )
      objectURL <- paste0('"',  Sys.time(), '"')
      addStatement(rep, subj=subjectURL, pred=predURL, obj=objectURL)
      # Notify user and save 
      showNotification("Your input is saved.")
      click("showMapButton")
    } else {
      alert("This element already exists. Click 'Update' instead, or rename and save.")
    }
  })
  
  # -------------------------------
  # Show map/network
  # -------------------------------
  observeEvent(input$showMapButton, {
    query = paste0( 
      'CONSTRUCT  {?s ?p ?o} WHERE {
      ?citation a cito:Citation ;
  	  cito:hasCitingEntity ?s ;
      cito:hasCitationCharacterization ?p ; 
      cito:hasCitedEntity ?o. 
      }' 
    )
    graphDF <- fetch_plan_sparql(query)
    if (graphDF[1] != "query failed" & length(graphDF) > 1) { 
      # render map
      output$Map <- renderVisNetwork(do_network(graphDF))
    } else {
      showNotification("The plan does not contain (sufficient) information.", 
                       type = "warning") }
  })
  
  # Show information for selected node in panel. 
  # The value for this query comes from visNetwork . 
  
  observeEvent(input$current_node_id$node, {
    render_plan_node(input$current_node_id$node)
  })
}

server
