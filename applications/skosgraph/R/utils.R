##### helper functions

# Wrap allegRo::addStatement() so that it does not add empty object nodes.
# Used mainly for tutor messages and media fields. The other fields should always have a value.
addStatement1 <- function(repo, subjectURL, predURL, objectURL, contextURL) {
  generic <- paste0('<', defaultNS, '>') 
  # Tests for nchar > 2 because the empty string comes in quoted form, so has length 2 
  # even if empty. Grrr
  if (nchar(objectURL) > 2 && objectURL != "<''>" && objectURL != generic) {
    addStatement(repo, subj=subjectURL, pred=predURL, obj=objectURL, context=contextURL)
  }
}

# removes < and > from elements in a dataframe. 
stripOffNS <- function(df) {
  df <- lapply(df, function(x) {gsub(">", "", x)})
  df <- lapply(df, function(x) {gsub("<", "", x)})
  df
}

random_name <- function(prefix) {
  v1 <- paste(sample(letters, 6), collapse = "")
  v2 <- floor(runif(1, min = 100, max = 999))
  paste0(prefix, v1, v2)
}

# return last element in list with URI paths as elements
last_URI_element <- function(l1) {
  l2 <- strsplit(l1, "/+")
  # replace with last component of the url
  l2 <- sapply(l2, function(x)
    x[length(x)])
  # ditto for fragments (#)
  l2 <- strsplit(l2, "#+")
  l2 <- sapply(l2, function(x)
    x[length(x)])
  l2
  
}


# version for object column. 
last_URI_element_1 <- function(l1, ns_list) {
   l2 <- list() 
  for (i in 1:length(l1)) {
    for (this_name in ns_list) {
      # pick apart the URL if it includes a namespace
      if (grepl(this_name, l1[i])) {
        splitList <- strsplit(l1[i], "/+")
        # after splitting, some rows in splitList will be a list. 
        # We want the last element of that list to become the sole row element
        splitList <- splitList[[1]]
        lastpos <- length(splitList)
        l2[i] <- splitList[lastpos]
        break
      } 
      # else leave the URL as is. 
      l2[i] <- l1[i]
    }
  }
   unlist(l2, recursive = FALSE)
    # same for fragments (#)
    l3 <- list()
    for (i in 1:length(l2)) {
      for (this_name in ns_list) {
        # pick apart the URL if it includes a namespace
        if (grepl(this_name, l2[i])) {
          splitList <- strsplit(l2[i], "#+")
          splitList <- splitList[[1]]
          lastpos <- length(splitList)
          l3[i] <- splitList[lastpos]
          break
        } 
        l3[i] <- l2[i]
      }
  }
  unlist(l3, recursive = FALSE)
}



# Check if a node exists on server
# Returns "true" or "false"
node_exists <- function(node) {
  query <- paste0('ASK {:', node, ' ?p ?o }')
  evalQuery(rep,
          query = query, returnType = "list",
          limit = 1)
}

value_exists <- function(node, predicate) {
  # note that both parameters need to come with prefixes 
  query <- paste0('{ ', node, ' ', predicate, '?o }')
  evalQuery(rep,
            query = query, returnType = "list",
            limit = 1)
}


# update the view with the plan table
fetch_plan <- function() {
  dfout <- (getStatements(rep,
                          context = userName, 
                          returnType = "matrix",
                          limit = 2000,
                          cleanUp = cleanUp))
  # getStatements() returns a complex http response object.
  # The query result is in [[return]. 
  dfout <- stripOffNS(as.data.frame(dfout[["return"]]))
  dfout[[1]] <- last_URI_element(dfout[[1]])
  dfout[[2]] <- last_URI_element(dfout[[2]])
  dfout[[3]] <- last_URI_element_1(dfout[[3]], ns_list) # object column needs special consideration. 
  dfout <- as.data.frame(dfout)
  colnames(dfout) = c("subject", "predicate", "object")
  dfout
}

# update the view with the graph
fetch_plan_sparql <- function(query) {
  dfout <- evalQuery(rep,
                     query = query, returnType = "dataframe",
                     cleanUp = TRUE, limit = 2000)
  # The query result is in [[return]. 
  
  if (dfout[1] != "query failed" & length(dfout) > 1) {
    dfout <- stripOffNS(as.data.frame(dfout[["return"]]))
    dfout[[1]] <- last_URI_element(dfout[[1]])
    dfout[[2]] <- last_URI_element(dfout[[2]])
    dfout[[3]] <- last_URI_element_1(dfout[[3]], ns_list) 
    dfout
  } else {
    showNotification("The plan does not contain (sufficient) information about this element.", 
                     type = "error")
  }
}

# Fetch predicates and objects for one subject
fetch_plan_node <- function(node) {
  if (node_exists(node) == "true") {
        query <- paste0(
          'SELECT ?p ?o  WHERE {:', node, ' ?p ?o}')
        dfout <- evalQuery(rep,
                           query = query, returnType = "dataframe",
                           cleanUp = TRUE, limit = 2000)
          dfout <- stripOffNS(as.data.frame(dfout[["return"]]))
          dfout[[1]] <- last_URI_element(dfout[[1]])
          dfout[[2]] <- last_URI_element_1(dfout[[2]], ns_list) 
          dfout
  } else {
    alert("Selected element does not exist yet on the server. Please create it.")
  }
}

# render a network graph 
do_network <- function(dfout) {
  # dfout has three columns: s, p, o. 
  
  # Nodes is a  list combining s and o nodes. 
  nodes1 <- c(dfout[[1]], dfout[[3]])
  nodes <- list()
  nodes[1] <- list(unique(nodes1))
  # add labels
  nodes[2] <- nodes[1]
  # name columns so that visNetwork understands. 
  nodes <- data.frame(nodes)
  colnames(nodes) = c("id", "label")
  
  # To add a column `group` based on rdf:type, use get_all_node_types() 
  # and match the type against nodes based on the id in nodes and the 
  # dataframe returned from the function. In other words, a Join 
  # based on id. 
  
  # node_types <- data.frame(get_all_node_types())
  # colnames(node_types) <- c("id", "type")
  # for (n in 1:length(nodes$id)) {
  #   for (t in 1:length(node_types$id)) {
  #     if (nodes$id[n] == node_types$id[t]) {
  #       nodes$group[[n]] <- node_types$type[t]
  #     }
  #   }
  # }
  
  # Edges: 
 
  edges <<- data.frame(from = dfout[[1]], 
                      to = dfout[[3]], 
                      label = dfout[[2]])
  # an ID for edges that gets returned in current_edge_id upon mouse selection.
  edges$id <- 1:nrow(edges)
  # print(edges)  # development only


  # Display graph, and a table with node details dependent on mouse click: 
  # https://rdrr.io/cran/visNetwork/man/visEvents.html
  
  visNetwork(nodes, edges, height = "1500px", width = "1500px") %>% 
    visNodes(shape = "box") %>%
    visEdges(arrows = "to") %>%
    visInteraction(hideEdgesOnDrag = TRUE)  %>%
    visOptions(highlightNearest = FALSE, nodesIdSelection = TRUE) %>%
    visLayout(randomSeed = 123) %>%
    visPhysics(solver = "forceAtlas2Based") %>%
    visEvents(selectNode = "function(nodes) {
        Shiny.setInputValue('current_node_id', nodes);
      ;}") %>%
   visEvents(selectEdge = "function(edges) {
        Shiny.setInputValue('current_edge_id', edges);
      ;}")


}

# Function finds the node type(s) for a node
get_node_type <- function(node) {
  # cat("\n", "get_node_type(node):" , " node: ", as.character(node), "\n")
  if (node_exists(node) == "true") {
    query <- paste0(
      'SELECT ?type WHERE { :', 
      node, 
      ' a ?type }' 
      )
    dfout <- evalQuery(rep,
                       query = query, returnType = "dataframe",
                       cleanUp = TRUE, limit = 2000)
    if (dfout[1] != "query failed" & length(dfout) > 1) {
      dfout <- stripOffNS(as.data.frame(dfout[["return"]]))
      node_type <- as.character(last_URI_element_1(dfout[[1]], ns_list))
      node_type <- node_type[[1]]
    } else {
      node_type <- "NA"
    } 
  } else {
    alert("Plan element does not exist on database. Please create it first.")
    return("FALSE")
  }
}

# Run a query to find the types of all nodes in the network
# that are connected by the relational properties. 
# returns a dataframe with 'id' and 'type'. 
get_all_node_types <- function() {
  query <- paste0('CONSTRUCT  { ?node1 <isa> ?type1 .
           ?node2 <isa> ?type2 . }  WHERE ',
                  '{GRAPH :', userName,
            ' { ?node1 ?p ?node2 .  
              ?node1 rdf:type ?type1 . 
              ?node2 rdf:type ?type2 . 
                            FILTER (?p IN (tp:followsAfter,  tp:responseTo)) }}')
  dfout <- evalQuery(rep,
                     query = query, returnType = "dataframe",
                     cleanUp = TRUE, limit = 2000)
  
  if (dfout[1] != "query failed" & length(dfout) > 1) {
    dfout <- stripOffNS(as.data.frame(dfout[["return"]]))
    node_type <- list()
    node_type$id <- last_URI_element(dfout[[1]])
    node_type$type <-  last_URI_element(dfout[[3]])
  } else {
    node_type <- "NA"
  }
  node_type
}


# display info on the selected node in a panel
render_plan_node <- function(node) {
  # First, find the panel 
  # cat("\n", "render_plan_node(node):" , " node: ", as.character(node), "\n")
  node_type <- get_node_type(node)
  # Note that this assumes only one type is being returned. That needs fixing:
  # if there are multiple types, the rest of this function should 
  # only run if the node type matches one of the panel types. 
  # node_type[1] is just a fix for the moment, to make sure things don't break. 
  node_type <- node_type[1]
  # open the panel corresponding to the node's type
  if (node_type %in% c("JournalArticle", "ResearchPaper", "Book", 
                      "BookChapter")) {
    updateTabsetPanel(inputId = "templates", selected = "Publication")
    updateSelectInput(inputId = "template", selected = "Publication")
    # reset_all_input()
    node_df <-  fetch_plan_node(node)
    fillPublicationTemplate(node, node_df)
  } else {
    alert("No type information on this node in the database.")
  }
}

# display info on the selected node in a panel
render_network_edge <- function(edge) {
 # cat("\n", "render_network_edge():" , " edge: ", as.character(edge), "\n")
  if (length(edge) == 1) {
    this_edge <- edges[unlist(edge),]
   # print(this_edge) # development only
    # find the cito object
    query <- paste0(
      'SELECT ?s  WHERE 
      { ?s a cito:Citation ;
      cito:hasCitingEntity :' , this_edge$from, ' ;' ,
      'cito:hasCitationCharacterization cito:', this_edge$label, ' ;' ,
      'cito:hasCitedEntity :', this_edge$to, ' .}'
      )
  #  print(query) # development only
    dfout <- evalQuery(rep,
                       query = query, returnType = "dataframe",
                       cleanUp = TRUE, limit = 1)
    dfout <- stripOffNS(as.data.frame(dfout[["return"]]))
    dfout[[1]] <- last_URI_element(dfout[[1]])
  # print(dfout) # development only
    # next retrieve the scheme and render it in input fields. 
    updateTabsetPanel(inputId = "templates", selected = "Citation")
    updateSelectInput(inputId = "template", selected = "Citation")
    # reset_all_input()
    node_df <-  fetch_plan_node(dfout)
    fillCitationTemplate(dfout, node_df)
  }
}

update_repo <- function(node) {
  # delete node's statements in the repo:
  planID <- paste0('<', defaultNS, node, '>') 
  node_type <- get_node_type(node)
  node_type <- node_type[1]
  deleteStatements(rep, subj = planID)
  # now save the panel content using shinyjs::click()
  if(node_type %in% c("Question", "AnswerTutee", "ActionTutee",
                      "LearningActivity", "ConceptMapAction")) {
    if (node_type == "Question") {
      click("saveQuestionButton")
    } else if (node_type == "AnswerTutee") {
      click("saveAnswerButton")
    } else if (node_type == "LearningActivity") {
      click("saveActivityButton")
    } else if (node_type == "ActionTutee") {
      click("saveTuteeActionButton")
    } else {
      click("savePatternButton")
    }
  } else {
    alert("No information on this selection in database.")
    validate("No information on this selection in database.")
  }
}

# query processing template

fetch_one_column <- function(query) {
# cat("\n", "****fetch_one_column() query: ", "\n")  # dev
# print(query) #dev
  
  dfout <- evalQuery(
    rep,
    query = query,
    returnType = "dataframe",
    cleanUp = TRUE,
    limit = 100
  )
  if (dfout[1] != "query failed" & length(dfout) > 1) {
    dfout <- stripOffNS(as.data.frame(dfout[["return"]]))
    #    print(dfout) #dev
    #  as.character(last_URI_element(dfout[[1]]))
    this_val <- last_URI_element(dfout[[1]])
    # remove the double quote 
    gsub("\"", "", this_val, fixed = TRUE)
 
  } else {
    alert("The database does not contain (sufficient) information .")
  }
}

# add name spaces for thesauri 
add_thesaurus_namespace <- function() {
  query <- 'PREFIX litrev: <http://www.learn-web.com/2023/litrev/> 
    SELECT ?scheme ?prefix ?ns WHERE {
      ?scheme a skos:ConceptScheme ; 
      skos:hasPrefix ?prefix ;
      skos:hasNameSpace ?ns . 
    }'
  # The query will return a 3-column dataframe: name, prefix, ns
  dfout <- evalQuery(rep,
                     query = query, returnType = "dataframe",
                     cleanUp = TRUE, limit = 50)
  if (dfout[1] != "query failed" & length(dfout) > 1) {
    dfout <- stripOffNS(as.data.frame(dfout[["return"]]))
    # leave the 3rd column of dfout untouched because we need the url! 
    dfout[[1]] <- last_URI_element(dfout[[1]])
    dfout[[2]] <- last_URI_element(dfout[[2]])
    # remove double quotes from prefix and ns column
    dfout[[2]] <- gsub("\"", "", dfout[[2]], fixed = TRUE)
    dfout[[3]] <- gsub("\"", "", dfout[[3]], fixed = TRUE)
  } else {
    showNotification("The plan does not contain (sufficient) information about this element.", 
                     type = "error")
  }
# cat("\n", "****add_thesaurus_namespace() dfout:", "/n")  #dev
#  print(dfout) # dev
 assign("thesauri_df", dfout, envir = globalenv()) # keep this information in glob env
  
  # add namespaces to server
  for (i in 1:length(dfout[["scheme"]])) {
   this_prefix <- sub(":", "", dfout[["prefix"]][i]) #remove the colon
   this_ns <- dfout[["ns"]][i]
   addNameSpace(rep, this_prefix, this_ns)
  }
}

lookup_prefix <- function(scheme) {
  this_index <- which(sapply(thesauri_df[["scheme"]], function(x) scheme %in% x))
  thesauri_df[["prefix"]][[this_index]]
}

lookup_namespace <- function(scheme) {
  this_index <- which(sapply(thesauri_df[["scheme"]], function(x) scheme %in% x))
  thesauri_df[["ns"]][[this_index]]
}

# is the aspect providing nouns (for object slots) or verbs (for predicate slots)?
# returns "Nouns" or "Verbs" 
NorV <- function(aspect) {
  query <- paste0(
    'PREFIX litrev: <http://www.learn-web.com/2023/litrev/>
         SELECT ?type  WHERE { litrev:', 
    aspect, 
    ' litrev:provides ?type . }'
  )
  fetch_one_column(query)
}


find_scheme_from_predicate <- function(aspect) {
  query <-
    paste0(
      'PREFIX litrev: <http://www.learn-web.com/2023/litrev/> ',
      'SELECT ?scheme { litrev:',
      aspect,
      ' litrev:hasThesaurus ?scheme }'
    )
  scheme <- fetch_one_column(query)
  scheme
}

fill_input_slots <- function(session, aspect, predicate, subject) {
  # Call the responsible input handler
  switch(
    aspect, 
    "ScholarlyWork" = input_bib(session, predicate, subject), 
    "Author" = input_author(session, predicate, subject),
    "Citation" = input_citation(session, predicate, subject),
    "LearningOutcome" = input_learning_outcome(session, predicate, subject)
    )
}

input_bib <- function(session, predicate, subject) {
  # handles bibliographic input 
  cat("\n", "****input_bib() entered with predicate = ", predicate, "subject = ", subject,  "\n")  #dev
  if (is.null(predicate)) {
    # set the predicate according to aspect selected
  
  updateSelectInput(session, "predicateInput", 
                    # choices = fill_predicate_input_slot(input$aspect),
                    choices = ScholarlyWorkPredicates,
                    selected  = NULL)
  } else {
  # subject field is list of known works
  query <- paste0('prefix litrev: <http://www.learn-web.com/2023/litrev/>
                  prefix lg: <http://www.learn-web.com/litgraph/>
                  SELECT ?work WHERE {
                 ?work a litrev:ScholarlyWork }')
  works <- fetch_one_column(query)
  updateSelectizeInput(session, "subjectInput", 
                       choices = works,
                       options = list(create = TRUE), 
                       selected = NULL)

  }

}


fill_predicate_input_slot <- function(session, aspect) {
cat("\n", "****fill_predicate_input_slot - aspect: :", aspect, "\n")  #dev
  # Call the responsible input handler
  switch(
    aspect, 
    "ScholarlyWork" = input_bib_predicate(session), 
    "Author" = input_author_predicate(session),
    "Citation" = input_citation_predicate(session),
    "LearningOutcome" = input_learning_outcome_predicate(session)
  )
}

fill_subject_input_slot <- function(session, aspect, predicateSelection) {
  cat("\n", "****fill_subject_input_slot - predicateSelection: :", predicateSelection, "\n")  #dev
  # Call the responsible input handler
  switch(
    aspect, 
    "ScholarlyWork" = input_bib_subject(session, predicateSelection), 
    "Author" = input_author_subject(session, predicateSelection),
    "Citation" = input_citation_subject(session, predicateSelection),
    "LearningOutcome" = input_learning_outcome_subject(session, predicateSelection)
  )
}

fill_object_input_slot <- function(session, aspect, predicateSelection, subjectSelection) {
  cat("\n", "****fill_object_input_slot - subjectSelection: :", subjectSelection, "\n")  #dev
  # Call the responsible input handler
  switch(
    aspect, 
    "ScholarlyWork" = input_bib_object(session, predicateSelection, subjectSelection), 
    "Author" = input_author_object(session, predicateSelection, subjectSelection),
    "Citation" = input_citation_object(session, predicateSelection, subjectSelection),
    "LearningOutcome" = input_learning_outcome_object(session, predicateSelection, subjectSelection)
  )
}

input_bib_predicate <- function(session) {
  # handles bibliographic input 
  cat("\n", "****input_bib_predicate() entered ",  "\n")  #dev

    # set the predicate according to aspect selected
    
    updateSelectInput(session, "predicateInput", 
                      choices = ScholarlyWorkPredicates,
                      selected  = NULL)
  # } else {
  #   # subject field is list of known works
  #   query <- paste0('prefix litrev: <http://www.learn-web.com/2023/litrev/>
  #                 prefix lg: <http://www.learn-web.com/litgraph/>
  #                 SELECT ?work WHERE {
  #                ?work a litrev:ScholarlyWork }')
  #   works <- fetch_one_column(query)
  #   updateSelectizeInput(session, "subjectInput", 
  #                        choices = works,
  #                        options = list(create = TRUE), 
  #                        selected = NULL)
  # }
  
}

input_bib_subject <- function(session, predicateSelection) {
# subject field is list of known works
  cat("\n", "****input_bib_subject() entered ",  "\n")  #dev
    query <- paste0('prefix litrev: <http://www.learn-web.com/2023/litrev/>
                  prefix lg: <http://www.learn-web.com/litgraph/>
                  SELECT ?work WHERE {
                 ?work a litrev:ScholarlyWork }')
    works <- fetch_one_column(query)
    updateSelectizeInput(session, "subjectInput",
                         choices = works,
                         options = list(create = TRUE),
                         selected = NULL)
}

input_bib_object <- function(session, predicateSelection, subjectSelection) {
  cat("\n", "****input_bib_object() entered with predicate: ", 
      predicateSelection, " subject: ", subjectSelection,  "\n")  #dev
  
  # The object can be a string (existing value for title, for instance, or an object)
  # We need to first find out if there are data already  on the subject
   if (node_exists(subjectSelection)) {
     query <- paste0(
    'SELECT ?p ?o  { :', 
    subjectSelection, 
    ' ?p ?o }')
     # The query will return a 2column dataframe: predicate p and object o
   #   browser()
     dfout <- evalQuery(rep,
                        query = query, returnType = "dataframe",
                        cleanUp = TRUE, limit = 1000)
       dfout <- stripOffNS(as.data.frame(dfout[["return"]]))
       # leave the 3rd column of dfout untouched because we need the url! 
       dfout[[1]] <- last_URI_element(dfout[[1]])
       dfout[[2]] <- last_URI_element(dfout[[2]])
       # remove double quotes from prefix and ns column
       # dfout[[1]] <- gsub("\"", "", dfout[[1]], fixed = TRUE)
       dfout[[2]] <- gsub("\"", "", dfout[[2]], fixed = TRUE)
     work_table <<- dfout
   }
  # Does the predicate have a value?
  # if (value_exists(paste0(':', subjectSelection), 
  #                  paste0(' bib:', predicateSelection))) {
  #   query <- paste0(
  #     'SELECT ?o { :', 
  #     subjectSelection, 
  #     ' bib:', 
  #     predicateSelection,
  #     ' ?o }' )
  #   objectVal <- fetch_one_column(query)
  # }
   }
  
  

  
  # query <- paste0('prefix litrev: <http://www.learn-web.com/2023/litrev/>
  #                 prefix lg: <http://www.learn-web.com/litgraph/>
  #                 SELECT ?work WHERE {
  #                ?work a litrev:ScholarlyWork }')
  # works <- fetch_one_column(query)
  # updateSelectizeInput(session, "subjectInput",
  #                      choices = works,
  #                      options = list(create = TRUE),
  #                      selected = NULL)







