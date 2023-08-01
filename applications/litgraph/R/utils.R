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

# set all input fields to empty
# Called when the user switches a tab.  
reset_all_input <- function() {
  textFields <- c("questionID", "questionResponseTo", "questionFollowsAfter", "questionImage",
                  "questionVideo", "answerID", "answerQuestionID", "alertMsg", "activityID", 
                  "activityResponseTo", "activityFollowsAfter", "activityImage", "activityVideo",
                  "actionID", "actionActivityID", "actionAlertMsg", "patternID", "cmapSubject",
                  "cmapObject", "cmapPredicate", "patternAlertMsg")
  textAreaFields <- c("questionText", "answerText", "lowGuidanceMsg", 
                      "highGuidanceMsg", "correctionMsg", "activityDescription", 
                      "actionDescription", "actionLowGuidanceMsg", "actionHighGuidanceMsg", 
                      "actionCorrectionMsg", "patternLowGuidanceMsg", "patternHighGuidanceMsg",
                      "patternCorrectionMsg")
  
  for (i in textFields) {
  updateTextInput(session = getDefaultReactiveDomain(), inputId = i, value = NA)
  }
  
  for (j in textAreaFields) {
    updateTextAreaInput(session = getDefaultReactiveDomain(), inputId = j, value = NA)
  }
}

# Check if a node exists on server
# Returns "true" or "false"
node_exists <- function(node) {
  query <- paste0(
    'ASK { GRAPH :', 
                userName, 
    ' {:', node, ' ?p ?o}}'
    )
  evalQuery(rep,
          query = query, returnType = "list",
          limit = 1)
}

# setup graph if it does not exist already
# needs to handle the case that there are no  named graphs in repo initially. 
provide_tutor_graph <- function(user){
  # fetch current graphs 
  query <- 'select distinct ?g { graph ?g { ?s ?p ?o } }'
  dfout <- evalQuery(rep,
                        query = query, returnType = "list",
                        cleanUp = TRUE, limit = 100)
  if (dfout[1] != "query failed" & length(dfout) > 1) {
    dfout <- stripOffNS(as.data.frame(dfout[["return"]]))
    dfout[[1]] <- last_URI_element(dfout[[1]])
    # more here based on existing grpahs
    # check if user has a graph already
    if (user %in% as.list(dfout[[1]])) {
      # do nothing 
    } else {
      # create seed graph for user
    }
  } else {
    # create seed graph for user 
  }
    
  
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
          'SELECT ?p ?o  WHERE {GRAPH :', userName, 
          ' {:', node, '?p ?o}}')
        dfout <- evalQuery(rep,
                           query = query, returnType = "dataframe",
                           cleanUp = TRUE, limit = 2000)
          dfout <- stripOffNS(as.data.frame(dfout[["return"]]))
          dfout[[1]] <- last_URI_element(dfout[[1]])
          dfout[[2]] <- last_URI_element_1(dfout[[2]], ns_list) 
          dfout
  } else {
    showNotification("The plan does not contain (sufficient) information about this element.", 
                     type = "error")
    alert("Selected element does not exist yet on the server. Please create it.")
    validate("The plan does not contain sufficient information about this element.")
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
  edges <- data.frame(from = dfout[[1]], 
                      to = dfout[[3]], 
                      label = dfout[[2]])
  ## Add a column for edge width and make followsAfter links look wider 
  ## https://www.r-bloggers.com/2022/07/how-to-do-conditional-mutate-in-r/
 # edges <-  edges %>%
 #    mutate(width = case_when(label == "followsAfter" ~ 3, 
 #                             label == "responseTo"  ~ 1))
  
  
  # Display graph, and a table with node details dependent on mouse click: 
  
  visNetwork(nodes, edges, height = "1500px", width = "1500px") %>% 
    visNodes(shape = "box") %>%
    visEdges(arrows = "to") %>%
    visInteraction(hideEdgesOnDrag = TRUE)  %>%
    visOptions(highlightNearest = FALSE, nodesIdSelection = TRUE) %>%
    # # colouring of nodes by group name (i.e., node type)
    # visGroups(groupname = "ConceptMapAction", 
    #           color = list(background = "lightgreen", 
    #                        border = "black",
    #                        highlight = "red")) %>% 
    # visGroups(groupname = "Question", 
    #           color = list(background = "lightblue", 
    #                        border = "black",
    #                        highlight = "red")) %>% 
    # visGroups(groupname = "AnswerTutee", 
    #           color = list(background = "yellow", 
    #                        border = "black",
    #                        highlight = "red")) %>% 
    # visGroups(groupname = "LearningActivity", 
    #           color = list(background = "orange", 
    #                        border = "black",
    #                        highlight = "red")) %>% 
    # visGroups(groupname = "ActionTutee", 
    #           color = list(background = "lightgrey", 
    #                        border = "black",
    #                        highlight = "red")) %>% 
    visLayout(randomSeed = 123) %>%
    visPhysics(solver = "forceAtlas2Based") %>%
    visEvents(selectNode = "function(nodes) {
        Shiny.setInputValue('current_node_id', nodes);
      ;}")

}

# Function finds the node type(s) for a node
get_node_type <- function(node) {
  if (node_exists(node) == "true") {
    query <- paste0('SELECT ?type WHERE {GRAPH :', userName, 
                    ' { :', node, ' a ?type. }}' )
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
  node_type <- get_node_type(node)
  # Note that this assumes only one type is being returned. That needs fixing:
  # if there are multiple types, the rest of this function should 
  # only run if the node type matches one of the panel types. 
  # node_type[1] is just a fix for the moment, to make sure things don't break. 
  node_type <- node_type[1]
  # open the panel corresponding to the node's type
  if(node_type %in% c("Question", "AnswerTutee", "ActionTutee",
                      "LearningActivity", "ConceptMapAction")) {
    updateTabsetPanel(inputId = "templates", selected = node_type)
    updateSelectInput(inputId = "template", selected = node_type)
    reset_all_input()
  } else {
    alert("No type information on this node in the database.")
  }
  # add node content to the panel
  node_df <-  fetch_plan_node(node)
  if (node_type == "Question") {
    fillQuestionTemplate(node, node_df)
  } else if (node_type == "AnswerTutee") {
    fillAnswerTemplate(node, node_df)
  } else if (node_type == "LearningActivity") {
    fillActivityTemplate(node, node_df)
  } else if (node_type == "ActionTutee") {
    fillActionTemplate(node, node_df)
  } else {
    fillPatternTemplate(node, node_df)
  }
}

fillQuestionTemplate <-function(node, node_df) {
  # ID corresponds to node
  updateTextInput(inputId = "questionID", value = node) 
  # df has two columns: predicates (fields) and objects (field values)
  fields <- node_df[[1]]
  fvalues <- node_df[[2]]
  # Loop over fields and fill panel with hits. 
  # gsub() is needed on fields with literals because we can't save 
  # double-quoted strings to the repo. 
  # Note that the loop in this form works also for the case that a field is empty.
  for (i in 1:length(fields)) {
    if (fields[i] == "text") {
      updateTextAreaInput(inputId = "questionText", value = gsub('"', '', fvalues[i]))
    } else if (fields[i] == "responseTo") {
      updateTextInput(inputId = "questionResponseTo", value = fvalues[i])
    } else if (fields[i] == "followsAfter") {
      updateTextInput(inputId = "questionFollowsAfter", value = fvalues[i])
    } else if (fields[i] == "image") {
      updateTextInput(inputId = "questionImage", value = gsub('"', '', fvalues[i]))
    } else if (fields[i] == "video") {
      updateTextInput(inputId = "questionVideo", value = gsub('"', '', fvalues[i]))
    } else {
      next
    }
  }
}

fillAnswerTemplate <-function(node, node_df) {
  updateTextInput(inputId = "answerID", value = node) 
  fields <- node_df[[1]]
  fvalues <- node_df[[2]]
  for (i in 1:length(fields)) {
    if (fields[i] == "text") {
      updateTextAreaInput(inputId = "answerText", value = gsub('"', '', fvalues[i]))
    } else if (fields[i] == "responseTo") {
      updateTextInput(inputId = "answerQuestionID", value = fvalues[i])
    } else if (fields[i] == "isCorrect") {
      updateTextInput(inputId = "correctness", value = gsub('"', '', fvalues[i]))
    } else if (fields[i] == "alertMessage") {
      updateTextInput(inputId = "alertMsg", value = gsub('"', '', fvalues[i]))
    } else if (fields[i] == "highGuidance") {
      updateTextAreaInput(inputId = "highGuidanceMsg", value = gsub('"', '', fvalues[i]))
    } else if (fields[i] == "lowGuidance") {
      updateTextAreaInput(inputId = "lowGuidanceMsg", value = gsub('"', '', fvalues[i]))
    } else if (fields[i] == "correctionMessage") {
      updateTextAreaInput(inputId = "correctionMsg", value = gsub('"', '', fvalues[i]))
    } else {
      next
    }
  }
}

fillActivityTemplate <-function(node, node_df) {
  updateTextInput(inputId = "activityID", value = node) 
  fields <- node_df[[1]]
  fvalues <- node_df[[2]]
  for (i in 1:length(fields)) {
    if (fields[i] == "text") {
      updateTextAreaInput(inputId = "activityDescription", value = gsub('"', '', fvalues[i]))
    } else if (fields[i] == "responseTo") {
      updateTextInput(inputId = "activityResponseTo", value = fvalues[i])
    } else if (fields[i] == "followsAfter") {
      updateTextInput(inputId = "activityFollowsAfter", value = fvalues[i])
    } else if (fields[i] == "image") {
      updateTextInput(inputId = "activityImage", value = gsub('"', '', fvalues[i]))
    } else if (fields[i] == "video") {
      updateTextInput(inputId = "activityVideo", value = gsub('"', '', fvalues[i]))
    } else {
      next
    }
  }
}

fillActionTemplate <-function(node, node_df) {
  updateTextInput(inputId = "actionID", value = node) 
  fields <- node_df[[1]]
  fvalues <- node_df[[2]]
  for (i in 1:length(fields)) {
    if (fields[i] == "text") {
      updateTextAreaInput(inputId = "actionDescription", value = gsub('"', '', fvalues[i]))
    } else if (fields[i] == "responseTo") {
      updateTextInput(inputId = "actionActivityID", value = fvalues[i])
    } else if (fields[i] == "isCorrect") {
      updateTextInput(inputId = "actionCorrectness", value = gsub('"', '', fvalues[i]))
    } else if (fields[i] == "alertMessage") {
      updateTextInput(inputId = "actionAlertMsg", value = gsub('"', '', fvalues[i]))
    } else if (fields[i] == "highGuidance") {
      updateTextAreaInput(inputId = "actionHighGuidanceMsg", value = gsub('"', '', fvalues[i]))
    } else if (fields[i] == "lowGuidance") {
      updateTextAreaInput(inputId = "actionLowGuidanceMsg", value = gsub('"', '', fvalues[i]))
    } else if (fields[i] == "correctionMessage") {
      updateTextAreaInput(inputId = "actionCorrectionMsg", value = gsub('"', '', fvalues[i]))
    } else {
      next
    }
  }
}

fillPatternTemplate <-function(node, node_df) {
  updateTextInput(inputId = "patternID", value = node) 
  fields <- node_df[[1]]
  fvalues <- node_df[[2]]
  for (i in 1:length(fields)) {
    if (fields[i] == "cmapSubject") {
      updateTextInput(inputId = "cmapSubject", value = fvalues[i])
    } else if (fields[i] == "cmapPredicate") {
      updateTextInput(inputId = "cmapPredicate", value = fvalues[i])
    } else if (fields[i] == "cmapObject") {
      updateTextInput(inputId = "cmapObject", value = fvalues[i])
    } else if (fields[i] == "isCorrect") {
      updateTextInput(inputId = "patternCorrectness", value = gsub('"', '', fvalues[i]))
    } else if (fields[i] == "alertMessage") {
      updateTextInput(inputId = "patternAlertMsg", value = gsub('"', '', fvalues[i]))
    } else if (fields[i] == "highGuidance") {
      updateTextAreaInput(inputId = "patternHighGuidanceMsg", value = gsub('"', '', fvalues[i]))
    } else if (fields[i] == "lowGuidance") {
      updateTextAreaInput(inputId = "patternLowGuidanceMsg", value = gsub('"', '', fvalues[i]))
    } else if (fields[i] == "correctionMessage") {
      updateTextAreaInput(inputId = "patternCorrectionMsg", value = gsub('"', '', fvalues[i]))
    } else {
      next
    }
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





