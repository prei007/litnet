### An app for interacting with the repository compumod on AllegroGraph ###

library(shiny)
library(allegRo)
library(urltools)
library(visNetwork)

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

# get the coding schemes

query = "SELECT ?scheme WHERE {?scheme a skos:ConceptScheme} "
df <- evalQuery(rep,
                     query = query, returnType = "dataframe",
                     cleanUp = TRUE, limit = 1000)
schemes <- as.data.frame(df[1])
colnames(schemes) <- "thesauri"

# Stripp off URIs
schemes <- stripOffNS(schemes)
schemes$thesauri <- fragment(schemes$thesauri)


########### Shiny code ########### 

# Define UI for application 
ui <- fluidPage(
  
  # Application title
  titlePanel("Bibliography: Computer simulations in environmental education."),
  
  # Short intro
  tags$p("(c) Peter Reimann"), 
  tags$p("This database contains the corpus of studies for a literature 
  review on the use of models and
computational models in Environmenal Education (EE), including 
Climate Change Education (CCE). To cite these data, please use
http://dx.doi.org/10.13140/RG.2.2.18597.42721. 
To cite or quote the coding categories, please use 
http://dx.doi.org/10.13140/RG.2.2.35374.64329.
Note that error messages mean that no data have been found, so nothing to worry about."), 
  
  #Interface elements
  navlistPanel(
    id = "tabset", 
    tabPanel("Thesauri", "A number of thesauri are used 
             as the controlled vocabulary for coding the studies. 
             Select a thesaurus to see its codes.", 
             selectInput(inputId = "dataset", 
                         label = "",
                         choices = schemes$thesauri
             ),
             tableOutput("ThesauriTable")
    ), 
    tabPanel("Codes", "All codes in alphabetic order.", 
      tableOutput("CodesTable")
    ),
    tabPanel("ByCode", "Type a code name to see the works coded by it." , 
             textInput(inputId = "Code" , label = "", 
                       value = "ABM" , 
                       placeholder = "" ),
             actionButton("Query1", "Run Query"), 
             tableOutput("CodeTable"), 
             tags$p("More general concepts are:"), 
             tableOutput ("MoreGeneralTable"), 
             tags$p("More specific concepts are:"), 
             tableOutput ("MoreSpecificTable")
    ), 
    tabPanel("Year", "Select or type a year (between 2011 and 2022) to see studies published in it.", 
             numericInput(inputId= "yearStart", 
                          label = "",
                          value = 2011, 
                          min = 2011,
                          max = 2021, 
                          step = 1
             ),
             tableOutput("YearTable")
    ),
    tabPanel("Pedagogy", "Works by pedagogy.", 
             tableOutput("PedagogyTable")
    ),
    tabPanel("ModelPedagogy", "Pedagogies for simulations.", 
             tableOutput("ModPedTable")
    ), 
    tabPanel("ArgumentGraph", "Visualising arguments (test):
             The database can also contain arguments formulated by the reviewer(s). 
             The take the form of a Toulmin Argument and can be visualised as a graph. 
             Scroll to zoom in/out; drag whole graph; click on a node for details.", 
             visNetworkOutput("ArgGraph"),  
             tags$p("On click: details on the selected node (if available):"), 
             tableOutput("ArgTable"))))
  


# Define server logic 
server <- function(input, output, session) {
  # Codes by Thesaurus
  output$ThesauriTable <- renderTable({
    query = paste0('SELECT ?concept ?label
    WHERE { ?concept skos:inScheme :', 
                   input$dataset, '.', 
      ' OPTIONAL {?concept skos:prefLabel ?label }}'
                   , ' ORDER BY ?concept') 
    dfout <- evalQuery(rep,
                       query = query, returnType = "dataframe",
                       cleanUp = TRUE, limit = 1000)
    dfout <- stripOffNS(dfout[["return"]])
    dfout[[1]] <- fragment(dfout[[1]])
    dfout
})
 # Show all codes
  output$CodesTable <- renderTable({
    query = 'SELECT ?concept ?label ?thesaurus WHERE {
               ?thesaurus a skos:ConceptScheme . 
               ?concept skos:inScheme ?thesaurus . 
               ?concept skos:prefLabel ?label
                }
              ORDER BY ?concept'
    dfout <- evalQuery(rep,
                       query = query, returnType = "dataframe",
                       cleanUp = TRUE, limit = 1000)
    dfout <- stripOffNS(dfout[["return"]])
    dfout[[1]] <- fragment(dfout[[1]])
    dfout[[3]] <- fragment(dfout[[3]])
    dfout
  })
  
  # Show Works by code
  # We need to make the query reactive on the user pressing the 
  #button so that the user 
  # does not see error messages all over the place. To achieve
  # this we use eventReactive() to encapsulate the input (the code label). 
  # Note the *query1()* in the call to the database. 
  
  query1 <- eventReactive(input$Query1, {
    paste0(
      'SELECT ?work ?wiki WHERE {
        ?work a dc:BibliographicResource . ' , 
      '?work ?p :' , input$Code, '.', 
      '?work :wikiLink ?wiki .  } 
      ORDER BY ?work' )
  })
  
  output$CodeTable <- renderTable({
    dfout <- evalQuery(rep,
                       query = query1(), returnType = "dataframe",
                       cleanUp = TRUE, limit = 1000)
    dfout <- stripOffNS(dfout[["return"]])
    dfout[[1]] <- fragment(dfout[[1]])
    dfout
  })
  
  # Show more general codes  -- also dependent on button click
  query2 <- eventReactive(input$Query1, {
    paste0(
        'SELECT ?concept WHERE {:',
        input$Code,
        ' skos:broader+ ?concept.}
            ORDER BY ?concept') 
  })
  
  output$MoreGeneralTable <- renderTable({
    dfout <- evalQuery(rep,
                       query = query2(), returnType = "dataframe",
                       cleanUp = TRUE, limit = 1000)
    dfout <- stripOffNS(dfout[["return"]])
    dfout[[1]] <- fragment(dfout[[1]])
    dfout
  })
  
  # Show more specific codes -- also dependent on button click
  query3 <- eventReactive(input$Query1, {
    paste0(
      'SELECT ?concept WHERE {:',
      input$Code,
      ' skos:narrower+ ?concept.}
            ORDER BY ?concept') 
  })
  
  output$MoreSpecificTable <- renderTable({
    dfout <- evalQuery(rep,
                       query = query3(), returnType = "dataframe",
                       cleanUp = TRUE, limit = 1000)
    dfout <- stripOffNS(dfout[["return"]])
    dfout[[1]] <- fragment(dfout[[1]])
    dfout
  })
  
  # works by year  
    output$YearTable <- renderTable({
      query = paste0(' SELECT ?work ?wiki
	    { ?work a dc:BibliographicResource . 
	      ?work :wikiLink ?wiki . 
        ?work dc:date "', input$yearStart, 
                     '"} ', 'ORDER BY ?work') 
      dfout <- evalQuery(rep,
                         query = query, returnType = "dataframe",
                         cleanUp = TRUE, limit = 1000)
      dfout <- stripOffNS(dfout[["return"]])
      dfout[[1]] <- fragment(dfout[[1]])
      dfout
    })
    
  # Works by pedagogy
    output$PedagogyTable <- renderTable({
      query = 'SELECT ?pedagogy ?work ?wiki WHERE {
                  ?work a dc:BibliographicResource.
                  ?pedagogy a skos:Concept . 
                  ?work :pedagogy ?pedagogy . 
                  ?work :wikiLink ?wiki }
              ORDER BY ?pedagogy'
      dfout <- evalQuery(rep,
                         query = query, returnType = "dataframe",
                         cleanUp = TRUE, limit = 1000)
      dfout <- stripOffNS(dfout[["return"]])
      dfout[[1]] <- fragment(dfout[[1]])  # work label
      dfout[[2]] <- fragment(dfout[[2]])  # pedaogy 
      dfout
    })
    
  # Works by model pedagogy
    output$ModPedTable <- renderTable({
      query = 'SELECT ?pedagogy ?work ?wiki WHERE {
                    :ModelBasedLearning skos:narrower* ?pedagogy . 
                    ?work ?p ?pedagogy . 
                    ?work :wikiLink ?wiki}
              ORDER BY ?pedagogy'
      dfout <- evalQuery(rep,
                         query = query, returnType = "dataframe",
                         cleanUp = TRUE, limit = 1000)
      dfout <- stripOffNS(dfout[["return"]]) # query results are in the list "return" of the object
      dfout[[1]] <- fragment(dfout[[1]])  # work label
      dfout[[2]] <- fragment(dfout[[2]])  # pedagogy 
      dfout
    })
    
 # Create an argument graph 
    
    output$ArgGraph <- renderVisNetwork({
    # Fetch data round 1: properties from argument nodes
      query = 'SELECT ?s ?p ?o WHERE {
                {?s ?p ?o. 
                 ?s a arg:ToulminArgument
                MINUS {?s rdf:type ?o}}
                MINUS {?s arg:text ?o} }'
      dfout <- evalQuery(rep,
                         query = query, returnType = "dataframe",
                         cleanUp = TRUE, limit = 1000)
      dfout <- stripOffNS(dfout[["return"]])
      dfout[[1]] <- fragment(dfout[[1]])
      dfout[[2]] <- fragment(dfout[[2]])
      dfout[[3]] <- fragment(dfout[[3]])
      
    # create nodes and edges round 1: property links from 
      arguments <- dfout
      # Nodes IDs need to be unique():
      nodes <- as.data.frame(c(unique(arguments[["s"]]), unique(arguments[["o"]])))
      nodes[2] = nodes[1] # copy ids as labels 
      colnames(nodes) = c("id", "label")
      # edges can be taken directly from the table: 
      edges <- data.frame(from = arguments[["s"]], 
                          to = arguments[["o"]], 
                          label = arguments[["p"]])
      
      # Fetch data round 2: dc:relation links from all argument properties
      query = 'SELECT DISTINCT ?o ("evidence" as ?rel) ?d  WHERE {
                   ?s a arg:ToulminArgument .
                   ?s ?p ?o. 
                   ?o dc:relation ?d. }'
      dfout <- evalQuery(rep,
                         query = query, returnType = "dataframe",
                         cleanUp = TRUE, limit = 1000)
      dfout <- stripOffNS(dfout[["return"]]) 
      dfout[[1]] <- fragment(dfout[[1]])
      dfout[[2]] <- gsub("\"", "", dfout[[2]]) # remove the quotes
      dfout[[3]] <- fragment(dfout[[3]])
      
      # add new nodes
      arguments <- dfout
      newNodes <- data.frame(unique(arguments[3]), unique(arguments[3])) # id and label columns
      names(newNodes) <- c("id", "label")
      nodes <- rbind(nodes, newNodes)
      nodes <- unique(nodes) # nodes ids must also be unique overall. 
      
      
      #add new edges
      newEdges <- data.frame(from = arguments[["o"]], 
                             to = arguments[["d"]], 
                             label = arguments[["rel"]])
      edges <- rbind(edges, newEdges)
      
# Render the graph 
      visNetwork(nodes, edges, width = "100%") %>% 
        visNodes(shape = "box", 
                 color = list(background = "lightblue", 
                              border = "darkblue",
                              highlight = "yellow")) %>%
        visEdges(arrows = "to") %>%
        visOptions(nodesIdSelection = TRUE, highlightNearest = TRUE) %>%
        visLayout(randomSeed = 123) %>% 
        visEvents(selectNode = "function(nodes) {
        Shiny.setInputValue('current_node_id', nodes);
      ;}")
})
# Show information for selected node
    output$ArgTable <- renderTable({
      query <- paste0('SELECT ?p ?o {:', 
                      input$current_node_id$node, 
                      ' ?p ?o. FILTER (?p NOT IN (rdf:type , dc:relation)) }')
      dfout <- evalQuery(rep,
                         query = query, returnType = "dataframe",
                         cleanUp = TRUE, limit = 1000)
      dfout <- stripOffNS(dfout[["return"]])
      dfout[[1]] <- fragment(dfout[[1]])
      dfout
    })

}  # EOF server    

# Run the application 
shinyApp(ui = ui, server = server)
