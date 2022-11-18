library(shiny)
library(shinydashboard)
library(shinydashboardPlus)
library(allegRo)
library(urltools)
library(dplyr)

stripOffNS <- function(df) {
  df <- lapply(df, function(x) {gsub(">", "", x)})
  df <- lapply(df, function(x) {gsub("<", "", x)})
  df
}

url = "http://learn-web.com"
user = "anonymous"
password = ""
service = service(url, user, password, testConnection = TRUE)

# connect to compumod repo. 

cat = catalog(service, "coolfutures")
rep = repository(cat, "compumod")
addNameSpace(repo = rep, prefix= "lrmi", nsURI = "http://purl.org/dcx/lrmi-terms/")
addNameSpace(repo = rep, prefix= "arg", nsURI = "http://www.coolfutures.net/rdf/Design_Conjectures/toulminarg#")


query = paste0('
                  select ?Science ?Domain ?Outcome ?Level ?Pedagogy
                 { ?id a dc:BibliographicResource . 
                  OPTIONAL {?id edtech:science ?Science}  
                  OPTIONAL {?id edtech:learningDomain ?Domain} 
                  OPTIONAL {?id edtech:outcome ?Outcome } 
                  OPTIONAL {?id edtech:educationalLevel ?Level} 
                  OPTIONAL {?id edtech:pedagogy ?Pedagogy }
                  FILTER (?id IN (:Dickes2019)) }'
)

dfout <- evalQuery(rep,
                   query = query, returnType = "dataframe",
                   cleanUp = TRUE, limit = 1000)
dfout <- stripOffNS(dfout[["return"]])
dfout[[1]] <- fragment(dfout[[1]])
dfout[[2]] <- fragment(dfout[[2]])
dfout[[3]] <- fragment(dfout[[3]])
dfout[[4]] <- fragment(dfout[[4]])
dfout[[5]] <- fragment(dfout[[5]])

elem5 <- unique(dfout[[5]])

paste0("<p>", "<strong>", "Pedagogy: ", "</strong>" , unique(dfout[[5]]) , "</p>", collapse = ", ")

paste0("<p>", "<strong>", "Science: ", "</strong> </p>" , "<p>" , 
       paste0(unique(dfout[[1]]), collapse = ', '), "</p>", 
       "<p>", "<strong>", "Pedagogy: ", "</strong> </p>" , 
        "<p>" , paste0(unique(dfout[[5]]), collapse = ', '), "</p>"
)


paste0("<p>", "<strong>", "Science: ", "</strong>", unique(dfout[[1]]), "</p>" , 
       "<p>", "<strong>", "Learning area: ", "</strong>", unique(dfout[[2]]), "</p>", 
       "<p>", "<strong>", "Targeted learning outcome: ", "</strong>" , unique(dfout[[3]]), "</p>", 
       "<p>", "<strong>", "Eduction level: ", "</strong>" , unique(dfout[[4]]), "</p>",
       "<p>", "<strong>", "Pedagogy: ", "</strong>" , unique(dfout[[5]]), "</p>",
      collapse = ","
)



  query = paste0('select ?Tech
            { ?id a dc:BibliographicResource . 
            OPTIONAL {?id edtech:technology ?Tech}  
            FILTER (?id IN (:Dickes2019))}' )
  dfout <- evalQuery(rep,
                     query = query, returnType = "dataframe",
                     cleanUp = TRUE, limit = 1000)
  dfout <- stripOffNS(dfout[["return"]])
  dfout[[1]] <- fragment(dfout[[1]])
  paste0("<p>", "<strong>", "Technologies: ", "</strong> </p>" , 
         "<p>" , paste0(unique(dfout[[1]]), collapse = ', '), "</p>")

