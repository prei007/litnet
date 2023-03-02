library(shiny)
library(allegRo)
library(tidyverse)
library(knitr)
library(urltools)


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


# get the data 
# This needs to be moved into the UI with values for 
# the studies passed into as parameters 

query = "SELECT ?paper ?property ?value 
WHERE {
  ?paper a dc:BibliographicResource . 
  ?value a skos:Concept . 
  ?paper ?property ?value .
  FILTER (?paper IN (:Dickes2016, :Dickes2019)) .} "

# Run query and store in dfr
dfr <- evalQuery(rep,
                 query = query, returnType = "dataframe",
                 cleanUp = TRUE, limit = 1000)

# remove the namespace stuff
studies <- as.data.frame(dfr[1])
colnames(studies) <- c('paper', 'property', 'pvalue')
studies <- stripOffNS(studies)
studies$paper <- fragment(studies$paper)
studies$property <- fragment(studies$property)
studies$pvalue <- fragment(studies$pvalue)



# Transpose into a tibble with papers as columns 
# using values_fn argument to suppress warning messsage. 
# We do want multiple values in cells. 
studies <- as_tibble(studies)
studies_long <- studies %>%
  pivot_wider(names_from = paper, values_from = pvalue, values_fn="list")

# For pretty printing, use knitr, but we need to see how to do this in Shiny.
print(kable(studies_long))




