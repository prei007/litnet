##################################
## Creating slides from queries ##
##################################

library(kableExtra)
library(allegRo)
library(tidyverse)
# library(knitr)
library(urltools)

### Functions
stripOffNS <- function(df) {
  df <- lapply(df, function(x) {gsub(">", "", x)})
  df <- lapply(df, function(x) {gsub("<", "", x)})
  df
}

### Making contact with AG 

url = "http://learn-web.com"
user = "anonymous"
password = ""
service = service(url, user, password, testConnection = TRUE)
cat = catalog(service, "coolfutures")
rep = repository(cat, "compumod")

# Optional: Add namespaces
# addNameSpace(repo = rep, prefix= "lrmi", nsURI = "http://purl.org/dcx/lrmi-terms/")

### First query

query = 'SELECT ?s ?d WHERE {
  {?s a :DesignApproach . 
  ?s dc:description ?d}
  UNION
  {?s a :ResearchApproach . 
  ?s dc:description ?d}
  }
ORDER BY ?s'

workTable <- evalQuery(rep,
                       query = query, returnType = "dataframe",
                       cleanUp = TRUE, convert = TRUE) 

wTab <- as.data.frame(workTable[1])
# Rename the columns 
colnames(wTab) <- c("work", "category", "code")

wTab <- stripOffNS(wTab)
wTab$work <- fragment(wTab$work)
wTab$category <- fragment(wTab$category)
wTab$code <- fragment(wTab$code)

wTabWide %>%
  kbl(caption = "Categories by works") %>%
  kable_paper(bootstrap_options = c("striped", "condensed"), full_width = T)

wTabWide 