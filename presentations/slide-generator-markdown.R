#' ---
#' title: "Slides generated from a database query"
#' author: "Peter Reimann"
#' date: '2022-08-01'
#' output: slidy_presentation
#' always_allow_html: true
#' ---
#' 
#+ echo=FALSE
library(kableExtra)
library(allegRo)
library(tidyverse)
library(urltools)
#'
#+ echo=FALSE
stripOffNS <- function(df) {
  df <- lapply(df, function(x) {gsub(">", "", x)})
  df <- lapply(df, function(x) {gsub("<", "", x)})
  df
}
#'
#+ echo=FALSE
### Making contact with AG 
url = "http://learn-web.com"
user = "anonymous"
password = ""
service = service(url, user, password, testConnection = FALSE)
cat = catalog(service, "coolfutures")
rep = repository(cat, "compumod")
#'
#+ echo=FALSE
### First query
query = 'SELECT ?s ?d WHERE {
  {?s a :DesignApproach . 
  ?s dc:description ?d}
  UNION
  {?s a :ResearchApproach . 
  ?s dc:description ?d}
  }
ORDER BY ?s'
#' 
#+ echo=FALSE
# clean the dataframe
workTable <- evalQuery(rep,
                       query = query, returnType = "dataframe",
                       cleanUp = TRUE, convert = TRUE) 
wTab <- as.data.frame(workTable[1])
colnames(wTab) <- c("approach", "gist")
wTab <- stripOffNS(wTab)
wTab$approach <- urltools::fragment(wTab$approach)
wTab <- as.tibble(wTab)
#' 
#' # Approaches 
#'
#+ echo=FALSE, results='asis'
wTab %>% 
  kbl() %>%
  kable_styling()
#'
#' # Cars too
#' 
#+ echo=FALSE, results='asis'
dt <- mtcars[1:5, 1:6]
dt %>%
  kbl() %>%
  kable_styling()
#'

