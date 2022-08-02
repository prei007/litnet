################################################
## Creating slides from queries using officer ##
################################################

library(allegRo)
library(tidyverse)
library(urltools)
library(officer)
library((magrittr))

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

### Title slide 

pres <- read_pptx("blank-wide.pptx")


# add title slide
pres <- add_slide(pres, layout = "Title Only", master = "Office Theme")

# add Title text
pres <- ph_with(pres, value = "Research and design approaches", location = ph_location_type(type = "title")) 
pres <- ph_with(pres, value = "Peter Reimann, The University of Sydney", location = ph_location_type(type = "subTitle"))

###  add second slide with list of approaches

query = 'SELECT ?s ?d WHERE {
  {?s a :DesignApproach . 
  ?s dc:description ?d}
  UNION
  {?s a :ResearchApproach . 
  ?s dc:description ?d}
  }
ORDER BY ?s'

# clean the dataframe

workTable <- evalQuery(rep,
                       query = query, returnType = "dataframe",
                       cleanUp = TRUE, convert = TRUE) 
wTab <- as.data.frame(workTable[1])
colnames(wTab) <- c("approach", "gist")
wTab <- stripOffNS(wTab)
wTab$approach <- urltools::fragment(wTab$approach)
wTab <- as.tibble(wTab)

pres <- add_slide(pres, layout = "Title and Content", master = "Office Theme")
pres <- ph_with(pres, value = "Approaches found in the literature", location = ph_location_type(type = "title"))
# pres <- ph_with(pres, value = as.vector(wTab[,1]), location = ph_location_type(type = "body"))
pres <- ph_with(pres, value = wTab, location = ph_location_type(type = "body"))

##  add a third slide with a table 

# create sample data frame
frame <- data.frame(a = 1:10, b = 11:20, c = 21:30)

# create slide to hold table
pres <- add_slide(pres, layout = "Title and Content", master = "Office Theme")
pres <- ph_with(pres, value = "Table Example", location = ph_location_type(type = "title"))

# add data frame to PowerPoint slide
pres <- ph_with(pres, value = frame, location = ph_location_type(type = "body"))

## Add a slide with a picture

pres <- add_slide(pres)
pres <- ph_with(pres, external_img("lake.jpeg", width = 5, height = 4),
                location = ph_location_type(type = "body"), use_loc_size = FALSE )

# Creating the ppt means printing the object to a file 
print(pres, target = "ApproachesPresentation.pptx")

