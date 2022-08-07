################################################
## Creating slides from queries using officer ##
################################################

# This is a first version, with much left out. In particular, this version does not 
# allow flexible sequencing of slides. A better version would use presentation-oriented
# information (a graph, for instance) and then built the slides accordingly. 
#
# An important distinction is between a single study and summary information, also not 
# taken into account here. This is a template for summary information only.  

library(allegRo)
library(tidyverse)
library(urltools)
library(officer)
# library(magrittr)

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
addNameSpace(repo = rep, prefix= "semsur", nsURI = "http://purl.org/SemSur/")

# Some longer text we need in the slides later:

intro_text = "This presentation is automatically generated from the database with the literature corpus. It does not look pretty as is, but you can improve the presentation to your heart's content."

lore_ipsum = "Lorem ipsum dolor sit amet, consectetur adipiscing elit. Nam si amitti vita beata potest, beata esse non potest. Si de re disceptari oportet, nulla mihi tecum, Cato, potest esse dissensio. Ergo id est convenienter naturae vivere, a natura discedere."

###  Adding slides

# read empty ppt as template
pres <- read_pptx("blank-wide.pptx")
# show slide layouts available: 
layout_summary(pres)

# add title slide
pres <- add_slide(pres, layout = "Title Slide", master = "Office Theme")
# to see the available properties use:
# s_props <- layout_properties(pres, layout="Title Slide")
pres <- ph_with(pres, value = "Research and design approaches", location = ph_location_type(type = "ctrTitle")) 
pres <- ph_with(pres, value = intro_text, location = ph_location_type(type = "subTitle"))

## Add a slide with a picture

pres <- add_slide(pres, layout = "Picture with Caption", master = "Office Theme")
# s_props <- layout_properties(pres, layout="Picture with Caption")
pres <- ph_with(pres, value = "Some of the studies are about simulating a lake", location = ph_location_type(type = "title"))
pres <- ph_with(pres, value = lore_ipsum, location = ph_location_type(type = "body"))
pres <- ph_with(pres, external_img("lake.jpeg"),
                location = ph_location_type(type = "pic"), use_loc_size = TRUE )

### add slide with list of problems

query = 'SELECT ?s ?d WHERE 
  {?s a ?problemType . 
   ?problemType rdfs:subClassOf semsur:Problem . 
  ?s dc:description ?d}
ORDER BY ?s'

# clean the dataframe
workTable <- evalQuery(rep,
                       query = query, returnType = "dataframe",
                       cleanUp = TRUE, convert = TRUE) 
wTab <- as.data.frame(workTable[1])
colnames(wTab) <- c("problem", "gist")
wTab <- stripOffNS(wTab)
wTab$problem <- urltools::fragment(wTab$problem)
wTab <- as.tibble(wTab)

pres <- add_slide(pres, layout = "Title and Content", master = "Office Theme")
pres <- ph_with(pres, value = "Problems stated in the literature", location = ph_location_type(type = "title"))
pres <- ph_with(pres, value = wTab, location = ph_location_type(type = "body"))


###  add slide with list of approaches

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
pres <- ph_with(pres, value = wTab, location = ph_location_type(type = "body"))


### A slide with a plot

# query for publications by year


query = 'SELECT ?year (COUNT(?year) AS ?yearTotal)
        WHERE
	{ 
    ?work dc:date ?year .
    }
GROUP BY ?year 
ORDER BY DESC(?year) '

yearTable <- evalQuery(rep,
                       query = query, returnType = "dataframe",
                       cleanUp = TRUE, limit = 100)
prtTable <- as.data.frame(yearTable[1])
colnames(prtTable) <- c("year", "count")

# Nunmbers in graphical form:
df <- as.data.frame(yearTable[1])
plot_object <- ggplot(df,aes(return.year, return.yearTotal) ) +
  geom_col() + 
  xlab("Year") +
  ylab ("Count").

#Add the slide
pres <- add_slide(pres, layout = "Title and Content")
pres <- ph_with(x = pres, value = plot_object, 
               location = ph_location_type("body") )
pres<- ph_with(x = pres, "Publications by Year", 
               location = ph_location_type(type = "title") )


####### Creating the ppt means printing the object to a file ########
print(pres, target = "ApproachesPresentation.pptx")

# removing slides 
remove_slide(pres, index=5)
remove_slide(pres, index=4)
remove_slide(pres, index=3)
remove_slide(pres, index=2)
remove_slide(pres, index=1)
