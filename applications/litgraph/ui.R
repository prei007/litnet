# dynamic UI 
# https://mastering-shiny.org/action-dynamic.html#programming-ui

library(shiny)
library(shinyjs)
library(visNetwork)
library(allegRo)

# setup the schemes and their properties.
# These will be needed to define fields in the UI. 
# Access: 
# names(arg_schemes) returns the scheme names
# the first list:
# arg_schemes[[1]] or arg_schemes$Analogy_Inference
#The first element of the first list:
# arg_schemes[[1]][1]

# Each group of fields should correspond to a node in the
# graph. Which means cito should be reified. What about the 
# simple codes? They likely should go into SKOS concept schemes. 
# This is really to be thought about: Which object propoerties should 
# be reified, which are just atttributes? 

# Reication has definite advantages and the graphing 
# basis can be constructed with Construct Queries. 

# The interface wfor this app is probably best developed in a mixed 
# mode, hardcoding the 'tabs' but programming the fields. 

# field_scheme <<- 
#   list(
#     bib_fields = 
#       c("ID", 
#         "Title", 
#         "Creator", 
#         "Date", 
#         "bibliographicCitation"),
#     author_fields = 
#       c("ID", 
#         "family_name",
#         "first_name",
#         "middle_name"),
#     cito_fields =
#       c("ID", 
#         "hasCitingEntity",
#         "hasCitationCharacterization",
#         "hasCitedEntity",
#         ),
#     project_fields =
#       c("ID", 
#         "projectName", 
#         "etc", 
#         "etc2")
#   )

# CiTO ontology
# https://sparontologies.github.io/cito/current/cito.html

cito_scheme <<- 
  c("agreesWith",
    "cites", 
    "citesAsAuthority",
    "citesAsDataSource",
    "citesAsEvidence", 
    "citesAsMetadataDocument",
    "citesAsPotentialSolution",
    "citesAsRecommendedReading",
    "citesAsRelated",
    "citesAsSourceDocument",
    "citesForInformation",
    "compiles", 
    "confirms",
    "containsAssertionFrom",
    "corrects",
    "credits", 
    "critiques",
    "derides",
    "describes",
    "disagreesWith",
    "discusses",
    "disputes",
    "documents",
    "extends",
    "givesBackgroundTo",
    "givesSupportTo",
    "includesExcerptFrom",
    "includesQuotationFrom",
    "likes",
    "linksTo" ,
    "parodies",
    "plagiarizes",
    "qualifies",
    "refutes",
    "repliesTo",
    "retracts",
    "reviews",
    "ridicules",
    "sharesAuthorWith",
    "sharesAuthorInstitutionWith",
    "sharesFundingAgencyWith",
    "sharesJournalWith", 
    "sharesPublicationAvenueWith",
    "speculatesOn",
    "supports",
    "updates",
    "usesConclusionFrom",
    "usesDataFrom",
    "usesMethodIn"
  )

# Annotation motivation 
# https://www.w3.org/TR/annotation-vocab/#named-individuals
oa_scheme <<- 
  c("commenting", 
    "assessing", 
    "classifying",
    "describing",
    "linking",
    "questioning")

# Create the tabset 
template_tabs <- tabsetPanel(
  id = "templates",
  type = "hidden",
  tabPanel("Login", 
           p("For using the server, you need to provide credentials."),
           textInput("userName", "User Name:"), 
           passwordInput("pwd", "Password:"),
           actionButton("loginButton", "Submit")
  ),
  tabPanel("Publication",
           textInput("pubID", "Publication ID:", value = "",
                     placeholder = "<FirstAuthor><year>, e.g. Doe2012"),
           textInput("pubAuthor", "Contributors:", value = "" ,
                         placeholder = "Author(s)' ID separated by comma"),
           textAreaInput ("pubTitle", "Title:", value =""),
           textInput ("pubYear", "Year:", value =""),
           textInput("pubIdentifier", "DOI:", value="",
                     placeholder = "DOI or ISBN"),
           actionButton("savePubButton", "Save new"),
           actionButton("updatePubButton", "Update") 
  ),
  tabPanel("Author",
           textInput("authorID", "Author ID:", value = "", 
                     placeholder = "Unique identifier"),
           textInput("familyName", "Family name:", value = ""),
           textInput("givenName", "Given name:", value = ""),
           actionButton("saveAuthorButton", "Save new"), 
           actionButton("updateAuthorButton", "Update") 
  ),
  tabPanel("Citation", 
           textInput("citationID", "ID:", value = random_name()),
           textInput("citingEntity", "Citing paper ID:", value = "", 
                     placeholder = "e.g :Lemmon2000"), 
           selectInput("citoType", "Citation type:", cito_scheme) ,
           textInput ("citedEntity", "Cited paper ID:", value ="", 
                      placeholder = "e.g. :Hendrix1985"),
           actionButton("saveCitationButton", "Save new"), 
           actionButton("updateCitationButton", "Update")
  ),
  tabPanel("Annotation", 
           textInput ("annoTarget", "Annotated paper ID:", value ="", 
                      placeholder = "e.g. :Hendrix1985"),
           textAreaInput("annoBody", "Annotation body:", value = ""),
           selectInput("annoType", "Motivation:", oa_scheme, selected = "commenting") ,
           actionButton("saveCitationButton", "Save new"), 
           actionButton("updateCitationButton", "Update")
  )
)

ui <- fluidPage(
  useShinyjs(), 
  sidebarLayout(
    sidebarPanel(
      selectInput("template", "Node template:", 
                  choices = c("Login", "Publication", "Author",
                              "Citation", "Annotation")
      ),
      template_tabs
    ),
    mainPanel(
      # show user name
      textOutput("userName"),
     # show the graph.
      p(" "),
      actionButton("showMapButton",  "show map view "),
      p(" "),
      # visNetworkOutput("Map", width = "1000px", height = "600px")
    )
  )
  
)

ui
