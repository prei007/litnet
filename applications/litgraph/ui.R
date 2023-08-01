# dynamic UI 
# https://mastering-shiny.org/action-dynamic.html#programming-ui

library(shiny)
library(shinyjs)
library(visNetwork)
library(allegRo)

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
           textInput("citationID", "ID:", value = random_name('CITE-')),
           textInput("citingEntity", "Citing paper ID:", value = "", 
                     placeholder = "e.g Lemmon2000"), 
           selectInput("citoType", "Citation type:", cito_scheme) ,
           textInput ("citedEntity", "Cited paper ID:", value ="", 
                      placeholder = "e.g. Hendrix1985"),
           actionButton("saveCitationButton", "Save new"), 
           actionButton("updateCitationButton", "Update")
  ),
  tabPanel("Annotation", 
           textInput("annoID", "ID:", value = random_name('ANNO-')),
           textInput ("annoTarget", "Annotated paper ID:", value ="", 
                      placeholder = "e.g. Hendrix1985"),
           textAreaInput("annoBody", "Annotation body:", value = ""),
           selectInput("annoMotivation", "Motivation:", oa_scheme, selected = "commenting") ,
           actionButton("saveAnnotationButton", "Save new"), 
           actionButton("updateAnnotationButton", "Update")
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
      visNetworkOutput("Map", width = "1000px", height = "600px")
    )
  )
  
)

ui
