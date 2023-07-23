# dynamic UI 
# https://mastering-shiny.org/action-dynamic.html#programming-ui


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

field_scheme <<- 
  list(
    bib_fields = 
      c("ID", 
        "Title", 
        "Creator", 
        "Date", 
        "bibliographicCitation"),
    author_fields = 
      c("ID", 
        "family_name",
        "first_name",
        "middle_name"),
    coding_fields =
      c("ID", 
        "hasCitingEntity",
        "hasKnowledgeAssertion_Premise",
        "hasConclusion"),
    project_fields =
      c("ID", 
        "projectName", 
        "etc", 
        "etc2")
  )



defaultNS <<- 'http://www.example.net/'


ui <- fluidPage(
  sidebarLayout(
    sidebarPanel(
      selectInput("scheme", "Select a scheme", names(field_scheme)),
      uiOutput("fillScheme"),
    ),
    mainPanel(
      textOutput("placeholder")
    )
  )
)

ui
