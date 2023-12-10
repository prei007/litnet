# Develop the data structure for a nested menu

library(allegRo)

# query Thesaurus

# Example case: TechnologyThesaurus because it is big and multi-level. 

query <- 'SELECT ?concept2 ?concept1 ?concept1Label WHERE {
  ?concept1 skos:inScheme tech:TechnologyThesaurus. 
  ?concept1 skos:broader ?concept2. 
  ?concept1 skos:prefLabel ?concept1Label 
  }
ORDER BY ?concept2'

dfout <- evalQuery(rep, query = query, returnType = "dataframe") 

if (dfout[1] != "query failed" & length(dfout) > 1) {
  dfout <- stripOffNS(as.data.frame(dfout[["return"]]))
  dfout[[1]] <- last_URI_element(dfout[[1]])
  dfout[[2]] <- last_URI_element(dfout[[2]])
  dfout[[3]] <- last_URI_element(dfout[[3]]) 
  dfout
} else {
  showNotification("The plan does not contain (sufficient) information about this element.", 
                   type = "error")
}

# each concept in the first column is a hierarchy level in the menu. 

# predicates[predicates$label == input$predicateInput, 'domain']
techDF <- data.frame('group' = dfout$concept2, 'concept' = dfout$concept1, 'label' = dfout$concept1Label)

techDF[techDF$group == "ImmersiveTech", "concept"]
techDF[techDF$group == "SocialMedia", "concept"]

# For each group, we construct a list of lists:
# I think we need to start with the smallest lists:
c1 = list(name = "XR")
c2 = list(name = "AR")
c3 = list(name = "Blog")
c4 = list(name = "Facebook")

ImmersiveTech = list(name = "Immersive technology", items = list(c1, c2))
SocialMedia = list(name = "Social media", items = list(c3, c4))

# ImmersiveTech = list(name = "Immersive technology", items = techDF[techDF$group == "ImmersiveTech", "concept"])
# SocialMedia = list(name = "Social media", items = techDF[techDF$group == "SocialMedia", "concept"])

TechMenu = list(
  techImmersive = list(name = "immersive tech", items = ImmersiveTech), 
  techSocial = list(name = "social tech", items = SocialMedia))
