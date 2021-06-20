# Journal

This is about publication on technology and environmental (etc.) education


## 19 June 2021

1. Searched for some references that should be sure hits on Scopus.
2. Extended the skos concepts in concepts.ttl with tech concepts. 
3. Transferred the references to RDF triples, using OntoRefine. Dublin core, mostly. 
4. Wrote first SPARQL INSERT queries that map from key words to the skos concepts. 

There are also limitations to scopus. Not all is indexed. Need to check with google and others.

## 20 June

Data preparation (open/onto refine): 

* all keywords to lower case

* Since not all entries have a DOI, I added a new column 'ID' with the GREL expression  `value.split(" ").get(0) + cells['Year'].value `. 
starting from column operations > add column based on the Auhtors column. 



