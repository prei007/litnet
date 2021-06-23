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


## 21 June

Queries for mapping and search all now work with the "multifields" for authors and keywords. 

## 22 June

### ASSA Lit review references

1. Created a shorter version ASSA_CCE_Refs
2. Must be saved as .csv because otherwise odd format for year in onto/open refine
3. add ID column based on Authors column with `value.split(" ").get(0) + cells['Year'].value`
4. to lower case Author and Index Keywords
5. RDF mapping as per [here](https://www.evernote.com/shard/s55/nl/6154799/fa786e5d-6c02-483d-bf9b-b09762f54247?title=OntoRefine%20%7C%20GraphDB%20Workbench)
6. Export as RDF and import ttl into graphdb
6. Base IRI  http://coolfutures.net/rdf/2021/edtech#
6. PREFIX edtech:  <http://coolfutures.net/rdf/2021/edtech#>
6. Add SKOS concepts and run basic queries:
7. Narrower relation
8. Query to fill index keywords with author keywords in case those are missing. 

