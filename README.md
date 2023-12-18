# LitGraph - supporting the 'living review'

LitGraph is a method for literature review method that builds on RDF and SKOS, and on software: AllegroGraph, R, and Shiny. It supports the coding step in a literature review: identifying the properties of and the relations between studies (papers). For this, there is a Shiny application with a point and click interface to enter study descriptions and codes. The app provides some basic visualisation capabilities, in addition to facilitating the entry of RDF data.

## Purpose

The main use case is the preparation of an analytic literature review or research synthesis (used synchronously). The review takes the form of attaching codes to publications (or parts of publications, i.e., micropublications) and creates relations between (parts of) publications. An example for a code is the age group of the human participants in a learning research study. An example for a relation between two studies is that S1 cites S2. Another example for a relational annotation is that S1 contains a claim C.

Reviewers can enter codes and relations by editing files and/or by using the Shiny app. A particular feature of the app is that it can import any coding scheme that is written in the SKOS notation with very little manual adjustments needed.

The main purpose of the Shiny app is to facilitate entry of RDF data. It offers values for codes in dropdown boxes and fills in namespaces needed for fully formatted RDF automatically. This makes it comparatively easy to create--or contribute to--a Knowledge Graph that contains the bibliographic data and the thematic codes and relations for the tables and graphs that appear in published literature reviews.

As an example, the app is set up with bibliographic data and SKOS concept schemes from a review on computational models and simulations in environmental education. A few of the domain-specific concept schemes are general in nature: those for education levels, areas of research codes, and Bloom's Taxonomy. 

## RDF data representation

### Ontologies

Re-used ontologies: 

- [bibo](http://purl.org/ontology/bibo/) for bibiographic detail
- [dcterms](http://purl.org/dc/terms/) for general meta-data
- [foaf](http://xmlns.com/foaf/0.1/) for people and organisations
- [skos](http://www.w3.org/2004/02/skos/core#) for controlled vocabularies (thematic codes) and nomenclature
- [prov](http://www.w3.org/ns/prov#) for provenance
- [mp](http://purl.org/mp/) for micropublications (claims)

Method- and application specific: 

-  [litrev](http://www-learnweb.com/2023/litrev/) for application-specific predicates

### SKOS concept schemes: 
The SKOS Simple Knowledge Organization System is a [W3C standard](https://www.w3.org/2004/02/skos/) for the description of thesauri and taxonomies. It is used here for describing the vocabulary for coding research works along various dimenions. Each concept scheme has it's own namespace: 

- [sci](http://www.learn-web.com/thesauri/sciences/) for science domains 
- [bloom](http://www.learn-web.com/thesauri/bloom_outcomes/) Bloom's Taxonomy of learning outcomes
- [edlevel](http://www.learn-web.com/thesauri/educlevels/) for education level (e.g., highschool)
- [lo](http://www.learn-web.com/thesauri/learning_outcomes/) for learning outcomes specific to environmental education 
- [rm](http://www.learn-web.com/thesauri/research_methods/) for research methods
- [envped](<http://www.learn-web.com/thesauri/envedped/) for pedagogies
- [tech](http://www.learn-web.com/thesauri/technologies/>) for technologies

The first three taxonomies are independent of the reviewed domain and can be re-used for other reviews. If a coding nomenclature is described in SKOS it can easily be added to the app.  

## Files
The files are organised into four folders:

**applications:** R and Shiny code, see Section 'The Shiny app' below

**models:** 

**ontologies:** 

**thesauri:** 

## Database
Use of the database is optional. It is required if one wants to use the app as the app functions a client to the database. 

### Initializing the DB

The database needs to contain at least one coding scheme. Typically, one would also import the bibliographic data before using the method or the app because it is tedious to enter such data by hand. But this is not required to get started. 





## The Shiny app

### Preparing the app


### Using the app 
