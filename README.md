# LitGraph - the 'living' review as Knowledge Graph

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

**application:** R and Shiny code, see Section 'The Shiny app' below

**models:** This folder contains optionally the input and output of coding as static files. It is up to the user if model files exist outside of the database content. For cases where the database (and app) are not used, this would be the place to store manually coded information. 

**ontologies:** Contains the OWL or RDFS ontologies used in the method and the app. This is optional information, and it is also optional to load the ontologies into the database or not. The former is recommended for any reasoning task to be performed on the database and for making ontological information available to the users of the database. 

### Thesauri

This is the folder for storing SKOS thesauri. To use a thesaurus in the app, the following needs to be specified in the respective file. 

The thesauri can be used for coding by creating models that are stored in files. They play also an important role wheh using the app. The app uses information from an thesaurus file to offer suggestions in to the app user as to (a) which coding schemes are available and (b) what the concepts in each scheme are. If the concept definitions include `skos:broader` relations, these will be used for displaying the concepts for selection in the app. The app also uses labels, definitions etc., if available. 

Using Bloom's Taxonomy in the file `BloomThesaurus.skos.ttl` as the example, the following information must be provided:

1. Namespace and prefixes: 

```
PREFIX : <http://www.learn-web.com/thesauri/bloom_outcomes/>  
PREFIX bloom: <http://www.learn-web.com/thesauri/bloom_outcomes/>
PREFIX skos: <http://www.w3.org/2004/02/skos/core#>
```
These must correspond to the `prefix` and `uri` columns in the file `predicates.csv` (and the variable `predicates` once the csv file is loaded into the app). The file `predicates.csv` is located in the folder `application` because the application reads in this file when initialising. (see below, "Preparing the app", for more information about `predicates.csv`.)

2. A single `skos:ConceptScheme`, the name of which needs to correspond with the one in the column `skos` in the file `predictes.csv`. For instance, in the Bloom taxonomy we find this definition: 

```
:BloomThesaurus a skos:ConceptScheme ; 
  rdfs:label "Bloom learning outcomes" ;
	dc:title "Bloom learning outcomes " ;
	skos:hasTopConcept :CognitiveOutcome, :AffectiveOutcome, :PsychomotorOutcome .
```
Only the first line is required, the other properties are optional. 

3. Each concept in the thesaurus should be member of the ConceptScheme. For instance:

```
:CognitiveOutcome a skos:Concept ; 
	skos:inScheme :BloomThesaurus ; 
	skos:prefLabel "Cognitive learning outcome"@en . 
```


## Database
Use of the database is optional. It is required if one wants to use the app as the app functions a client to the database. For most users, it will be a black box, with only an administrator being concerned with it. 

### Initializing the DB
The database needs to contain at least one coding scheme. Typically, one would also import the bibliographic data before using the method or the app because it is tedious to enter such data by hand. But this is not required to get started. 

A user with r/w rights on the database uploads skos files and whatver bibliographic information is available in RDF format into the database. This step is not covered here as it usually done by the database administrator. 


## The Shiny app

### Preparing the app
The app gets information about the predicates to offer to the user in form of a table `predicates` and in form of files with SKOS concept schemes (see above, "Thesauri"). 

#### Information in the file predicates.csv
Each predicate that the app is supposed to use needs to have a row in the spreadsheet. The attributes (columns) for each predicate are:

* label: the predicate name, including the prefix. For instance, `foaf:name`, `cito:cites`. 
* aspect: The aspect bundles predicates into logical groups. For instance, `Author`, `Citation`. 
* domain: The predicates domain (the kinds of resources it applies to), in prefix notation. (Note: Only one domain per predicate)
* range: The kind of values the predicate can take. For instance `xsd:string` for literals, `bibo:Article` for scholarly works. A special case is where the value range should come from a SKOS thesaurus. In that case, range = `Thesaurus`. (Note: Only one range per predicate). 
* skos: If the range has value `Thesaurus`, then provide here the name of the skos:ConceptScheme that delivers the values for the predicate; keep empty otherwise. 
* prefix: The prefix for the predicate's namespace
* uri: The URI for the predicate's namespace. 

Note that prefix and URI have to correspond to the predicate label, and the prefixes and URIs for SKOS thesauri need to correspond with the information in the file with the thesaurus definitions. See "Thesauri" above. 


#### Information in the app itself
In the app, information about predicates needs to be provided in the first lines of the code of `app.R` in the folder `application`. 

See https://github.com/prei007/litrev/issues/47 for reducing the need to do this manually. 


### Using the app 

provide a video

