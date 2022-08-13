---
title: "Working with graphs"
slug: "working-with-graphs"
draft: false
images: []
weight: 9995
type: docs
toc: true
---

What is a Named Graph? 

An internal database document identifier (name) used by a SQL-compliant RDBMS to partition storage of relations represented as RDF sentence/statement graphs. 

Why are Named Graphs Important? 

A Named Graph is like a page in a book (the database) that contains a collection of paragraphs (sentence collections). Thus, it provides a powerful mechanism for query scoping that negates the need to scope all database queries to the entire database.

## Storing Data in a Named Graph
    PREFIX    rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#> 
    PREFIX    owl: <http://www.w3.org/2002/07/owl#> 
    PREFIX schema: <http://schema.org/> 
    PREFIX   foaf: <http://xmlns.com/foaf/0.1/>
       
    WITH <urn:this:database:doc>
    INSERT { <#relatedTo> a                     rdf:Property, owl:ObjectProperty ;
                          rdfs:label            "relatedTo"                      ;
                          rdfs:domain           rdfs:Resource                    ;
                          rdfs:range            xsd:anyURI                       .
             foaf:knows rdfs:subPropertyOf      <#relatedTo>                     ;
                        owl:equivalentProperty  schema:knows                     . 
             <#this>    a                       foaf:Person                      ; 
                        schema:name             "John Doe"                . 
             <#that>    a                       foaf:Person                      ; 
                        schema:name             "Jane Doe"                  . 
           }



