---
title: "Cypher"
slug: "cypher"
draft: false
images: []
weight: 9978
type: docs
toc: true
---

Cypher is the query language used by Neo4j. You use Cypher to perform tasks and matches against a Neo4j Graph. 

Cypher is ["inspired by SQL"](https://neo4j.com/developer/cypher-query-language/#_about_cypher) and is designed to by intuitive in the way you describe the relationships, i.e. typically the drawing of the pattern will look similar to the Cypher representation of the pattern.

## Deletion
# Delete all nodes

    MATCH (n) 
    DETACH DELETE n

`DETACH` doesn't work in older versions(less then 2.3), for previous versions use

    MATCH (n)
    OPTIONAL MATCH (n)-[r]-()
    DELETE n, r

# Delete all nodes of a specific label

    MATCH (n:Book) 
    DELETE n

## Update a Node
    MATCH (n) 
    WHERE n.some_attribute = "some identifier" 
    SET n.other_attribute = "a new value"

## Delete All Orphan Nodes
Orphan nodes/vertices are those lacking all relationships/edges.

``` cypher
MATCH (n)
WHERE NOT (n)--()
DELETE n
```

## Creation
# Create a node

    CREATE (neo:Company) //create node with label 'Company'

    CREATE (neo:Company {name: 'Neo4j', hq: 'San Mateo'}) //create node with properties

# Create a relationship

    CREATE (beginning_node)-[:edge_name{Attribute:1, Attribute:'two'}]->(ending_node)

# Query Templates

Running neo4j locally, in the browser GUI (default: http://localhost:7474/browser/), you can run the following command to get a palette of queries.

`:play query template`

This helps you get started creating and merging nodes and relationships by typing queries.

## Create an Edge
    CREATE (beginning_node)-[:edge_name{Attribute:1, Attribute:'two'}]->(ending_node)

## Match (capture group) and link matched nodes
    Match (node_name:node_type {}), (node_name_two:node_type_two {})
    CREATE (node_name)-[::edge_name{}]->(node_name_two)

