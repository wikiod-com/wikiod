---
title: "Getting started with neo4j"
slug: "getting-started-with-neo4j"
draft: false
images: []
weight: 1
type: docs
toc: true
---

## Cypher Query Language
This is the Cypher, Neo4j's query language.  In many ways, Cypher is similar to SQL if you are familiar with it, except SQL refers to items stored in a table while Cypher refers to items stored in a graph.

First, we should start out by learning how to create a graph and add relationships, since that is essentially what Neo4j is all about.

    CREATE (ab:Object { age: 30, destination: "England", weight: 99 })

- You use **CREATE** to create data
- To indicate a node, you use parenthesis: () 
- The ab:Object part can be broken down as follows: a variable 'ab' and label 'Object' for the new node.  Note that the variable can be anything, but you have to be consistent in a line of Cypher Query
- To add properties to the node, use brackets: {} brackets

Next, we will learn about finding MATCHes

    MATCH (abc:Object) WHERE abc.destination = "England" RETURN abc;

MATCH specifies that you want to search for a certain node/relationship pattern
(abc:Object) refers to one node Pattern (with label Object) which store the matches in the variable abc.  You can think of this entire line as the following

    abc =  find the matches that is an Object WHERE the destination is England.

In this case, WHERE adds a constraint which is that the destination must be England.  You must include a return at the end for all MATCH queries (neo4j will not accept just a Match...your query must always return some value [this also depends on what type of query you are writing...we will talk more about this later as we introduce the other types of queries you can make].

The next line will be explained in the future, after we go over some more elements of the Cypher Query Language.  This is to give you a taste of what we can do with this language! 
  Below, you will find an example which gets the cast of movies whose title starts with 'T'

    MATCH (actor:Person)-[:ACTED_IN]->(movie:Movie)
    WHERE movie.title STARTS WITH "T"
    RETURN movie.title AS title, collect(actor.name) AS cast
    ORDER BY title ASC LIMIT 10;




A complete list of commands and their syntax can be found at the official [Neo4j Cypher Reference Card here][1].


  [1]: http://neo4j.com/docs/cypher-refcard/current/ "Neo4j Cypher Refcard"

## Installation or Setup
Go to [Install Neo4j][1] which should detect the OS platform via your web browser, download and follow the usual installation instructions for your OS. 

Neo4j was created with Java, therefore will run on any platform with Java installed, however the Neo4j team has simplified installation by providing easy installation packages for popular platform (e.g. a .dmg for Mac, a .deb for Debian and Ubuntu, an .exe for Windows 64 and 32 bit platforms...).

*To review other versions and platforms available, see [Other Neo4j Releases Page][2]*


  [1]: https://neo4j.com/download/
  [2]: https://neo4j.com/download/other-releases/


**Setup Neo4j as a Docker container :**

    ## Required : Docker machine, docker cli

    # Pull neo4j image from the docker hub
    docker pull neo4j
    
    # create the docker container
    docker run \
        --publish=7474:7474 --publish=7687:7687 \
        --volume=$HOME/neo4j/data:/data \
        neo4j
    
    # If you are running docker directly on the host (e.g ubuntu, RHEL, CentOs etc)
    #     Access the neo4j console at http://localhost:7474
    # If you are on OSX/ Windows
    #     Access the neo4j console at http://<docker-machine-ip>:7474


## Installation & Starting a Neo4j server
Prerequisite steps:
- Install Java at your machine
- Visit [neo4j website][1] and click the link "Download Community Edition" or visit directly the [download link][2].
- Unzip the .tar downloaded file in your home directory

# Start Neo4j from console (headless, without web server)

- Visit the sub-directory `/bin` of the extracted folder and execute in terminal
`./neo4j console`
- You can now execute neo4j queries in the terminal

# Start Neo4j web server

- Visit the sub-directory /bin of the extracted folder and execute in terminal
`./neo4j start`
- Visit http://localhost:7474/
- Only the first time, you will have to sign in with the default account and change the default password. As of community version 3.0.3, the default username and password are neo4j and neo4j.
- You can now insert Neo4j queries in the console provided in your web browser and visually investigate the results of each query.

# Start Neo4j web server

Each Neo4j server currently (in the community edition) can host a single Neo4j database, so in order to setup a new database:
- Visit sub-directory `/bin` and execute `./neo4j stop` to stop the server
- Visit the sub-directory `/conf` and edit the file `neo4j.conf`, changing the value of the parameter `dbms.active_database` to the name of the new database that you want to create.
- Visit again the sub-directory /bin and execute `./neo4j start`
- The web server has started again with the new empty database. You can visit again http://localhost:7474/ to work with the new database.
- The created database is located in the sub-directory `/data/databases`, under a folder with the name specified in the parameter `dbms.active_database`.

# Delete one of the databases

- Make sure the Neo4j server is not running; go to sub-directory /bin and execute `./neo4j status`. If the output message shows that the server is running, also execute `./neo4j stop`.
- Then go to sub-directory /data/databases and delete the folder of the database you want to remove.

  [1]: https://neo4j.com/download/
  [2]: https://neo4j.com/download-thanks/?edition=community

## RDBMS Vs Graph Database

RDBMS    |   Graph Database
---------|-----------------
Tables   | Graphs
Rows     |   Nodes
Columns and Data    |    Properties and its values
Constraints    |    Relationships
Joins    |    Traversal


