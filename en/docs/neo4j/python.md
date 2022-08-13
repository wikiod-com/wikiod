---
title: "Python"
slug: "python"
draft: false
images: []
weight: 9994
type: docs
toc: true
---

## Install neo4jrestclient
    pip install neo4jrestclient

## Connect to neo4j
    from neo4jrestclient.client import GraphDatabase
    db = GraphDatabase("http://localhost:7474", username="neo4j", password="mypass")

## Create some nodes with labels
    user = db.labels.create("User")
    u1 = db.nodes.create(name="user1")
    user.add(u1)
    u2 = db.nodes.create(name="user2")
    user.add(u2)
    
# You can associate a label with many nodes in one go
    Language = db.labels.create("Language")
    b1 = db.nodes.create(name="C++")
    b2 = db.nodes.create(name="Python")
    beer.add(b1, b2)

## Create relationships
    u1.relationships.create("likes", b1)
    u1.relationships.create("likes", b2)
    u2.relationships.create("likes", b1)
# Bi-directional relationships
    u1.relationships.create("friends", u2)

## Match using neo4jrestclient
    from neo4jrestclient import client

    q = 'MATCH (u:User)-[r:likes]->(m:language) WHERE u.name="Marco" RETURN u, type(r), m'
# "db" as defined above
    results = db.query(q, returns=(client.Node, str, client.Node))
# Print results
    for r in results:
    print("(%s)-[%s]->(%s)" % (r[0]["name"], r[1], r[2]["name"]))
# Output:
    (Marco)-[likes]->(C++)
    (Marco)-[likes]->(Python)

