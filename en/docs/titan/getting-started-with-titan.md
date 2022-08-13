---
title: "Getting started with titan"
slug: "getting-started-with-titan"
draft: false
images: []
weight: 1
type: docs
toc: true
---

## Installation or Setup
Detailed instructions on getting titan set up or installed.

## Initialising a Titan Graph
With the appropriate [storage backend](http://s3.thinkaurelius.com/docs/titan/1.0.0/storage-backends.html) running a new titan graph can be initialised via:

    graph = TitanFactory.open("config.properties");

Wehere `config.properties` defines several [configurations](http://s3.thinkaurelius.com/docs/titan/1.0.0/titan-config-ref.html) relevant to the storage backend. Titan provides some sample configs in its downloadable package. For example `conf/titan-cassandra.properties`

**A shorthand** can also be used when you want to ignore most configuration options:

For example for a [Cassandra](http://s3.thinkaurelius.com/docs/titan/1.0.0/cassandra.html) backend: 

    graph = TitanFactory.open("cassandra:localhost")

For a [Berkeley DB](http://s3.thinkaurelius.com/docs/titan/1.0.0/bdb.html) backend:

    graph = TitanFactory.open('berkeleyje:/tmp/graph')
    
All the above commands will create a Titan Graph which persists the relevant backend. _If this is the first time executing these commands the graph will initially be empty._
 

