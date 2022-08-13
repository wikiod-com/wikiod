---
title: "Mongo as Shards"
slug: "mongo-as-shards"
draft: false
images: []
weight: 9985
type: docs
toc: true
---

## Sharding Environment Setup
Sharding Group Members : 
 
   For sharding there are three players.

   

 1. Config Server
 2. Replica Sets
 3. Mongos

    For a mongo shard we need to setup the above three servers.

Config Server Setup : 
     add the following to mongod conf file

    sharding:
      clusterRole: configsvr
    replication:
      replSetName: <setname>  

  **run :** mongod --config <path-to-config-file>

*we can choose config server as replica set or may be a standalone server. Based on our requirement we can choose the best. If config need to run in replica set we need to follow the replica set setup*


**Replica Setup :** 
      Create replica set
    // Please refer the replica setup

**MongoS Setup :**
         Mongos is main setup in shard. Its is query router to access all replica sets
 
 Add the following in mongos conf file 

        sharding:
          configDB: <configReplSetName>/cfg1.example.net:27017;
 
Configure Shared :

 Connect the mongos via shell (mongo --host <Mongoshostname> --port <port>)

    

 1. sh.addShard( "<replSetName>/s1-mongo1.example.net:27017")
 2. sh.enableSharding("<database>")
 3. sh.shardCollection("< database >.< collection >", { < key > : < direction > } )
 4. sh.status() // To ensure the sharding




