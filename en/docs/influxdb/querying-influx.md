---
title: "Querying Influx"
slug: "querying-influx"
draft: false
images: []
weight: 9996
type: docs
toc: true
---

## Show databases and connect
To list the available databases, use the following command:

    $ show databases

    name: databases
    name
    ----
    _internal
    devices
    ... list of your databases

You can connect to one specific database:

    $ use <database_name>

By using a single database, the scope for each subsequent query will be limited to that database. This means that you won't have to explicitely provide the name of the database in each subsequent query. 

Do note that by design the SSH connection remembers the current scope, but not all client SDK's are guaranteed to have this available as well.

## Show measurements
When compared to a other database types, a **measurement** in Influx can be considered, on a very high level, as being similar to a _table_ in relational databases or a _collection_ in document databases.

List all measurements for the currently active database:

    $ show measurements

Lists all measurements for a particular database:

    $ show measurements on <database_name>

Example result:

    $ show measurements on devices

    name: measurements
    name
    ----
    health
    location
    network
    usage

Remarks:

The syntax also supports clauses like `WITH`, `WHERE`, `LIMIT` and `OFFSET`. In-depth information about this can be found in the API docs '[Show Measurements](https://docs.influxdata.com/influxdb/v1.2/query_language/schema_exploration/#show-measurements)'.

