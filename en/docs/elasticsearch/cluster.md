---
title: "Cluster"
slug: "cluster"
draft: false
images: []
weight: 9960
type: docs
toc: true
---

Cluster Health provides a lot of information about the cluster, such as the number of shards that are allocated ("active") as well as how many are unassigned and relocating. In addition, it provides the current number of nodes and data nodes in the cluster, which can allow you to poll for missing nodes (e.g., if you expect it to be `15`, but it only shows `14`, then you are missing a node).

For someone that knows about Elasticsearch, "assigned" and "unassigned" shards can help them to track down issues.

The most common field checked from Cluster Health is the `status`, which can be in one of three states:

- red
- yellow
- green

The colors each mean one -- and only one -- very simple thing:

1. Red indicates that you are missing _at least_ one primary shard.
    - A missing primary shard means that an index cannot be used to write (index) new data in most cases.
        - Technically, you can still index to any primary shards that are available in that index, but practically it means that you cannot because you do not generally control what shard receives any given document.
        - Searching is still possible against a red cluster, but it means that you will get partial results if any index you search is missing shards.
    - In normal circumstances, it just means that the primary shard is being allocated (`initializing_shards`).
    - If a node just left the cluster (e.g., because the machine running it lost power), then it makes sense that you will be missing some primary shards _temporarily_.
        - Any replica shard for that primary shard will be promoted to be the primary shard in this scenario.
2. Yellow indicates that all primary shards are active, but _at least_ one replica shard is missing.
    - A missing replica only impacts indexing if [consistency settings](https://www.elastic.co/guide/en/elasticsearch/reference/current/docs-index_.html#index-consistency) require it to impact indexing.
         - By default, there is only one replica for any primary and indexing can happen with a single missing replica.
    - In normal circumstances, it just means that the replica shard is being allocated (`initializing_shards`).
    - A one node cluster with replicas enabled will _always_ be yellow _at best_. It can be red if a primary shard is not yet assigned.
        - If you only have a single node, then it makes sense to disable replicas because you are not expecting any. Then it can be green.
3. Green indicates that all shards are active.
    - The only shard activity allowed for a green cluster is `relocating_shards`.
    - New indices, and therefore new shards, will cause the cluster to go from red to yellow to green, as each shard is allocated (primary first, making it yellow, then replicas if possible, making it green).
        - In Elasticsearch 5.x and later, new indices will **not** make your cluster red unless it takes them too long to allocate.

## Human readable, tabular Cluster Health with selected headers
Example uses basic HTTP syntax. Any `<#>` in the example should be removed when copying it.

Like most `_cat` APIs in Elasticsearch, the API selectively responds with a default set of fields. However, other fields exist from the API if you want them:

    GET /_cat/health?help <1>

1. `?help` causes the API to return the fields (and short names) as well as a brief description.

`_cat/health` has existed since Elasticsearch 1.x, but here is an example of its output from Elasticsearch 5.x:

Fields available as-of this example's creation date:

    epoch                 | t,time                                   | seconds since 1970-01-01 00:00:00  
    timestamp             | ts,hms,hhmmss                            | time in HH:MM:SS                   
    cluster               | cl                                       | cluster name                       
    status                | st                                       | health status                      
    node.total            | nt,nodeTotal                             | total number of nodes              
    node.data             | nd,nodeData                              | number of nodes that can store data
    shards                | t,sh,shards.total,shardsTotal            | total number of shards             
    pri                   | p,shards.primary,shardsPrimary           | number of primary shards           
    relo                  | r,shards.relocating,shardsRelocating     | number of relocating nodes         
    init                  | i,shards.initializing,shardsInitializing | number of initializing nodes       
    unassign              | u,shards.unassigned,shardsUnassigned     | number of unassigned shards        
    pending_tasks         | pt,pendingTasks                          | number of pending tasks            
    max_task_wait_time    | mtwt,maxTaskWaitTime                     | wait time of longest task pending  
    active_shards_percent | asp,activeShardsPercent                  | active number of shards in percent 

You can then use this to print only those fields:

    GET /_cat/health?h=timestamp,cl,status&v <1>

1. `h=...` defines the list of fields that you want returned.
2. `v` (verbose) defines that you want it to print the headers.

The output from an instance of Elasticsearch 5.x:

    timestamp cl            status
    15:38:00  elasticsearch yellow



## JSON-based Cluster Health
Example uses basic HTTP syntax. Any `<#>` in the example should be removed when copying it.

The `_cat` APIs are often convenient for humans to get at-a-glance details about the cluster. But you frequently want consistently parseable output to use with software. In general, the JSON APIs are meant for this purpose.

    GET /_cluster/health

`_cluster/health` has existed since Elasticsearch 1.x, but here is an example of its output from Elasticsearch 5.x:

    {
      "cluster_name": "elasticsearch",
      "status": "yellow",
      "timed_out": false,
      "number_of_nodes": 1,
      "number_of_data_nodes": 1,
      "active_primary_shards": 45,
      "active_shards": 45,
      "relocating_shards": 0,
      "initializing_shards": 0,
      "unassigned_shards": 44,
      "delayed_unassigned_shards": 0,
      "number_of_pending_tasks": 0,
      "number_of_in_flight_fetch": 0,
      "task_max_waiting_in_queue_millis": 0,
      "active_shards_percent_as_number": 50.56179775280899
    }

## Human readable, tabular Cluster Health with headers
Example uses basic HTTP syntax. Any `<#>` in the example should be removed when copying it.

You can use the `_cat` APIs to get a human readable, tabular output for various reasons.

    GET /_cat/health?v <1>

1. The `?v` is optional, but it implies that you want "verbose" output.

`_cat/health` has existed since Elasticsearch 1.x, but here is an example of its output from Elasticsearch 5.x:

With verbose output:

    epoch      timestamp cluster       status node.total node.data shards pri relo init unassign pending_tasks max_task_wait_time active_shards_percent
    1469302011 15:26:51  elasticsearch yellow          1         1     45  45    0    0       44             0                  -                 50.6%

## Human readable, tabular Cluster Health without headers
Example uses basic HTTP syntax. Any `<#>` in the example should be removed when copying it.

You can use the `_cat` APIs to get a human readable, tabular output for various reasons.

    GET /_cat/health <1>

`_cat/health` has existed since Elasticsearch 1.x, but here is an example of its output from Elasticsearch 5.x:

Without verbose output:

    1469302245 15:30:45 elasticsearch yellow 1 1 45 45 0 0 44 0 - 50.6%

