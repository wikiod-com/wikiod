---
title: "LOAD Operator"
slug: "load-operator"
draft: false
images: []
weight: 9992
type: docs
toc: true
---

## Loading Stock market data
Let us assume the following stock market data stored in `HDFS`. It is a `CSV` file with fields: **Symbol, Date, Open, High, Close & Volume**.

    ABT,20160106,42.310001,42.98,42.209999,42.560001,5906000
    BAC,20160201,14.05,14.09,13.8,13.96,105739400
    CAS,20160129,1.9,1.97,1.83,1.84,34500
    DCA,20160129,3.46,3.54,3.46,3.51,84600
    ECL,20160114,103.480003,105.400002,102.480003,104.82,1485000
    FAF,20160201,34.040001,34.82,33.939999,34.639999,1222600
    TYL,20160201,156.070007,159.550003,155.690002,158.259995,177100
    UTL,20160201,38.610001,39.889999,38.57,39.27,119500
    VTR,20160128,54.09,54.73,53.549999,53.790001,2441300
    WWE,20160201,17.629999,18,17.27,17.799999,734100
    XRX,20160104,10.41,10.43,10.13,10.3,9122600
    YUM,20160104,71.32,72.25,70.639999,72.209999,3466300
    ZTR,20160104,12.1,12.14,11.98,12.11,60200

**Example 1** A simple `LOAD` statement for the above data would look like:

    stocks = load '/user/pig/stock.txt' using PigStorage(',') as 
                 (sym:chararray, date:int, open:float, high:float, low:float, 
                  close:float, vol:int);



## Loading data from ElasticSearch
Before checking the specific syntax, let's take a look on how to setup your environment to load needed plugins.

**Setup**

To load data directly from ElasticSearch you need to download the `elasticsearch-hadoop` plugin. You have different ways to make it work, for a quick setup you can do the following.

In order to make it work, you need to put the jar file `elasticsearch-hadoop-<version>.jar` in a folder of the node where you have the pig server installed. In my case - quite common googling around - I also needed to add `commons-httpclient-<version>.jar` inside that folder.

Then you can run `pig` in shell mode (called `grunt`) simply typing `pig` on the console. Then you have to load those two jars in the following way:

    REGISTER /path/to/jars/commons-httpclient-<version>.jar;
    REGISTER /path/to/jars/elasticsearch-hadoop-<version>.jar;

Now you are ready to write some code.

**Example**

Let's check the syntax to load data from a complex case.

    DATA = LOAD 'my_index/log' USING org.elasticsearch.hadoop.pig.EsStorage(
    'es.nodes=https://server1:port1,https://server2:port2,https://server3:port3',
    'es.query=?q=*',
    'es.net.ssl=true',
    'es.net.http.auth.user=user',
    'es.net.http.auth.pass=pass',
    'es.net.ssl.keystore.type=JKS',
    'es.net.ssl.truststore.location=file:///path/to/truststore.jks',
    'es.net.ssl.truststore.pass=pass');

This is the complete example, now let's analyze it step by step.

* `es.nodes` holds the list of the nodes of your ElasticSearch cluster. You have to specify your nodes as a comma separated list, with the associated port.
* `es.query` holds the query that will be submitted to ElasticSearch in order to fetch data. You can also put a query in DSL format, but be careful that only the match part of the query will be considered! If you try to limit the number of fields through the query DSL it won't work: in order to achieve that you need to use the `es.read.source.filter` parameter. example of a query DSL: `'es.query = { "query":{ "match_all":{} } }'`
* `es.net.ssl=true` is self explanatory, you need also to give the login credentials to ElasticSearch with `es.net.http.auth.user` and `es.net.http.auth.pass`.
* `es.net.ssl.keystore.type` if you need a truststore, you can select here the type. In the `es.net.ssl.truststore.location` parameter you set the location of the file, be careful to add `file://` prefix, and in the `es.net.ssl.truststore.pass` parameter you set the password of the truststore file.

**Some useful settings**

* `es.read.source.filter=field1,field2,field3` allows you to fetch only the specified fields from ElasticSearch (in this example three).
* `es.output.json=true` allows you to fetch data in a key-value format (JSON). Setting to `false` will return data in CSV format (default).

