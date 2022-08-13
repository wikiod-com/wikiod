---
title: "Curl Commands"
slug: "curl-commands"
draft: false
images: []
weight: 9918
type: docs
toc: true
---

## Syntax
 - curl -X\<VERB> '\<PROTOCOL>://\<HOST>:\<PORT>/\<PATH>?\<QUERY_STRING>' -d '\<BODY>'
 - Where:
 - VERB: The appropriate HTTP method or verb: GET, POST, PUT, HEAD, or DELETE
 - PROTOCOL: Either http or https (if you have an https proxy in front of Elasticsearch.) 

 - HOST: The hostname of any node in your Elasticsearch cluster, or localhost for a node on your local machine.

 - PORT: The port running the Elasticsearch HTTP service, which defaults to 9200.

 - PATH: API Endpoint (for example _count will return the number of documents in the cluster). Path may contain multiple components, such as _cluster/stats or _nodes/stats/jvm

 - QUERY_STRING: Any optional query-string parameters (for example ?pretty will pretty-print the JSON response to make it easier to read.) 

 - BODY: A JSON-encoded request body (if the request needs one.) 

 - Reference: [Talking to Elasticsearch : Elasticsearch Docs][1]


  [1]: https://www.elastic.co/guide/en/elasticsearch/guide/current/_talking_to_elasticsearch.html#_talking_to_elasticsearch

## Curl Command for counting number of documents in the cluster
    curl -XGET 'http://www.example.com:9200/myIndexName/_count?pretty'

Output:

    {
      "count" : 90,
      "_shards" : {
        "total" : 6,
        "successful" : 6,
        "failed" : 0
      }
    }
    
The index has 90 documents within it.

Reference Link: [Here][1]


  [1]: https://www.elastic.co/guide/en/elasticsearch/guide/current/_talking_to_elasticsearch.html#_talking_to_elasticsearch

## Retrieve a document by Id
    curl -XGET 'http://www.example.com:9200/myIndexName/myTypeName/1'

Output:

    {
        "_index" : "myIndexName",
        "_type" : "myTypeName",
        "_id" : "1",
        "_version" : 1,
        "found": true,
        "_source" : {
            "user" : "mrunal",
            "postDate" : "2016-07-25T15:48:12",
            "message" : "This is test document!"
        }
    }

Reference Link: [Here][1]


  [1]: https://www.elastic.co/guide/en/elasticsearch/reference/current/docs-get.html

## Create an Index
    curl -XPUT 'www.example.com:9200/myIndexName?pretty'

Output:

    {
      "acknowledged" : true
    }
Reference Link: [Here][1]


  [1]: https://www.elastic.co/guide/en/elasticsearch/reference/1.4/_create_an_index.html

## List all indices
    curl 'www.example.com:9200/_cat/indices?v'

output:

    health status index               pri rep docs.count docs.deleted store.size pri.store.size 
    green  open   logstash-2016.07.21   5   1       4760            0      4.8mb          2.4mb 
    green  open   logstash-2016.07.20   5   1       7232            0      7.5mb          3.7mb 
    green  open   logstash-2016.07.22   5   1      93528            0    103.6mb           52mb 
    green  open   logstash-2016.07.25   5   1      20683            0     41.5mb         21.1mb 

Reference Link: [Here][1]


  [1]: https://www.elastic.co/guide/en/elasticsearch/reference/1.4/_list_all_indexes.html

## Delete an Index
    curl -XDELETE 'http://www.example.com:9200/myIndexName?pretty'

output:

    {
      "acknowledged" : true
    }

Reference Link: [Here][1]


  [1]: https://www.elastic.co/guide/en/elasticsearch/reference/1.4/_delete_an_index.html

## List all documents in a index
    curl -XGET http://www.example.com:9200/myIndexName/_search?pretty=true&q=*:*


This uses the `Search` API and will return all the entries under index `myIndexName`.

Reference Link: [Here][1]


  [1]: https://www.elastic.co/guide/en/elasticsearch/reference/current/search-search.html

