---
title: "Search API"
slug: "search-api"
draft: false
images: []
weight: 9973
type: docs
toc: true
---

The search API allows you to execute a search query and get back search hits that match the query. The query can either be provided using a simple query string as a parameter, or using a request body.



## Search using request body
Searches can also be done on elasticsearch using a search DSL.The query element within the search request body allows to define a query using the Query DSL.

    GET /my_index/type/_search
    {
        "query" : {
            "term" : { "field_to_search" : "search_item" }
        }
    }

## Multi search
The multi_search option allows us to search for a query in multiple fields at once.

    GET /_search
    {
      "query": {
        "multi_match" : {
          "query":    "text to search", 
          "fields": [ "field_1", "field_2" ] 
        }
      }
    }


We can also boost the score of certain fields using the boost operator(^), and use wild cards in the field name (*)

    GET /_search
        {
          "query": {
            "multi_match" : {
              "query":    "text to search", 
              "fields": [ "field_1^2", "field_2*" ] 
            }
          }
        }

## Routing
When executing a search, it will be broadcast to all the index/indices shards (round robin between replicas). Which shards will be searched on can be controlled by providing the routing parameter. For example, when indexing tweets, the routing value can be the user name:

    curl -XPOST 'localhost:9200/twitter/tweet?routing=kimchy&pretty' -d'
    {
        "user" : "kimchy",
        "postDate" : "2009-11-15T14:12:12",
        "message" : "trying out Elasticsearch"
    }'



## URI search, and Highlighting
A search request can be executed purely using a URI by providing request parameters. Not all search options are exposed when executing a search using this mode, but it can be handy for quick "curl tests".

    GET Index/type/_search?q=field:value


Another useful feature provided is highlighting the match hits in the documents.

    GET /_search
    {
        "query" : {
            "match": { "field": "value" }
        },
        "highlight" : {
            "fields" : {
                "content" : {}
            }
        }
    }

In the above case, the particular field will be highlighted for each search hit

