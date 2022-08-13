---
title: "Python Interface"
slug: "python-interface"
draft: false
images: []
weight: 9962
type: docs
toc: true
---

## Parameters
| Parameter | Details |
| ------ | ------ |
| hosts   | Array of hosts in the form of object containing keys `host` and `port`. Default `host` is 'localhost' and `port` is 9200. A sample entry looks like   `[{"host": "ip of es server", "port": 9200}]`|
|sniff_on_start| Boolean if you want the client to sniff nodes on startup, sniffing means getting list of nodes in elasticsearch cluster|
|sniff_on_connection_fail| Boolean for triggering sniffing if connection fails when client is active|
| sniffer_timeout | time difference in seconds between each sniff|
| sniff_timeout | time for a single request of sniffing in seconds|
| retry_on_timeout |Booelan for if client should timeout trigger contacting a different elasticsearch node or just throw error|
|http_auth| Basic http authentication can be provided here in the form of `username:password`|


## Indexing a Document (ie. Adding an sample)
Install the necessary Python Library via:

    $ pip install elasticsearch

Connect to Elasticsearch, Create a Document (e.g. data entry) and "Index" the document using Elasticsearch.

    from datetime import datetime
    from elasticsearch import Elasticsearch

    # Connect to Elasticsearch using default options (localhost:9200)
    es = Elasticsearch()

    # Define a simple Dictionary object that we'll index to make a document in ES
    doc = {
        'author': 'kimchy',
        'text': 'Elasticsearch: cool. bonsai cool.',
        'timestamp': datetime.now(),
    }

    # Write a document
    res = es.index(index="test-index", doc_type='tweet', id=1, body=doc)
    print(res['created'])

    # Fetch the document
    res = es.get(index="test-index", doc_type='tweet', id=1)
    print(res['_source'])

    # Refresh the specified index (or indices) to guarantee that the document
    #  is searchable (avoid race conditions with near realtime search)
    es.indices.refresh(index="test-index")

    # Search for the document
    res = es.search(index="test-index", body={"query": {"match_all": {}}})
    print("Got %d Hits:" % res['hits']['total'])

    # Show each "hit" or search response (max of 10 by default)
    for hit in res['hits']['hits']:
        print("%(timestamp)s %(author)s: %(text)s" % hit["_source"])

## Connection to a cluster
    es = Elasticsearch(hosts=hosts, sniff_on_start=True, sniff_on_connection_fail=True, sniffer_timeout=60, sniff_timeout=10, retry_on_timeout=True)

## Creating an empty index and setting the mapping
In this example, we create an empty index (we index no documents in it) by defining its mapping.

First, we create an `ElasticSearch` instance and we then define the mapping of our choice. Next, we check if the index exists and if not, we create it by specifying the `index` and `body` parameters that contain the index name and the body of the mapping, respectively.
 
    from elasticsearch import Elasticsearch
    
    # create an ElasticSearch instance
    es = Elasticsearch()
    # name the index
    index_name = "my_index"
    # define the mapping
    mapping = {
        "mappings": {
            "my_type": {
                    "properties": {
                        "foo": {'type': 'text'},
                        "bar": {'type': 'keyword'}
                    }
                }
            }
        }
        
    # create an empty index with the defined mapping - no documents added
    if not es.indices.exists(index_name):
        res = es.indices.create(
            index=index_name,
            body=mapping
        )
        # check the response of the request
        print(res)
        # check the result of the mapping on the index
        print(es.indices.get_mapping(index_name))

## Partial Update and Update by query
Partial Update: Used when a partial document update is needed to be done, i.e. in the following example the field `name` of the document with id `doc_id` is going to be updated to 'John'. Note that if the field is missing, it will just be added to the document.

    doc = {
        "doc": {
            "name": "John"
        }
    }
    es.update(index='index_name',
              doc_type='doc_name',
              id='doc_id',
              body=doc)

Update by query: Used when is needed to update documents that satisfy a condition, i.e. in the following example we update the age of the documents whose `name` field matches 'John'.

   
    q = {
      "script": {
        "inline": "ctx._source.age=23",
        "lang": "painless"
      },
      "query": {
        "match": {
            "name": "John"
        }
      }
    }

    es.update_by_query(body=q, 
                       doc_type='doc_name', 
                       index='index_name')

