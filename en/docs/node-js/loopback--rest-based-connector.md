---
title: "Loopback - REST Based connector"
slug: "loopback---rest-based-connector"
draft: false
images: []
weight: 9993
type: docs
toc: true
---

Rest based connectors and how to deal with them. We all know Loopback does not provide elegance to REST based connections

## Adding a web based connector
<PRE>
//This example gets the response from iTunes 
{
  "rest": {
    "name": "rest",
    "connector": "rest",
    "debug": true,
    "options": {
      "useQuerystring": true,
      "timeout": 10000,
      "headers": {
        "accepts": "application/json",
        "content-type": "application/json"
      }
    },
    "operations": [
      {
        "template": {
          "method": "GET",
          "url": "https://itunes.apple.com/search",
          "query": {
            "term": "{keyword}",
            "country": "{country=IN}",
            "media": "{itemType=music}",
            "limit": "{limit=10}",
            "explicit": "false"
          }
        },
        "functions": {
          "search": [
            "keyword",
            "country",
            "itemType",
            "limit"
          ]
        }
      },
      {
        "template": {
          "method": "GET",
          "url": "https://itunes.apple.com/lookup",
          "query": {
            "id": "{id}"
          }
        },
        "functions": {
          "findById": [
            "id"
          ]
        }
      }
    ]
  }
}
</PRE>

