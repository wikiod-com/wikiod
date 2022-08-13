---
title: "Design Documents"
slug: "design-documents"
draft: false
images: []
weight: 9989
type: docs
toc: true
---

Design documents behave like all documents in terms of revisions, replication, and conflicts. You can also add attachments to design documents.

## _design/example
Design documents contain application logic. Any document in a database that has  an _id starting with "_design/" can be used as design document. Usually there is one design document for each application.

    {
        "_id": "_design/example",
        "view": {
            "foo": {
                "map": "function(doc){...};",
                "reduce": "function(keys, values, rereduce){...};"
            }
        }
    }

The example above defines a **view** named *foo*, which can be requested from the following path, assuming the database is named *db*:

> http://localhost:5984/db/_design/example/_view/foo

