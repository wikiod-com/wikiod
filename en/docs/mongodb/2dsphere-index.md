---
title: "2dsphere Index"
slug: "2dsphere-index"
draft: false
images: []
weight: 9993
type: docs
toc: true
---

## Create a 2dsphere Index
`db.collection.createIndex()` method is used to create a `2dsphere` index. The blueprint of a `2dsphere` index : 

    db.collection.createIndex( { <location field> : "2dsphere" } )
Here, the `location field` is the key and `2dsphere` is the type of the index. In the following example we are going to create a `2dsphre` index in the `places` collection.
    
    db.places.insert(
    {
      loc : { type: "Point", coordinates: [ -73.97, 40.77 ] },
      name: "Central Park",
      category : "Parks"
    })
The following operation will create `2dsphere` index on the `loc` field of `places` collection.
    
    db.places.createIndex( { loc : "2dsphere" } )

