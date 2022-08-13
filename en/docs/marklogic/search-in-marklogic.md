---
title: "Search in MarkLogic"
slug: "search-in-marklogic"
draft: false
images: []
weight: 9993
type: docs
toc: true
---

This section provides an overview of search in MarkLogic. Intent is to cover cts:search, search:search and qbe with use cases and examples

## Fetching all the documents with word "marklogic"
    cts:search(
        fn:doc(), 
        cts:word-query("marklogic"))

## Fetching all the documents with word "marklogic", document in collection "first-collection"
This can be done in the following two ways -

    cts:search(
        fn:collection("first-collection"), 
        cts:word-query("marklogic"))

In this, the scope is changed from all the documents to documents in collection "first-collection" only.

In the second approach, use of [cts:collection-query][1] has been made. This should give better performance than the first approach.
    
    cts:search(
        fn:doc(), 
        cts:and-query((
            cts:collection-query("first-collection"),
            cts:word-query("marklogic")))

[1]:https://docs.marklogic.com/cts:collection-query

## Fetching all the documents with a particular value of an element
This query returns all the documents with element "company" and its value as "marklogic"

    cts:element-value-query(xs:QName('company'), 'marklogic'))

## Checking presence of elements and attributes in documents
The following query returns the documents which have an element named "company" -

    cts:element-value-query(
    xs:QName('company'), '*', ("wildcarded")))

The following query returns the documents which have an element named "company" with an attribute named "name" -

    cts:element-attribute-value-query(
    xs:QName('company'), xs:QName('name'), '*', ("wildcarded")))  

