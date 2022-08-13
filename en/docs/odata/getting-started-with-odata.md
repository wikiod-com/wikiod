---
title: "Getting started with odata"
slug: "getting-started-with-odata"
draft: false
images: []
weight: 1
type: docs
toc: true
---

## Installation or Setup
Detailed instructions on getting odata set up or installed.

## Odata- The Best way to Rest
From Odata.org

> OData (Open Data Protocol) is an OASIS standard that defines the best
> practice for building and consuming RESTful APIs. OData helps you
> focus on your business logic while building RESTful APIs without
> having to worry about the approaches to define request and response
> headers, status codes, HTTP methods, URL conventions, media types,
> payload formats and query options etc. OData also guides you about
> tracking changes, defining functions/actions for reusable procedures
> and sending asynchronous/batch requests etc. Additionally, OData
> provides facility for extension to fulfil any custom needs of your
> RESTful APIs.

The most exciting feature of Odata from my view are,
 1. **Url conventions**

In normal API's there is no standard way to specify a url, means by seeing someone API we cannot ensure that what that API is doing. Odata helps to create a standard url based on the business logic.
 
 2. **Querying**
 
In normal API, once we created further if we want only a specific data from the response we will do like this 
   
 - Call the API (From Server API returns everything)
 - Apply filtering on the client side.

otherwise creates separate API which results filtered data.

But instead of these OData API allows querying option means we can include filtering conditions in Odata url itself, the Odata automatically filter the result from server so we can acheive what ever data we wants alone.

To play with Odata in postman
http://www.odata.org/getting-started/learning-odata-on-postman/

