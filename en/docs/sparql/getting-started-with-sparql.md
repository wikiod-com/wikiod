---
title: "Getting started with sparql"
slug: "getting-started-with-sparql"
draft: false
images: []
weight: 1
type: docs
toc: true
---

## Getting Started with a SPARQL Endpoint
A query engine is required in order to execute SPARQL queries over a dataset. Query engines may be embedded in applications, or accessed remotely. Remote access may be through a vendor-specific API, or through the [SPARQL protocol](https://www.w3.org/TR/sparql11-protocol/) if an implementation supports it. This documentation will not cover how to submit queries through specific vendor APIs.

SPARQL Endpoint implementations typically provide a user-friendly web interface for submitting queries and viewing their results. Public SPARQL endpoints (such as [DBPedia](http://dbpedia.org/sparql)) can serve as useful datasets for non-mutating examples.

If you wish to configure a private SPARQL Endpoint for experimentation, [Apache Fuseki](https://jena.apache.org/documentation/serving_data/) provides a free and platform independent option.

