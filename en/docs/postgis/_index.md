---
title : postgis Tutorial
slug : postgis-tutorial
weight : 9991
draft : false
images : []
type : docs
---

PostGIS is a set of extensions for the PostgreSQL database. With PostGIS you can store geospatial data and perform spatial queries on a postgres database.

Unlike the default datatypes on a normal postgres database, spatial data has to be handled differently. The queries you can perform on a spatial database are usually defined by bounding-boxes in 2 or 3 dimensions. To store, index and handle these datatypes postGIS uses a concept called [R-Trees][1], wich is not part of the default postgres-installation.

With a postGIS database, you are able to:

 * store spatial data
 * perform spatial queries, to retrieve and extract information (points, areas).
 * manage spatial information and metadata on tables (like: used coorinate-reference-system).
 * convert geometries from one coordinate-system to another
 * compare geometries, and extract properties (like: edge-length of a road or building)
 * generate new geometries from others.


  [1]: https://en.wikipedia.org/wiki/R-tree

