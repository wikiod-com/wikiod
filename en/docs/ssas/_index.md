---
title : ssas Tutorial
slug : ssas-tutorial
weight : 9980
draft : false
images : []
type : docs
---

SSAS is a Microsoft analytical solution. It began strictly as an OLAP solution, but it now has multiple modes. SSAS can be used as a semantic layer that supports both formatted reports and managed self-service BI scenarios. Client tools connect to SSAS databases through OLE DB or ADOMD providers. 

Typical Reasons for using SSAS include: 

 - increased query speed
 - shared metadata (joins, hierarchies, KPIs)
 - ability to implement row-level security
 - multidimensional analysis (ad-hoc analysis, advanced time calculations, drill-through)
 - avoiding resource contention with OLTP source systems
 - consolidating data from multiple sources


BISM Architecture
-------
[![BISM Artchitecture][1]][1]

Modes
=====

From SSAS 2012 through SSAS 2016, there are 3 available modes: Multidimensional, Tabular, and Power Pivot for SharePoint. Both multidimensional and tabular modes have the ability to store the source data in the model and process it periodically, or directly query the data from the source. Directly accessing the source data is called ROLAP in multidimensional mode and Direct Query in Tabular mode.

Multidimensional
-------

Multidimensional SSAS is seen as as a more mature, traditional corporate BI solution. It uses an OLAP engine and requires a well formed star schema data model to work optimally. MDX is used to query the SSAS cube and XMLA is use to define, deploy, update, and process the cube. 

Attributes of a Multidimensional SSAS Solution: 

 - stored model can be larger than server memory
 - can have multiple cubes per database
 - can process partitions in parallel
 - contains data mining capabilities

Features of a Multidimensional SSAS Solution: 

 - native parent-child hierarchies
 - native many-to-many relationships
 - writeback
 - named sets
 - native role-playing dimensions
 - dimension attributes for optimization, discretization, default member, aggregation 


## Tabular ##
Tabular SSAS was introduced with SSAS 2012. It uses an in-memory xVelocity engine. DAX is the native query language, although MDX can be used and the SSAS engine will translate it to DAX. From compatibility level 1200 forward, TMSL (Tabular Model Scripting Language) is JSON that is used to define and process the tabular model. Tabular SSAS solutions on compatibility level 1100 and 1103 use XMLA. 

 - In-memory means it must fit on the server
 - Single model per database
 - Partitions are processed serially until SSAS 2016
 - Performs better on distinct counts in many situations compared to a multidimensional alternative

Features of a Tabular SSAS Solution:

 - parent-child hierarchies through DAX
 - many-to-many relationships through DAX 
 - no native UI for creating drillthrough actions
 - greater ability to integrate disparate data sources compared to multidimensional
 - flexible model (tables rather than dimensions and measure groups)
 - role playing dimensions with calculated tables in SSAS 2016+
 - Upgrade path from Power Pivot models

  [1]: https://i.stack.imgur.com/30whE.png

