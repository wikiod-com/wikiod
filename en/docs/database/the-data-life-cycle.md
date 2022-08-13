---
title: "The Data Life Cycle"
slug: "the-data-life-cycle"
draft: false
images: []
weight: 9987
type: docs
toc: true
---

The article describes the different environments in which data can reside and the characteristics of both the data and the environment at each stage of the data life cycle.

## Data life cycle
Data is usually perceived as something static that is entered into a database and later queried. But in many environments, data is actually more similar to a product in an assembly line, moving from one environment to another and undergoing transformations along the way.

[![enter image description here][1]][1]

■ OLTP: online transactional processing

■ DSA: data staging area

■ DW: data warehouse

■ BISM: Business Intelligence Semantic Model

■ DM: data mining

■ ETL: extract, transform, and load

■ MDX: Multidimensional Expressions

■ DAX: Data Analysis Expressions

■ DMX: Data Mining Extensions

**Online Transactional Processing**

Data is entered initially into an online transactional processing (OLTP) system. The focus of an OLTP system is data entry and not reporting—transactions mainly insert, update, and delete data. However, an OLTP environment is not suitable for reporting purposes because a normalized model usually involves many tables (one for each entity) with complex relationships. Even simple reports require joining many tables, resulting in complex and poorly performing queries.

**Data Warehouses**

A data warehouse (DW) is an environment designed for data retrieval and reporting purposes. When it serves an entire organization; such an environment is called a data warehouse; when it serves only part of the organization or a subject matter area in the organization, it is called a data mart. The data model of a data warehouse is designed and optimized mainly to support data retrieval needs. The model has intentional redundancy, fewer tables, and simpler relationships, ultimately resulting in simpler and more efficient queries as compared to an OLTP environment.

The process that pulls data from source systems (OLTP and others), manipulates it, and loads it into the data warehouse is called extract, transform, and load, or ETL.

Often the ETL process will involve the use of a data staging area (DSA) between the OLTP and the DW. The DSA usually resides in a relational database such as a SQL Server database and is used as the data cleansing area. The DSA is not open to end users.

**The Business Intelligence Semantic Model**

The Business Intelligence Semantic Model (BISM) is Microsoft’s latest model for supporting the entire BI stack of applications. The idea is to provide rich, flexible, efficient, and scalable analytical and reporting capabilities. Its architecture includes three layers:

 - the data model 
 - business logic and queries 
 - data access

The deployment of the model can be in an Analysis Services server or PowerPivot. With Analysis Services, you can use either a multidimensional data model or a tabular (relational) one. With PowerPivot, you use a tabular data model.

The business logic and queries use two languages: Multidimensional Expressions (MDX), based on multidimensional concepts, and Data Analysis Expressions (DAX), based on tabular concepts.

The data access layer can get its data from different sources: relational databases such as the DW, files, cloud services, line of business (LOB) applications, OData feeds, and others. The data access layer can either cache the data locally or just serve as a pass-through layer directly from the data sources.

BISM provides the user with answers to all possible questions, but the user’s task is to ask the right questions—to sift anomalies, trends, and other useful information from the sea of data.

**Data Mining**

Data mining (DM) is the next step; instead of letting the user look for useful information in the sea of data, data mining models can do this for the user. That is, data mining algorithms comb the data and sift the useful information from it. The language used to manage and query data mining models is Data Mining Extensions (DMX).

  [1]: https://i.stack.imgur.com/h4tDL.jpg

