---
title: "Using Dynamic and custom SQL query in Liferay"
slug: "using-dynamic-and-custom-sql-query-in-liferay"
draft: false
images: []
weight: 9998
type: docs
toc: true
---

There are scenarios when dealing with service layer in liferay, when we need to query database with too many clauses or dealing with multiple tables.In such cases,we use either of:

1)Dynamic query(wrapper on Hibernate criteria API)

2)Custom SQL queries

References:

 1. [Custom SQL][1]
 2. [Dynamic query][2]


  [1]: https://dev.liferay.com/develop/tutorials/-/knowledge_base/7-0/custom-sql
  [2]: https://dev.liferay.com/develop/tutorials/-/knowledge_base/7-0/dynamic-query

## Using Dynamic query in Liferay
For most of the scenarios involving entities from service layer,we can make do with the default service calls,with some help from the finders as well.For simple scenarios involving multiple entities,we move towards using Dynamic query API.This is a wrapper API for the Criteria API used in Hibernate.It can be used for cases,where we need to generate dynamic query,which is not very complex in nature,using several constructs from the API.
To start with,some of the most commonly used constructs are:
`DynamicQueryFactoryUtil`-Used for constructing query

`RestrictionsFactoryUtil`-Used for providing restrictions i.e.fields for comparison with a certain value to narrow down the results matching a certain value or within a range,etc

`ProjectionFactoryUtil`-Used for providing projections to get fields which will be part of search result i.e. instead of providing the whole entity,will provide only certain fields or apply aggregration function(such as min.max,avg) on the same.

`PropertyFactoryUtil`-Used for comparison of some property from the entity class to mostly do comparsion with other fields from a query

The implementation of these classes are present in dao.orm.jpa package with all the available methods


