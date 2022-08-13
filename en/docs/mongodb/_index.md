---
title : MongoDB Tutorial
slug : mongodb-tutorial
weight : 9731
draft : false
images : []
type : docs
---

- Data in the world started to grow tremendously after mobile application came in the market. This huge amount of data became almost impossible to handle with traditional relational database - SQL. NoSQL databases are introduced to handle those data where much more flexibility came like variable number of columns for each data.
- MongoDB is one of the leading NoSQL databases. Each collection contains a number of JSON documents. Any data model that can be expressed in a JSON document can be easily stored in MongoDB.
- MongoDB is a server-client database. Server usually runs with the binary file `mongod` and client runs with `mongo`.
- There is no join operation in MongoDB prior to v.3.2, [for various philosophical and pragmatic reasons][1]. But Mongo shell supports javascript, so if $lookup is not available, one can simulate join operations on documents in javascript before inserting.
- To run an instance in production environment, it's strongly advised to follow the [Operations Checklist][1].


[1]: https://www.mongodb.com/blog/post/joins-and-other-aggregation-enhancements-coming-in-mongodb-3-2-part-1-of-3-introduction
[2]: https://docs.mongodb.com/manual/administration/production-checklist-operations/

