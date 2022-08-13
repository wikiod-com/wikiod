---
title : hibernate Tutorial
slug : hibernate-tutorial
weight : 9884
draft : false
images : []
type : docs
---

The `SessionFactory` bean is responsible for creating, maintaining, closing and flushing all the database sessions that the `TransactionManager` asks it to create. That's why we autowire the `SessionFactory` into DAO's and make run all queries through it. 

One of the biggest questions that new Hibernate users ask is "When do my changes get committed?" and the answer makes sense when you think how the `TransactionManager` works with the `SesisonFactory`. Your database changes will be flushed and committed when you exit the service method that was annotated with `@Transactional`. The reason for this is, that a transaction is supposed to represent a single 'unit' of unbroken work. If something goes wrong with the unit, then it is assumed that the unit failed and all changes should be rolled back. So the `SessionFactory` will flush and clear the session when you exit the service method that you called originally. 

That's not to say that it won't also flush and clear the session while your transaction is going on. For example, if I call a service method to add a collection of 5 objects and return the total count of objects in the database, the `SessionFactory` would realise that the query (`SELECT COUNT(*)`) requires an updated state to be accurate, and so would flush the addition of the 5 objects before running the count query. The execution could look something like this:

