---
title : jpa Tutorial
slug : jpa-tutorial
weight : 9934
draft : false
images : []
type : docs
---

JPA is the Java Persistence API, a specification handling the mapping of Java objects and their relationships to a relational database. This is called an object-relational mapper (ORM). It is an alternative for (or supplement to) the more low-level [JDBC][1]. It is most useful when pursuing a Java-oriented approach and when complex object graphs need to be persisted.

JPA in itself is not an implementation. You will need a persistence provider for that (see examples). Current implementations of the latest JPA 2.1 standard are 
[EclipseLink][2] (also the reference implementation for JPA 2.1, which means "proof that the spec can be implemented"); [Hibernate][3], and [DataNucleus][4].

## Metadata
The mapping between Java objects and database tables is defined via **persistence metadata**. The JPA provider will use the persistence metadata information to perform the correct database operations. JPA typically defines the metadata via annotations in the Java class.

## Object-Relational Entity Architecture
The entity architecture is composed of: 
- entities
- persistence units
- persistence contexts
- entity manager factories
- entity managers


  [1]: https://www.wikiod.com/jdbc/getting-started-with-jdbc
  [2]: https://www.eclipse.org/eclipselink/
  [3]: http://hibernate.org/
  [4]: http://www.datanucleus.org

