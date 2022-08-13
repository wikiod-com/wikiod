---
title : doctrine2 Tutorial
slug : doctrine2-tutorial
weight : 9971
draft : false
images : []
type : docs
---

Doctrine 2 is the colloquial term used for the [Object Relational Mapper(ORM)](https://en.wikipedia.org/wiki/Object-relational_mapping) component of the [Doctrine Project](http://www.doctrine-project.org/index.html).

The ORM sits on top of the [Doctrine Project's Database Abstraction Layer (DBAL)](http://docs.doctrine-project.org/projects/doctrine-dbal/en/latest/) providing ways to query and manipulate information in the underlying database using Doctrine's [DSL](https://en.wikipedia.org/wiki/Domain-specific_language) called DQL.

Using the ORM provides developers with many advantages over direct access to the database:

* DBAL abstracts over many platforms allowing use of the same ORM code with many databases platforms. (MySQL, PgSQL, Sqlite, Oracle)
* Mapping database structures to [domain models](https://en.wikipedia.org/wiki/Domain_model) allows for [separation of concerns](https://en.wikipedia.org/wiki/Separation_of_concerns).
* Support for advanced [caching](http://doctrine-orm.readthedocs.io/en/latest/reference/caching.html) techniques across many platforms (APC, Redis, etc.)

Moreover Doctrine 2 offers integrations with many popular PHP web frameworks ([Symfony](https://github.com/doctrine/DoctrineBundle), [Zend](https://github.com/doctrine/DoctrineORMModule), [Laravel](http://www.laraveldoctrine.org/)) which make difficulty of setup low.

