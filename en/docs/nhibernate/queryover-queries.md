---
title: "QueryOver Queries"
slug: "queryover-queries"
draft: false
images: []
weight: 9988
type: docs
toc: true
---

NHibernate 3.0 introduced the QueryOver API, which combines the use of extension methods and lambda expressions to provide a statically typesafe wrapper around the ICriteria API.  The ICriteria API is NHibernate's implementation of the [Query Object][1] pattern. 

  [1]: http://martinfowler.com/eaaCatalog/queryObject.html

## Basic query
A basic `QueryOver` query is performed against an `ISession` using the `QueryOver<T>` method, where `T` is the type of a mapped entity.

    IList<Customer> customers = session.QueryOver<Customer>()
        .Where(c => c.LastName == "Simpson")
        .List();

## Query with join using JoinQueryOver
To join and and for instance filter on the joined table use `JoinQueryOver`.

    IList<Customer> customers = session.QueryOver<Customer>()
        .Inner.JoinQueryOver(x => x.Organisation)
        .Where(y => y.Name == "Acme Inc")
        .List();

## Query with join using JoinAlias
It's possible to use JoinAlias method to join several tables. It's useful when it's needed to specify some property from the joined table in the select statement:

    Customer customerAlias = null;
    Organization organizationAlias = null;

    IList<Customer> customers = session.QueryOver(() => customerAlias)
        .Left.JoinAlias(x => x.Organization, () => organizationAlias)
        .Where(customer => customer.Name == "Customer Name")
        .And(() => customerAlias.Age > 18)
        .AndNot(() => organizationAlias.Name == "Forbidden Organization")
        .List();

