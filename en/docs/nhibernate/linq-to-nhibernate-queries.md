---
title: "LINQ to NHibernate Queries"
slug: "linq-to-nhibernate-queries"
draft: false
images: []
weight: 9991
type: docs
toc: true
---

The LINQ to NHibernate driver is centered on the `IQueryable<T>` interface.

Be sure to add `using NHibernate.Linq;` in order to use the NHibernate LINQ provider.

## Basic query
    IQueryable<Cat> cats = session.Query<Cat>()
        .Where(c => c.Name == "Max");


