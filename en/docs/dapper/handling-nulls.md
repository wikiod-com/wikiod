---
title: "Handling Nulls"
slug: "handling-nulls"
draft: false
images: []
weight: 9972
type: docs
toc: true
---

## null vs DBNull
In ADO.NET, correctly handling `null` is a constant source of confusion. The key point in dapper is that *you don't have to*; it deals with it all internally.

- parameter values that are `null` are correctly sent as `DBNull.Value`
- values read that are `null` are presented as `null`, or (in the case of mapping to a known type) simply ignored (leaving their type-based default)

It just works:

    string name = null;
    int id = 123;
    connection.Execute("update Customer set Name=@name where Id=@id",
        new {id, name});

