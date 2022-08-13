---
title: "HQL"
slug: "hql"
draft: false
images: []
weight: 9987
type: docs
toc: true
---

HQL is Hibernate Query Language, it based on SQL and behind the scenes it is changed into SQL but the syntax is different. You use entity/class names not table names and field names not column names. It also allows many shorthands. 


The main thing to remember when using hql is the use the class name and field names instead of the table and column names we are used to in SQL.

## Selecting a whole table
    hql = "From EntityName";

## Select specific columns
    hql = "Select id, name From Employee";

## Include a Where clause
    hql = "From Employee where id = 22";

## Join
    hql = "From Author a, Book b Where a.id = book.author";

