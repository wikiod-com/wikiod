---
title: "Where not in multiple columns"
slug: "where-not-in-multiple-columns"
draft: false
images: []
weight: 9994
type: docs
toc: true
---

## Filter out multiple combinations of values
Lets say you want to filter a query by two columns, but only certain combinations of those columns.  For example, it's OK to have account 60400 with reference JE, but you cannot have account 60400 with reference ED, but you can have account 60500 with reference ED.  

    select * from schema.table where (acct, ref) not in
    ( values
      (60400, 'ED'),
      (60600, 'ED'),
      (60701, 'ED'),
      (70400, 'ED'),
      (70500, 'ED'),
      (70600, 'ED'),
      (80800, 'ED')
    );

