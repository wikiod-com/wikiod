---
title: "Insert into one table by selecting from another table"
slug: "insert-into-one-table-by-selecting-from-another-table"
draft: false
images: []
weight: 9995
type: docs
toc: true
---

## Insert into one table by selecting from another table
    insert into schema.table (field1, field2)
      select 'Static Value', foreignField from schema.otherTable;

