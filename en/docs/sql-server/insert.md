---
title: "Insert"
slug: "insert"
draft: false
images: []
weight: 9991
type: docs
toc: true
---

## Add a row to a table named Invoices

    INSERT INTO Invoices [ /* column names may go here */ ]
    VALUES (123, '1234abc', '2016-08-05 20:18:25.770', 321, 5, '2016-08-04');

* Column names are required if the table you are inserting into contains a column with the IDENTITY attribute.


    INSERT INTO Invoices ([ID], [Num], [DateTime], [Total], [Term], [DueDate])
    VALUES (123, '1234abc', '2016-08-05 20:18:25.770', 321, 5, '2016-08-25');

