---
title: "Delimiting keywords or special characters"
slug: "delimiting-keywords-or-special-characters"
draft: false
images: []
weight: 9992
type: docs
toc: true
---

## Delimiting table or column name which is a reserved word as well
Say you have a table named table or you want to create a table with name which is also a keyword, You have to include the name table in pair of double quotes "table"

Select * from table;
Above query will fail with syntax error, where as below query will run fine.

Select * from "table";

## Delimit the table or column name with special characters
Select * from firm's_address;

Select * from "firm's_address";


