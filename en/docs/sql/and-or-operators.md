---
title: "AND & OR Operators"
slug: "and--or-operators"
draft: false
images: []
weight: 9981
type: docs
toc: true
---

## Syntax
 1. SELECT * FROM table WHERE (condition1) AND (condition2);

 2. SELECT * FROM table WHERE (condition1) OR (condition2);

## AND OR Example
Have a table

| Name| Age|City|
| ------ | ------ | ------ |
| Bob| 10|Paris|
| Mat| 20|Berlin|
| Mary| 24|Prague|

    select Name from table where Age>10 AND City='Prague'

Gives

|Name|
| ------ |
|Mary|


----------

    select Name from table where Age=10 OR City='Prague'


Gives

|Name|
| ------ |
|Bob|
|Mary|





