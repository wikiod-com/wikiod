---
title: "Sorting incoming data, but send forward only a subset of rows"
slug: "sorting-incoming-data-but-send-forward-only-a-subset-of-rows"
draft: false
images: []
weight: 9997
type: docs
toc: true
---

## Using Sort and Conditional Split components
Since you need to sort & rename the fields, the best option will be the Sort Component in the Data Flow task (like you mentioned) . If you only want to rename columns, then use the "Derived Column" component. The Sort component should look as follows:
[![enter image description here][1]][1]
In my example, you can see the LastName, FirstName & BirthDate are sorted and LastName & BirthDate are renamed.

To return a subset of rows, you should use the Conditional Split component.
[![enter image description here][2]][2]
In my example, only rows that have Surnames (LastNames) that start with "D" would be returned.


The Data flow task should look like this:

[![enter image description here][3]][3]

**Note** : Sorting operation can be done in the database using SQL scripts (stored procedure) so it is advised to use SQL for better performance. 

  [1]: https://i.stack.imgur.com/XNENr.png
  [2]: https://i.stack.imgur.com/yq4tv.png
  [3]: https://i.stack.imgur.com/bO3BV.png

