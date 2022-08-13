---
title: "Self-Referencing tables"
slug: "self-referencing-tables"
draft: false
images: []
weight: 9986
type: docs
toc: true
---

In the example above, a reference field (SupID) can be used to indicate the ID of that employee's supervisor.  

Using something as simple as a DLOOKUP can return the name of that supervisor.  
eg.  `DLOOKUP("Name","EmployeeTable", "ID = " & SupID)`


----------

Another good example of this is to look at how automated Access switchboards are created, and more specifically the construct of the Switchboard table.  Each switchboard option refers to another option within the same table - similar to how this example self references.

## Self Referencing Employee Table
[![enter image description here][1]][1]


  [1]: https://i.stack.imgur.com/kQ54Z.png

