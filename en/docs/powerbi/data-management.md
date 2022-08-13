---
title: "Data Management"
slug: "data-management"
draft: false
images: []
weight: 9992
type: docs
toc: true
---

## Filtering  data by dynamic criteria
For example, we need to create two visualisations, "Before" and "After", and use dynamic filter for the date of the split.

 - Let's say our query is called `table`.

 - Add an additional `date` table with possible dates of the split. 

 - Add a slicer control with the table added in the previous step.

 - Include a measure of this form to the `table`:  
`IsBefore = IF((Max('table'[Date])<Min('Date'[Date])),1,0)`

 - Add two visuals, filter first by `IsBefore = 1` and second - by `IsBefore = 0`

[![enter image description here][1]][1]


  [1]: http://i.stack.imgur.com/ur5CE.png

