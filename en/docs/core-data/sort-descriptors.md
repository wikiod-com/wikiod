---
title: "Sort Descriptors"
slug: "sort-descriptors"
draft: false
images: []
weight: 9997
type: docs
toc: true
---

## Ordering Data Returned By Fetch Requests
Set the NSFetchRequest property sortDescriptors to determine how data is returned.

    let fetchRequest = NSFetchRequest(entityName: "NAME_OF_ENTITY")
    let sortDescriptor = NSSortDescriptor(key: "NAME_OF_ATTRIBUTE", ascending: true)
    fetchRequest.sortDescriptors = [sortDescriptor]

## Multiple Sort Descriptors
You can also set multiple sort descriptors, to sort by one attribute within another. For example, return all entries ordered by date, and by name within each date:

    let fetchRequest = NSFetchRequest(entityName: "NAME_OF_ENTITY")
    let sortDescriptor1 = NSSortDescriptor(key: "name", ascending: true)
    let sortDescriptor2 = NSSortDescriptor(key: "date", ascending: true)
    fetchRequest.sortDescriptors = [sortDescriptor1, sortDescriptor2]

