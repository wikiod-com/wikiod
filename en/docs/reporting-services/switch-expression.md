---
title: "Switch Expression"
slug: "switch-expression"
draft: false
images: []
weight: 9997
type: docs
toc: true
---

## Changing text color using switch condition
Let's assume we have an entity status field with 3 options

 - Tentative
 - Pending
 - Approved

Our goal is to show different color for each status as follow:
Tentative will be Red
Pending will be Orange
Approved will be Green

**The switch condition:**

    =Switch(Fields!ItemStatus.Value = "Tentative","Red",
    Fields!ItemStatus.Value = "Pending", "Orange",
    Fields!ItemStatus.Value = "Approved", "Green")

**The pattern is:**

    =Switch([Condition statement] , [Value if True],
    [Condition statement] , [Value if True],
    [Condition statement] , [Value if True])

The second `,` sign starts a new condition. No `,` sign is needed for the last condition.

**Results:**

[![enter image description here][1]][1]


  [1]: http://i.stack.imgur.com/9m32r.png

