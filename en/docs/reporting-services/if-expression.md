---
title: "IF expression"
slug: "if-expression"
draft: false
images: []
weight: 9995
type: docs
toc: true
---

## If points field value is greater than 10 then display good else average
We have a set of data 

[![enter image description here][1]][1]

We would like to see

[![enter image description here][2]][2]

by using the following expression in the detail textbox. we can achieve the Remark  

     =IIF(Fields!Points.Value>=10,"Good","Average")


  [1]: http://i.stack.imgur.com/31Aja.png
  [2]: http://i.stack.imgur.com/vABWz.png

## Using IIF to Screen for Division by Zero
The IIF statement can be used in expressions to screen for division by zero:

    =IIF(Fields!PossibleZero.Value=0,0,Fields!Denominator.Value/IIF(Fields!PossibleZero.Value=0,1,Fields!PossibleZero.Value))

SSRS does not short circuit IIF arguments.  Therefore, using a single IIF function to screen for division by zero will have no effect and give an `#ERROR` value.

Instead, a pair of nested IIF statements can be used.  The outer IIF controls the value returned in the case of division by zero, 0 in the example above.  The inner IIF is a "dummy" value that prevents the engine from actually performing a division by zero in this case.

## AND / OR IF condition
Sometimes a complex IF condition is needed.

Let's take and example, Assuming we have the following raw data:

| ItemID  | Item Name |Item Status |
| ------ | ------ |-------|
| 1 | Item 1   |Tentative |
| 1 | Item 1   |Pending |
| 1 | Item 1   |Approved |

**The Goal is:**

Let's assume our business user ask to see which items are not approved and which are approved. `Tentative` and `Pending` items are considered as `Not Approved`.

**IF Condition Example:**

    =IIF((Fields!ItemStatus.Value = "Tentative") Or (Fields!ItemStatus.Value = "Pending")
    ,"Not Approved", "Approved")

**The results:**

[![enter image description here][1]][1]


  [1]: http://i.stack.imgur.com/7g6z1.png

