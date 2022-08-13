---
title: "Find Control by ID"
slug: "find-control-by-id"
draft: false
images: []
weight: 9984
type: docs
toc: true
---

## Syntax
 1. `control.FindControl("Id Of The Control To Be Found")`



 - `FindControl` is not recursive, it only searches through immediate children of the control
 - There is an overload `FindControl(String, int)` which is not indented for public usage
 - If nothing is found, `FindControl` returns `null`, so this is often a good idea to verify result for being not `null`

## Accessing the TextBox Control in aspx Page
    TextBox txt = (TextBox)FindControl(yourtxt_Id);

## Find a control in a GridView, Repeater, ListView etc.
If the Control has rows.

    TextBox tb = GridView1.Rows[i].FindControl("TextBox1") as TextBox;

Or if it has items.

    TextBox tb = Repeater1.Items[i].FindControl("TextBox1") as TextBox;

