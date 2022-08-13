---
title: "Comments"
slug: "comments"
draft: false
images: []
weight: 9982
type: docs
toc: true
---

## Single-line comments
Single line comments are preceded by `--`, and go until the end of the line:

    SELECT *
    FROM Employees -- this is a comment
    WHERE FName = 'John'

## Multi-line comments
Multi-line code comments are wrapped in `/* ... */`:

    /* This query
       returns all employees */
    SELECT *
    FROM Employees

It is also possible to insert such a comment into the middle of a line:

    SELECT /* all columns: */ *
    FROM Employees

