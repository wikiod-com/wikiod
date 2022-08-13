---
title: "Shared variables"
slug: "shared-variables"
draft: false
images: []
weight: 9992
type: docs
toc: true
---

## Syntax
 - Shared NumberVar x;
 - Shared StringVar y := "Hello, World!";



Shared variables allow values to be used at any point in the processing of the report. Similar to a global variable, the shared values can also be accessed by subreports. This allows for a more direct method of comparison and allows the developer to circumvent the strict nature of the top-to-bottom report evaluation.

A shared variable can be defined, set, or recalled in any valid formula field. Their most common use is to store information that would otherwise be forgotten or inaccessible at a later stage in the report.

## Basic implementation
Define the variables within a formula field:

    Shared NumberVar x := 1000;
    Shared NumberVar y;

Assigning the values is optional. To display the variable in a second formula later on in the report, the call is nearly identical:

    Shared NumberVar x;
    x

