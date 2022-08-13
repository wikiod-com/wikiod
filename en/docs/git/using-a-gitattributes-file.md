---
title: "Using a .gitattributes file"
slug: "using-a-gitattributes-file"
draft: false
images: []
weight: 9953
type: docs
toc: true
---

## Automatic Line Ending Normalization
Create a `.gitattributes` file in the project root containing:

    * text=auto

This will result in all text files (as identified by Git) being committed with LF, but checked out according to the host operating system default.

This is equivalent to the recommended `core.autocrlf` defaults of:

* `input` on Linux/macOS
* `true` on Windows

## Identify Binary Files
Git is pretty good at identifying binary files, but you can explicitly specify which files are binary. Create a `.gitattributes` file in the project root containing:

    *.png binary

`binary` is a built-in macro attribute equivalent to `-diff -merge -text`.

## Disable Line Ending Normalization
Create a `.gitattributes` file in the project root containing:

    * -text

This is equivalent to setting `core.autocrlf = false`.

## Prefilled .gitattribute Templates
If you are unsure which rules to list in your `.gitattributes` file, or you just want to add generally accepted attributes to your project, you can shoose or generate a `.gitattributes` file at:

 - https://gitattributes.io/
 - https://github.com/alexkaratarakis/gitattributes

