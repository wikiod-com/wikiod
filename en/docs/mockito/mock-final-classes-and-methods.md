---
title: "Mock final classes and methods"
slug: "mock-final-classes-and-methods"
draft: false
images: []
weight: 9998
type: docs
toc: true
---

Since Mockito 2.x we have the ability to mock final classes and methods.

## How to make it
Steps:

 1. Add dependency to **Mockito** version 2.x in your `gradle` (at the time of writing this text the latest version is **2.7.22**):

        testCompile "org.mockito:mockito-core:$versions.mockito"

 2. Create a file in test resources with name `org.mockito.plugins.MockMaker`:
[![tree structure][1]][1]

 3. Add next line in this file:

        mock-maker-inline

  [1]: https://i.stack.imgur.com/MzTXL.png


