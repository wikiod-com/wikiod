---
title: "JUnit"
slug: "junit"
draft: false
images: []
weight: 9987
type: docs
toc: true
---

## Rules
To add a JUnit [rule][1] to a test fixture:

    @Rule @JvmField val myRule = TemporaryFolder()

The `@JvmField` annotation is necessary to expose the backing field with the same visibility (public) as the `myRule` property (see [answer][2]). JUnit rules require the annotated rule field to be public.


  [1]: https://github.com/junit-team/junit4/wiki/rules
  [2]: http://stackoverflow.com/questions/32899947/kotlin-junit-rules

