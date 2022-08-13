---
title: "Naming Conventions"
slug: "naming-conventions"
draft: false
images: []
weight: 9995
type: docs
toc: true
---

## Functions
    Get-User()

 - Use *Verb-Noun* pattern while naming a function. 
 - Verb implies an action e.g. `Get`, `Set`, `New`, `Read`, `Write` and many more. See [approved verbs][1].
 - Noun should be singular even if it acts on multiple items. `Get-User()` may return one or multiple users.
 - Use Pascal case for both Verb and Noun. E.g. `Get-UserLogin()`

  [1]: https://msdn.microsoft.com/en-us/library/ms714428(v=vs.85).aspx

