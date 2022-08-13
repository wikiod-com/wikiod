---
title: "How to invoke a private method dynamically"
slug: "how-to-invoke-a-private-method-dynamically"
draft: false
images: []
weight: 9993
type: docs
toc: true
---

Use of `<cfinvoke>` or `invoke()` should be faster than `evaluate()`

## CFML
    <cfinvoke method="#somePrivateMethodName#">
      <cfinvokeargument name="argument1" value="one">
    </cfinvoke>

## CFSCRIPT (CF10+)
    invoke("", somePrivateMethodName, {argument1='one'});

