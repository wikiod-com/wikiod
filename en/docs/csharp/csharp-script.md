---
title: "C# Script"
slug: "c-script"
draft: false
images: []
weight: 9945
type: docs
toc: true
---

## Simple code evaluation
You can evaluate any valid C# code:

    int value = await CSharpScript.EvaluateAsync<int>("15 * 89 + 95");
    var span = await CSharpScript.EvaluateAsync<TimeSpan>("new DateTime(2016,1,1) - DateTime.Now");

If type is not specified, the result is `object`:

    object value = await CSharpScript.EvaluateAsync("15 * 89 + 95");

