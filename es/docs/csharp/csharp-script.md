---
title: "Guión C#"
slug: "guion-c"
draft: false
images: []
weight: 9945
type: docs
toc: true
---

## Evaluación de código simple
Puede evaluar cualquier código C# válido:

    int value = await CSharpScript.EvaluateAsync<int>("15 * 89 + 95");
    var span = await CSharpScript.EvaluateAsync<TimeSpan>("new DateTime(2016,1,1) - DateTime.Now");

Si no se especifica el tipo, el resultado es `objeto`:

    object value = await CSharpScript.EvaluateAsync("15 * 89 + 95");

