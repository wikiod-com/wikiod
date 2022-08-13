---
title: "Script C#"
slug: "script-c"
draft: false
images: []
weight: 9945
type: docs
toc: true
---

## Avaliação de código simples
Você pode avaliar qualquer código C# válido:

    int value = await CSharpScript.EvaluateAsync<int>("15 * 89 + 95");
    var span = await CSharpScript.EvaluateAsync<TimeSpan>("new DateTime(2016,1,1) - DateTime.Now");

Se o tipo não for especificado, o resultado será `object`:

    object value = await CSharpScript.EvaluateAsync("15 * 89 + 95");

