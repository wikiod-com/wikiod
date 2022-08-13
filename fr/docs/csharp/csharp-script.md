---
title: "Script C#"
slug: "script-c"
draft: false
images: []
weight: 9945
type: docs
toc: true
---

## Évaluation simple du code
Vous pouvez évaluer n'importe quel code C# valide :

    int value = await CSharpScript.EvaluateAsync<int>("15 * 89 + 95");
    var span = await CSharpScript.EvaluateAsync<TimeSpan>("new DateTime(2016,1,1) - DateTime.Now");

Si le type n'est pas spécifié, le résultat est `object` :

    object value = await CSharpScript.EvaluateAsync("15 * 89 + 95");

