---
title: "C# Komut Dosyası"
slug: "c-komut-dosyas"
draft: false
images: []
weight: 9945
type: docs
toc: true
---

## Basit kod değerlendirmesi
Herhangi bir geçerli C# kodunu değerlendirebilirsiniz:

    int value = await CSharpScript.EvaluateAsync<int>("15 * 89 + 95");
    var span = await CSharpScript.EvaluateAsync<TimeSpan>("new DateTime(2016,1,1) - DateTime.Now");

Tür belirtilmezse, sonuç "nesne" olur:

    object value = await CSharpScript.EvaluateAsync("15 * 89 + 95");

