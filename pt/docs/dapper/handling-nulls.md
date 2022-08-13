---
title: "Manipulando Nulos"
slug: "manipulando-nulos"
draft: false
images: []
weight: 9972
type: docs
toc: true
---

## null vs DBNull
No ADO.NET, manipular corretamente `null` é uma fonte constante de confusão. O ponto chave no dapper é que *você não precisa*; ele lida com tudo isso internamente.

- valores de parâmetro que são `null` são enviados corretamente como `DBNull.Value`
- valores lidos que são `null` são apresentados como `null`, ou (no caso de mapeamento para um tipo conhecido) simplesmente ignorados (deixando seu padrão baseado em tipo)

Simplesmente funciona:

    string name = null;
    int id = 123;
    connection.Execute("update Customer set Name=@name where Id=@id",
        new {id, name});

