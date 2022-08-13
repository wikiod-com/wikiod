---
title: "Manejo de valores nulos"
slug: "manejo-de-valores-nulos"
draft: false
images: []
weight: 9972
type: docs
toc: true
---

## nulo frente a DBNulo
En ADO.NET, manejar correctamente `null` es una fuente constante de confusión. El punto clave en dapper es que *no tienes que hacerlo*; se ocupa de todo internamente.

- Los valores de parámetro que son `null` se envían correctamente como `DBNull.Value`
- los valores leídos que son "nulos" se presentan como "nulos" o (en el caso de la asignación a un tipo conocido) simplemente se ignoran (dejando su valor predeterminado basado en el tipo)

Simplemente funciona:

    string name = null;
    int id = 123;
    connection.Execute("update Customer set Name=@name where Id=@id",
        new {id, name});

