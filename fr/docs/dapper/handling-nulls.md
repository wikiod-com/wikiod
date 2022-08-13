---
title: "Gestion des valeurs nulles"
slug: "gestion-des-valeurs-nulles"
draft: false
images: []
weight: 9972
type: docs
toc: true
---

## null contre DBNull
Dans ADO.NET, la gestion correcte de "null" est une source constante de confusion. Le point clé de dapper est que * vous n'êtes pas obligé de le faire *; il s'occupe de tout en interne.

- les valeurs de paramètre qui sont `null` sont correctement envoyées en tant que `DBNull.Value`
- les valeurs lues qui sont `null` sont présentées comme `null`, ou (dans le cas d'un mappage à un type connu) simplement ignorées (en laissant leur valeur par défaut basée sur le type)

Cela fonctionne simplement :

    string name = null;
    int id = 123;
    connection.Execute("update Customer set Name=@name where Id=@id",
        new {id, name});

