---
title: "Programmation orientée objet en C#"
slug: "programmation-orientee-objet-en-c"
draft: false
images: []
weight: 9975
type: docs
toc: true
---

Ce sujet essaie de nous dire comment nous pouvons écrire des programmes basés sur l'approche OOP. Mais nous n'essayons pas d'enseigner le paradigme de la programmation orientée objet.
Nous couvrirons les sujets suivants :
Classes, propriétés, héritage, polymorphisme, interfaces, etc.

## Des classes:
Le squelette de la classe déclarante est :

<> : Obligatoire

[]:Optionnel

    [private/public/protected/internal] class <Desired Class Name> [:[Inherited class][,][[Interface Name 1],[Interface Name 2],...]
    {
        //Your code
    }
Ne vous inquiétez pas si vous ne comprenez pas toute la syntaxe, nous nous familiariserons avec toutes les parties de cela. Pour le premier exemple, considérez la classe suivante :

    class MyClass
    {
        int i = 100;
        public void getMyValue()
        {
            Console.WriteLine(this.i);//Will print number 100 in output
        }
    }

dans cette classe, nous créons la variable `i` avec le type `int` et avec la méthode privée par défaut [Modificateurs d'accès] (https://msdn.microsoft.com/en-us/library/ms173121.aspx) et `getMyValue()` avec modificateurs d'accès public.

