---
title: "Classe partielle et méthodes"
slug: "classe-partielle-et-methodes"
draft: false
images: []
weight: 9970
type: docs
toc: true
---

Les classes partielles nous offrent la possibilité de diviser les classes en plusieurs parties et dans plusieurs fichiers source. Toutes les parties sont combinées en une seule classe au moment de la compilation. Toutes les parties doivent contenir le mot-clé "partiel", doivent avoir la même accessibilité. Toutes les pièces doivent être présentes dans le même assemblage pour qu'il soit inclus au moment de la compilation.

## Syntaxe
- classe publique **partielle** MyPartialClass { }

- Les classes partielles doivent être définies dans le même assembly et le même espace de noms que la classe qu'elles étendent.

- Toutes les parties de la classe doivent utiliser le mot clé `partial`.

- Toutes les parties de la classe doivent avoir la même accessibilité ; `public`/`protégé`/`privé` etc..

- Si une partie utilise le mot clé `abstract`, alors le type combiné est considéré comme abstrait.

- Si une partie utilise le mot clé `sealed`, alors le type combiné est considéré comme scellé.

- Si une partie utilise un type de base, le type combiné hérite de ce type.

- Le type combiné hérite de toutes les interfaces définies sur toutes les classes partielles.

## Cours partiels
Les classes partielles offrent la possibilité de diviser la déclaration de classe (généralement en fichiers séparés). Un problème courant qui peut être résolu avec des classes partielles est de permettre aux utilisateurs de modifier le code généré automatiquement sans craindre que leurs modifications soient écrasées si le code est régénéré. De plus, plusieurs développeurs peuvent travailler sur la même classe ou les mêmes méthodes.

    using System;
    
    namespace PartialClassAndMethods
    {
        public partial class PartialClass
        {
            public void ExampleMethod() {
                Console.WriteLine("Method call from the first declaration.");
            }
        }
    
        public partial class PartialClass
        {
            public void AnotherExampleMethod()
            {
                Console.WriteLine("Method call from the second declaration.");
            }
        }
    
        class Program
        {
            static void Main(string[] args)
            {
                PartialClass partial = new PartialClass();
                partial.ExampleMethod(); // outputs "Method call from the first declaration."
                partial.AnotherExampleMethod(); // outputs "Method call from the second declaration."
            }
        }
    }

## Méthodes partielles
La méthode partielle consiste en la définition dans une déclaration de classe partielle (en tant que scénario commun - dans le scénario généré automatiquement) et l'implémentation dans une autre déclaration de classe partielle.

    using System;
    
    namespace PartialClassAndMethods
    {
        public partial class PartialClass // Auto-generated
        {
            partial void PartialMethod();
        }
    
        public partial class PartialClass // Human-written
        {
            public void PartialMethod()
            {
                Console.WriteLine("Partial method called.");
            }
        }
    
        class Program
        {
            static void Main(string[] args)
            {
                PartialClass partial = new PartialClass();
                partial.PartialMethod(); // outputs "Partial method called."
            }
        }
    }

## Classes partielles héritant d'une classe de base
Lors de l'héritage d'une classe de base, une seule classe partielle doit avoir la classe de base spécifiée.

    // PartialClass1.cs
    public partial class PartialClass : BaseClass {}

    // PartialClass2.cs
    public partial class PartialClass {}

Vous *pouvez* spécifier la *même* classe de base dans plus d'une classe partielle. Il sera signalé comme redondant par certains outils IDE, mais il se compile correctement.

    // PartialClass1.cs
    public partial class PartialClass : BaseClass {}

    // PartialClass2.cs
    public partial class PartialClass : BaseClass {} // base class here is redundant

Vous *ne pouvez pas* spécifier des classes de base *différentes* dans plusieurs classes partielles, cela entraînera une erreur du compilateur.

    // PartialClass1.cs
    public partial class PartialClass : BaseClass {} // compiler error

    // PartialClass2.cs
    public partial class PartialClass : OtherBaseClass {} // compiler error

