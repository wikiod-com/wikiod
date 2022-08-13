---
title: "Pile et tas"
slug: "pile-et-tas"
draft: false
images: []
weight: 9987
type: docs
toc: true
---

Il convient de noter que lors de la déclaration d'un type de référence, sa valeur initiale sera "null". C'est parce qu'il ne pointe pas encore vers un emplacement en mémoire et qu'il s'agit d'un état parfaitement valide.
Cependant, à l'exception des types nullables, les types valeur doivent généralement toujours avoir une valeur.



## Types de valeur utilisés
Les types de valeur contiennent simplement une _**value**_.

Tous les types de valeur sont dérivés de la classe [System.ValueType][1], et cela inclut la plupart des types intégrés.

Lors de la création d'un nouveau type de valeur, une zone de mémoire appelée __*la pile*__ est utilisée.
La pile augmentera en conséquence, de la taille du type déclaré. Ainsi, par exemple, un int se verra toujours allouer 32 bits de mémoire sur la pile. Lorsque le type de valeur n'est plus dans la portée, l'espace sur la pile sera désalloué.

Le code ci-dessous illustre un type de valeur affecté à une nouvelle variable. Une structure est utilisée comme un moyen pratique de créer un type de valeur personnalisé (la classe System.ValueType ne peut pas être étendue autrement).

La chose importante à comprendre est que lors de l'attribution d'un type de valeur, la valeur elle-même _**copiée**_ dans la nouvelle variable, ce qui signifie que nous avons deux instances distinctes de l'objet, qui ne peuvent pas s'affecter.

    struct PersonAsValueType
    {
        public string Name;
    }

    class Program
    {
        static void Main()
        {
            PersonAsValueType personA;

            personA.Name = "Bob";

            var personB = personA;

            personA.Name = "Linda";

            Console.WriteLine(                // Outputs 'False' - because 
                object.ReferenceEquals(       // personA and personB are referencing 
                    personA,                  // different areas of memory
                    personB));                

            Console.WriteLine(personA.Name);  // Outputs 'Linda'
            Console.WriteLine(personB.Name);  // Outputs 'Bob'
        }
    }


[1] : https://msdn.microsoft.com/en-us/library/system.valuetype.aspx

## Types de référence utilisés
Les types de référence sont composés à la fois d'une _**référence**_ à une zone de mémoire et d'une _**valeur**_ stockée dans cette zone.
Ceci est analogue aux pointeurs en C/C++.

Tous les types de référence sont stockés sur ce qu'on appelle _**le tas**_.
Le tas est simplement une zone de mémoire gérée où les objets sont stockés. Lorsqu'un nouvel objet est instancié, une partie du tas sera allouée pour être utilisée par cet objet, et une référence à cet emplacement du tas sera renvoyée. Le tas est géré et maintenu par le _garbage collector_, et ne permet pas d'intervention manuelle.

En plus de l'espace mémoire requis pour l'instance elle-même, un espace supplémentaire est requis pour stocker la référence elle-même, ainsi que des informations temporaires supplémentaires requises par le CLR .NET.

Le code ci-dessous montre qu'un type de référence est affecté à une nouvelle variable. Dans ce cas, nous utilisons une classe, toutes les classes sont des types de référence (même statiques).

Lorsqu'un type de référence est affecté à une autre variable, c'est la _**référence**_ à l'objet qui est copié, __pas__ la valeur elle-même. Il s'agit d'une distinction importante entre les types valeur et les types référence.

Les implications de ceci sont que nous avons maintenant _deux_ références au même objet.
Toute modification des valeurs dans cet objet sera reflétée par les deux variables.

    class PersonAsReferenceType
    {
        public string Name;
    }

    class Program
    {
        static void Main()
        {
            PersonAsReferenceType personA;

            personA = new PersonAsReferenceType { Name = "Bob" };

            var personB = personA;

            personA.Name = "Linda";

            Console.WriteLine(               // Outputs 'True' - because
                object.ReferenceEquals(      // personA and personB are referencing 
                    personA,                 // the *same* memory location
                    personB));

            Console.WriteLine(personA.Name); // Outputs 'Linda'
            Console.WriteLine(personB.Name); // Outputs 'Linda'
        }

