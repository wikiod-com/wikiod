---
title: "Type de valeur vs type de référence"
slug: "type-de-valeur-vs-type-de-reference"
draft: false
images: []
weight: 9929
type: docs
toc: true
---

## Syntaxe
* Passage par référence : public void Double(ref int numberToDouble) { }

## Introduction

### Types de valeur

Les types valeur sont les plus simples des deux. Les types valeur sont souvent utilisés pour représenter les données elles-mêmes. Un entier, un booléen ou un point dans l'espace 3D sont tous des exemples de bons types de valeurs.

Les types de valeur (structs) sont déclarés à l'aide du mot-clé struct. Voir la section syntaxe pour un exemple de déclaration d'une nouvelle structure.

D'une manière générale, nous avons 2 mots-clés qui sont utilisés pour déclarer des types de valeur :

- Structures
- Énumérations

### Types de référence

Les types de référence sont légèrement plus complexes. Les types de référence sont des objets traditionnels au sens de la programmation orientée objet. Ainsi, ils prennent en charge l'héritage (et ses avantages) et prennent également en charge les finaliseurs.

En C#, nous avons généralement ces types de référence :
- Des classes
- Délégués
- Interfaces

Les nouveaux types de référence (classes) sont déclarés à l'aide du mot-clé class. Pour un exemple, consultez la section sur la syntaxe pour savoir comment déclarer un nouveau type de référence.

## Différences majeures

Les principales différences entre les types de référence et les types de valeur peuvent être vues ci-dessous.

### Les types de valeur existent sur la pile, les types de référence existent sur le tas

C'est la différence souvent mentionnée entre les deux, mais en réalité, cela revient à dire que lorsque vous utilisez un type valeur en C#, tel qu'un int, le programme utilisera cette variable pour se référer directement à cette valeur. Si vous dites int mine = 0, alors la variable mine fait directement référence à 0, ce qui est efficace. Cependant, les types de référence contiennent en fait (comme leur nom l'indique) une référence à l'objet sous-jacent, ce qui s'apparente aux pointeurs dans d'autres langages tels que C++.

Vous ne remarquerez peut-être pas les effets de cela immédiatement, mais les effets sont là, puissants et subtils. Voir l'exemple sur la modification des types de référence ailleurs pour un exemple.

Cette différence est la principale raison des autres différences suivantes et mérite d'être connue.

### Les types de valeur ne changent pas lorsque vous les modifiez dans une méthode, les types de référence le font

Lorsqu'un type valeur est passé dans une méthode en tant que paramètre, si la méthode modifie la valeur de quelque manière que ce soit, la valeur n'est pas modifiée. En revanche, passer un type référence dans cette même méthode et le modifier modifiera l'objet sous-jacent, de sorte que d'autres choses qui utilisent ce même objet auront l'objet nouvellement modifié plutôt que leur valeur d'origine.

Voir l'exemple des types de valeur par rapport aux types de référence dans les méthodes pour plus d'informations.

#### Et si je veux les changer ?

Passez-les simplement dans votre méthode à l'aide du mot-clé "ref", et vous passerez alors cet objet par référence. Cela signifie que c'est le même objet en mémoire. Ainsi les modifications que vous apporterez seront respectées. Voir l'exemple sur le passage par référence pour un exemple.

### Les types de valeur ne peuvent pas être nuls, les types de référence peuvent

À peu près comme il est dit, vous pouvez affecter null à un type de référence, ce qui signifie que la variable que vous avez affectée ne peut avoir aucun objet réel qui lui soit affecté. Dans le cas des types de valeur, cependant, cela n'est pas possible. Vous pouvez cependant utiliser Nullable<Type>, pour permettre à votre type de valeur d'être nullable, si c'est une exigence, bien que si c'est quelque chose que vous envisagez, réfléchissez bien si une classe n'est peut-être pas la meilleure approche ici, si elle est votre propre type.

## Passage par référence à l'aide du mot-clé ref.

De la [documentation][1] :


> En C#, les arguments peuvent être passés aux paramètres soit par valeur, soit par
> référence. Le passage par référence permet aux membres de la fonction, aux méthodes,
> propriétés, indexeurs, opérateurs et constructeurs pour modifier la valeur
> des paramètres et que ce changement persiste dans l'appel
> environnement. Pour passer un paramètre par référence, utilisez le `ref` ou `out`
> mot clé.

La différence entre `ref` et `out` est que `out` signifie que le paramètre passé doit être assigné avant que la fonction ne se termine. En revanche, les paramètres passés avec `ref` peuvent être modifiés ou laissés inchangés.


    using System;

    class Program
    {
        static void Main(string[] args)
        {
            int a = 20;
            Console.WriteLine("Inside Main - Before Callee: a = {0}", a);
            Callee(a);
            Console.WriteLine("Inside Main - After Callee: a = {0}", a);
            
            Console.WriteLine("Inside Main - Before CalleeRef: a = {0}", a);
            CalleeRef(ref a);
            Console.WriteLine("Inside Main - After CalleeRef: a = {0}", a);
         
            Console.WriteLine("Inside Main - Before CalleeOut: a = {0}", a);
            CalleeOut(out a);
            Console.WriteLine("Inside Main - After CalleeOut: a = {0}", a);
            
            Console.ReadLine();
        }
    
        static void Callee(int a)
        {
            a = 5;
            Console.WriteLine("Inside Callee a : {0}", a);
        }
    
        static void CalleeRef(ref int a)
        {
            a = 6;
            Console.WriteLine("Inside CalleeRef a : {0}", a);
        }
        
        static void CalleeOut(out int a)
        {
            a = 7;
            Console.WriteLine("Inside CalleeOut a : {0}", a);
        }
    }

**Production** :

    Inside Main - Before Callee: a = 20
    Inside Callee a : 5
    Inside Main - After Callee: a = 20
    Inside Main - Before CalleeRef: a = 20
    Inside CalleeRef a : 6
    Inside Main - After CalleeRef: a = 6
    Inside Main - Before CalleeOut: a = 6
    Inside CalleeOut a : 7
    Inside Main - After CalleeOut: a = 7

[1] : https://msdn.microsoft.com/en-IN/library/0f66670z.aspx


## Modification des valeurs ailleurs
<!-- language-all : c# -->
```
public static void Main(string[] args)
{
    var studentList = new List<Student>();
    studentList.Add(new Student("Scott", "Nuke"));
    studentList.Add(new Student("Vincent", "King"));
    studentList.Add(new Student("Craig", "Bertt"));

    // make a separate list to print out later
    var printingList = studentList; // this is a new list object, but holding the same student objects inside it

    // oops, we've noticed typos in the names, so we fix those
    studentList[0].LastName = "Duke";
    studentList[1].LastName = "Kong";
    studentList[2].LastName = "Brett";

    // okay, we now print the list
    PrintPrintingList(printingList);
}

private static void PrintPrintingList(List<Student> students)
{
    foreach (Student student in students)
    {
        Console.WriteLine(string.Format("{0} {1}", student.FirstName, student.LastName));
    }
}
```

Vous remarquerez que même si la liste printingList a été créée avant les corrections des noms des étudiants après les fautes de frappe, la méthode PrintPrintingList imprime toujours les noms corrigés :

    Scott Duke
    Vincent Kong
    Craig Brett

En effet, les deux listes contiennent une liste de références aux mêmes étudiants. Ainsi, la modification de l'objet étudiant sous-jacent se propage aux utilisations par l'une ou l'autre des listes.

Voici à quoi ressemblerait la classe étudiante.

```
public class Student
{
    public string FirstName { get; set; }
    public string LastName { get; set; }

    public Student(string firstName, string lastName)
    {
        this.FirstName = firstName;
        this.LastName = lastName;
    }
}
```


## Passage par référence
Si vous souhaitez que l'exemple Types de valeur vs Types de référence dans les méthodes fonctionne correctement, utilisez le mot-clé ref dans la signature de votre méthode pour le paramètre que vous souhaitez passer par référence, ainsi que lorsque vous appelez la méthode.
<!-- language-all : c# -->

```
public static void Main(string[] args)
{
    ...
    DoubleNumber(ref number); // calling code
    Console.WriteLine(number); // outputs 8
    ...
}
```

```
public void DoubleNumber(ref int number)
{
    number += number;
}
```

Apporter ces modifications mettrait à jour le numéro comme prévu, ce qui signifie que la sortie de la console pour le numéro serait 8.

## Mission


## Différence avec les paramètres de méthode ref et out
Il y a deux manières possibles de passer un type valeur par référence : `ref` et `out`. La différence est qu'en la passant avec `ref` la valeur doit être initialisée mais pas en la passant avec `out`. L'utilisation de `out` garantit que la variable a une valeur après l'appel de la méthode :

    public void ByRef(ref int value)
    {
        Console.WriteLine(nameof(ByRef) + value);
        value += 4;
        Console.WriteLine(nameof(ByRef) + value);
    }

    public void ByOut(out int value)
    {
        value += 4 // CS0269: Use of unassigned out parameter `value'  
        Console.WriteLine(nameof(ByOut) + value); // CS0269: Use of unassigned out parameter `value'  

        value = 4;
        Console.WriteLine(nameof(ByOut) + value);
    }

    public void TestOut()
    {
        int outValue1;
        ByOut(out outValue1); // prints 4

        int outValue2 = 10;   // does not make any sense for out
        ByOut(out outValue2); // prints 4
    }

    public void TestRef()
    {
        int refValue1;
        ByRef(ref refValue1); // S0165  Use of unassigned local variable 'refValue'

        int refValue2 = 0;
        ByRef(ref refValue2); // prints 0 and 4

        int refValue3 = 10;
        ByRef(ref refValue3); // prints 10 and 14
    }

Le problème est qu'en utilisant `out` le paramètre `must` doit être initialisé avant de quitter la méthode, donc la méthode suivante est possible avec `ref` mais pas avec `out` :


    public void EmtyRef(bool condition, ref int value)
    {
        if (condition)
        {
            value += 10;
        }
    }

    public void EmtyOut(bool condition, out int value)
    {
        if (condition)
        {
            value = 10;
        }
    } //CS0177: The out parameter 'value' must be assigned before control leaves the current method

En effet, si la "condition" ne tient pas, la "valeur" n'est pas affectée.

## paramètres ref vs out


