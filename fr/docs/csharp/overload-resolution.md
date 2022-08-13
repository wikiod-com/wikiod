---
title: "Résolution de surcharge"
slug: "resolution-de-surcharge"
draft: false
images: []
weight: 9975
type: docs
toc: true
---

Le processus de résolution de surcharge est décrit dans la [spécification C#][1], section 7.5.3. Les sections 7.5.2 (inférence de type) et 7.6.5 (expressions d'invocation) sont également pertinentes.

Le fonctionnement de la résolution de surcharge sera probablement modifié dans C# 7. Les notes de conception indiquent que Microsoft déploiera un nouveau système pour déterminer quelle méthode est la meilleure (dans des scénarios compliqués).


[1] : https://www.microsoft.com/en-us/download/details.aspx?id=7029

## Exemple de surcharge de base
Ce code contient une méthode surchargée nommée **Hello** :

    class Example
    {
        public static void Hello(int arg)
        {
            Console.WriteLine("int");
        }
     
        public static void Hello(double arg)
        {
            Console.WriteLine("double");
        }
     
        public static void Main(string[] args) 
        {
            Hello(0);
            Hello(0.0);
        }
    }

Lorsque la méthode **Main** est appelée, elle imprimera

    int
    double

Au moment de la compilation, lorsque le compilateur trouve l'appel de méthode `Hello(0)`, il trouve toutes les méthodes portant le nom `Hello`. Dans ce cas, il en trouve deux. Il essaie ensuite de déterminer laquelle des méthodes est *meilleure*. L'algorithme pour déterminer quelle méthode est la meilleure est complexe, mais il se résume généralement à "faire le moins de conversions implicites possible".

Ainsi, dans le cas de `Hello(0)`, aucune conversion n'est nécessaire pour la méthode `Hello(int)` mais une conversion numérique implicite est nécessaire pour la méthode `Hello(double)`. Ainsi, la première méthode est choisie par le compilateur.

Dans le cas de `Hello(0.0)`, il n'y a aucun moyen de convertir implicitement `0.0` en `int`, donc la méthode `Hello(int)` n'est même pas prise en compte pour la résolution de surcharge. Seule la méthode reste et elle est donc choisie par le compilateur.

## "params" n'est pas développé, sauf si nécessaire.
Le programme suivant :

    class Program
    {
        static void Method(params Object[] objects)
        {
            System.Console.WriteLine(objects.Length);
        }   
        static void Method(Object a, Object b)
        {
            System.Console.WriteLine("two");
        }
        static void Main(string[] args)
        {
            object[] objectArray = new object[5];

            Method(objectArray);
            Method(objectArray, objectArray);
            Method(objectArray, objectArray, objectArray);
        }
    }

imprimera :

    5
    two
    3

L'expression d'appel `Method(objectArray)` pourrait être interprétée de deux manières : un seul argument `Object` qui se trouve être un tableau (ainsi le programme afficherait `1` car ce serait le nombre d'arguments, ou comme un tableau d'arguments, donnés sous la forme normale, comme si la méthode "Method" n'avait pas le mot-clé "params". Dans ces situations, la forme normale, non développée, a toujours la priorité. Ainsi, le programme affiche "5".

Dans la seconde expression, `Method(objectArray, objectArray)`, la forme développée de la première méthode et la seconde méthode traditionnelle sont applicables. Dans ce cas également, les formulaires non développés sont prioritaires, de sorte que le programme imprime "deux".

Dans la troisième expression, `Method(objectArray, objectArray, objectArray)`, la seule option est d'utiliser la forme développée de la première méthode, et donc le programme imprime `3`.

## Passer null comme l'un des arguments
Si tu as

    void F1(MyType1 x) {
        // do something
    }

    void F1(MyType2 x) {
        // do something else
    }

et pour une raison quelconque, vous devez appeler la première surcharge de `F1` mais avec `x = null`, puis en faisant simplement

    F1(null);

ne compilera pas car l'appel est ambigu. Pour contrer cela, vous pouvez faire

    F1(null as MyType1);

