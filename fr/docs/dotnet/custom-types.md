---
title: "Types personnalisés"
slug: "types-personnalises"
draft: false
images: []
weight: 9975
type: docs
toc: true
---

Généralement, une `struct` n'est utilisée que lorsque les performances sont très importantes. Étant donné que les types de valeur vivent sur la pile, ils sont accessibles beaucoup plus rapidement que les classes. Cependant, la pile a beaucoup moins de place que le tas, donc les struct doivent rester petits (Microsoft recommande que les `struct` ne prennent pas plus de 16 octets).

Une `classe` est le type le plus utilisé (parmi ces trois) en C #, et c'est généralement ce que vous devriez utiliser en premier.

Un `enum` est utilisé chaque fois que vous pouvez avoir une liste clairement définie et distincte d'éléments qui n'ont besoin d'être définis qu'une seule fois (au moment de la compilation). Les énumérations sont utiles aux programmeurs en tant que référence légère à une valeur : au lieu de définir une liste de variables "constantes" à comparer, vous pouvez utiliser une énumération et obtenir le support d'Intellisense pour vous assurer que vous n'utilisez pas accidentellement une valeur erronée.

## Définition de la structure
Les structures héritent de System.ValueType, sont des types valeur et vivent sur la pile. Lorsque les types valeur sont passés en paramètre, ils sont passés par valeur.
-------------------------------------------------- ----------------------

    Struct MyStruct
    {
        public int x;
        public int y;
    }

**Transmis par valeur** signifie que la valeur du paramètre est *copiée* pour la méthode et que toute modification apportée au paramètre dans la méthode n'est pas répercutée en dehors de la méthode. Par exemple, considérez le code suivant, qui appelle une méthode nommée `AddNumbers`, en passant les variables `a` et `b`, qui sont de type `int`, qui est un type Value.

    int a = 5;
    int b = 6;
    
    AddNumbers(a,b);

    public AddNumbers(int x, int y)
    {
        int z = x + y; // z becomes 11
        x = x + 5; // now we changed x to be 10
        z = x + y; // now z becomes 16
    } 

Même si nous avons ajouté 5 à `x` dans la méthode, la valeur de `a` reste inchangée, car c'est un type Value, et cela signifie que `x` était une *copie* de la valeur de `a`, mais pas réellement 'un'.

N'oubliez pas que les types de valeur vivent sur la pile et sont transmis par valeur.

    

## Définition de classe
Les classes héritent de System.Object, sont des types de référence et vivent sur le tas. Lorsque les types de référence sont passés en tant que paramètre, ils sont passés par référence.
-------------------------------------------------- ----------------------


    public Class MyClass
    {
        public int a;
        public int b;
    }

**Passé par référence** signifie qu'une *référence* au paramètre est passée à la méthode, et toute modification du paramètre sera reflétée en dehors de la méthode lors de son retour, car la référence est *au même objet exact dans Mémoire*. Utilisons le même exemple que précédemment, mais nous allons d'abord "envelopper" les `int` dans une classe.

    MyClass instanceOfMyClass = new MyClass();
    instanceOfMyClass.a = 5;
    instanceOfMyClass.b = 6;
    
    AddNumbers(instanceOfMyClass);
    
    public AddNumbers(MyClass sample)
    {
        int z = sample.a + sample.b; // z becomes 11
        sample.a = sample.a + 5; // now we changed a to be 10
        z = sample.a + sample.b; // now z becomes 16
    } 

Cette fois, lorsque nous avons remplacé `sample.a` par `10`, la valeur de `instanceOfMyClass.a` *également* change, car elle était *passée par référence*. Passé par référence signifie qu'une *référence* (aussi parfois appelée *pointeur*) vers l'objet a été passée dans la méthode, au lieu d'une copie de l'objet lui-même.

N'oubliez pas que les types de référence vivent sur le tas et sont passés par référence.

## Définition de l'énumération
Une énumération est un type spécial de classe. Le mot clé `enum` indique au compilateur que cette classe hérite de la classe abstraite System.Enum. Les énumérations sont utilisées pour des listes distinctes d'éléments.
-------------------------------------------------- ----------------------

    
    public enum MyEnum
    {
        Monday = 1,
        Tuesday,
        Wednesday,
        //...
    }

Vous pouvez considérer une énumération comme un moyen pratique de mapper des constantes à une valeur sous-jacente. L'énumération définie ci-dessus déclare des valeurs pour chaque jour de la semaine et commence par "1". `Tuesday` deviendrait alors automatiquement mappé sur `2`, `Wednesday` sur `3`, etc.

Par défaut, les énumérations utilisent `int` comme type sous-jacent et commencent à 0, mais vous pouvez utiliser n'importe lequel des *types intégraux* suivants : `byte, sbyte, short, ushort, int, uint, long ou ulong`, et peut spécifier des valeurs explicites pour n'importe quel élément. Si certains éléments sont explicitement spécifiés, mais que d'autres ne le sont pas, chaque élément après le dernier défini sera incrémenté de 1.

Nous utiliserions cet exemple en *transformant* une autre valeur en un *MyEnum* comme ceci :

    MyEnum instance = (MyEnum)3; // the variable named 'instance' gets a 
                                 //value of MyEnum.Wednesday, which maps to 3.

    int x = 2;
    instance = (MyEnum)x; // now 'instance' has a value of MyEnum.Tuesday

Un autre type d'énumération utile, bien que plus complexe, est appelé "Flags". En *décorant* une énumération avec l'attribut `Flags`, vous pouvez attribuer à une variable plusieurs valeurs à la fois. Notez que lorsque vous faites cela, vous * devez * définir explicitement les valeurs dans la représentation de base 2.

    [Flags]
    public enum MyEnum
    {
        Monday = 1,
        Tuesday = 2,
        Wednesday = 4,
        Thursday = 8,
        Friday = 16,
        Saturday = 32, 
        Sunday = 64
    }

Vous pouvez maintenant comparer plusieurs valeurs à la fois, soit en utilisant des *comparaisons au niveau du bit*, soit, si vous utilisez .NET 4.0 ou une version ultérieure, la méthode intégrée `Enum.HasFlag`.

    MyEnum instance = MyEnum.Monday | MyEnum.Thursday; // instance now has a value of
                                                       // *both* Monday and Thursday,
                                                       // represented by (in binary) 0100. 

    if (instance.HasFlag(MyEnum.Wednesday))
    {
        // it doesn't, so this block is skipped
    }
    else if (instance.HasFlag(MyEnum.Thursday))
    {
        // it does, so this block is executed
    }



Étant donné que la classe Enum est une sous-classe de `System.ValueType`, elle est traitée comme un type valeur et transmise par valeur, et non par référence. L'objet de base est créé sur le tas, mais lorsque vous transmettez une valeur enum dans un appel de fonction, une copie de la valeur utilisant le type de valeur sous-jacent de l'Enum (généralement System.Int32) est poussée sur la pile. Le compilateur suit l'association entre cette valeur et l'objet de base qui a été créé sur la pile. Voir [Classe ValueType (System) (MSDN)][1] pour plus d'informations.


[1] : https://msdn.microsoft.com/en-us/library/system.valuetype(v=vs.110).aspx

