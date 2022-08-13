---
title: "Les opérateurs"
slug: "les-operateurs"
draft: false
images: []
weight: 9543
type: docs
toc: true
---

En C#, un [opérateur](https://docs.microsoft.com/en-us/dotnet/csharp/program) est un élément de programme appliqué à un ou plusieurs opérandes dans une expression ou une instruction. Les opérateurs qui prennent un opérande, tels que l'opérateur d'incrémentation (++) ou new, sont appelés opérateurs unaires. Les opérateurs qui prennent deux opérandes, tels que les opérateurs arithmétiques (+,-,*,/), sont appelés opérateurs binaires. Un opérateur, l'opérateur conditionnel (?:), prend trois opérandes et est le seul opérateur ternaire en C#.



## Syntaxe
- public static OperandType opérateur operatorSymbol(OperandType operand1)
- public static OperandType opérateur operatorSymbol(OperandType operand1, OperandType2 operand2)

## Paramètres
| Paramètre | Détails |
| --------- | ------- |  
| symboleopérateur | L'opérateur étant surchargé, par ex. +, -, /, * |
| TypeOpérande | Le type qui sera retourné par l'opérateur surchargé.
| opérande1 | Premier opérande à utiliser pour effectuer l'opération.
| opérande2 | Deuxième opérande à utiliser pour effectuer l'opération, lors d'opérations binaires.
| déclarations | Code facultatif nécessaire pour effectuer l'opération avant de renvoyer le résultat.

Tous les opérateurs sont définis comme des "méthodes statiques", ils ne sont pas "virtuels" et ils ne sont pas hérités.
### Priorité des opérateurs

Tous les opérateurs ont une "priorité" particulière en fonction du groupe auquel appartient l'opérateur (les opérateurs du même groupe ont la même priorité). Cela signifie que certains opérateurs seront appliqués avant d'autres. Ce qui suit est une liste de groupes (contenant leurs opérateurs respectifs) classés par ordre de priorité (le plus élevé en premier) :

* **Opérateurs principaux**
* `a.b` - Accès membre.
* `a?.b` - Accès de membre conditionnel nul.
* `->` - Déréférencement du pointeur combiné avec l'accès des membres.
* `f(x)` - Appel de fonction.
* `a[x]` - Indexeur.
* `a?[x]` - Indexeur conditionnel nul.
* `x++` - Incrément de suffixe.
* `x--` - Décrément de suffixe.
* `new` - Instanciation de type.
* `default(T)` - Renvoie la valeur initialisée par défaut de type `T`.
* `typeof` - Renvoie l'objet `Type` de l'opérande.
* `checked` - Active la vérification de débordement numérique.
* `unchecked` - Désactive la vérification de débordement numérique.
* `delegate` - Déclare et renvoie une instance déléguée.
* `sizeof` - Renvoie la taille en octets de l'opérande de type.

* **Opérateurs unaires**
* `+x` - Renvoie `x`.
* `-x` - Négation numérique.
* `!x` - Négation logique.
* `~x` - Complément au niveau du bit/déclare les destructeurs.
* `++x` - Incrément du préfixe.
* `--x` - Décrémentation du préfixe.
* `(T)x` - Moulage type.
* `wait` - attend une `tâche`.
* `&x` - Renvoie l'adresse (pointeur) de `x`.
* `*x` - Déréférencement du pointeur.

* **Opérateurs multiplicatifs**
* `x * y` - Multiplication.
* `x / y` - Division.
* `x % y` - Module.

* **Opérateurs additifs**
* `x + y` - Addition.
* `x – y` - Soustraction.

* **Opérateurs de décalage au niveau du bit**
* `x << y` - Décaler les bits vers la gauche.
* `x >> y` - Décaler les bits vers la droite.

* **Opérateurs relationnels/de test de type**
* `x < y` - Inférieur à.
* `x > y` - Supérieur à.
* `x <= y` - Inférieur ou égal à.
* `x >= y` - Supérieur ou égal à.
* `est` - Compatibilité de type.
* `as` - Conversion de type.

* **Opérateurs d'égalité**
* `x == y` - Égalité.
* `x != y` - Non égal.

* **Opérateur ET logique**
* `x & y` - ET logique/au niveau du bit.

* **Opérateur logique XOR**
* `x ^ y` - XOR logique/au niveau du bit.

* **Opérateur OU logique**
* `x | y` - OU logique/au niveau du bit.

* **Opérateur AND conditionnel**
* `x && y` - ET logique en court-circuit.

* **Opérateur OU conditionnel**
* `x || y` - OU logique de court-circuit.

* **Opérateur de fusion nulle**
* `x ?? y` - Renvoie `x` s'il n'est pas nul ; sinon, renvoie 'y'.

* **Opérateur conditionnel**
* `x ? y : z` - Évalue/renvoie `y` si `x` est vrai ; sinon, évalue `z`.


---

**Contenu connexe**

- [Opérateur de coalescence nulle] [1]
- [Opérateur conditionnel nul][2]
- [nom de l'Opérateur][3]

[1] : https://www.wikiod.com/fr/docs/c%23/37/null-coalescing-operator#t=201511232329424573937
[2] : https://www.wikiod.com/fr/docs/c%23/41/the-null-conditional-operator#t=201511232329445644147
[3] : https://www.wikiod.com/fr/docs/c%23/80/nameof-operator#t=201608081725023270827

## Opérateurs surchargeables
C# permet aux types définis par l'utilisateur de surcharger les opérateurs en définissant des fonctions membres statiques à l'aide du mot clé `operator`.
L'exemple suivant illustre une implémentation de l'opérateur `+`.

Si nous avons une classe `Complex` qui représente un nombre complexe :

    public struct Complex
    {
        public double Real { get; set; }
        public double Imaginary { get; set; }
    }

Et nous voulons ajouter l'option d'utiliser l'opérateur `+` pour cette classe. c'est à dire.:

    Complex a = new Complex() { Real = 1, Imaginary = 2 };
    Complex b = new Complex() { Real = 4, Imaginary = 8 };
    Complex c = a + b;

Nous devrons surcharger l'opérateur `+` pour la classe. Cela se fait à l'aide d'une fonction statique et du mot-clé `operator` :

    public static Complex operator +(Complex c1, Complex c2)
    {
       return new Complex 
       { 
           Real = c1.Real + c2.Real,
           Imaginary = c1.Imaginary + c2.Imaginary 
       };
    }

Les opérateurs tels que `+`, `-`, `*`, `/` peuvent tous être surchargés. Cela inclut également les opérateurs qui ne renvoient pas le même type (par exemple, `==` et `!=` peuvent être surchargés, malgré le retour de booléens). La règle ci-dessous relative aux paires est également appliquée ici.

Les opérateurs de comparaison doivent être surchargés par paires (par exemple, si `<` est surchargé, `>` doit également être surchargé).

Une liste complète des opérateurs surchargeables (ainsi que des opérateurs non surchargeables et des restrictions imposées à certains opérateurs surchargeables) peut être consultée sur [MSDN - Opérateurs surchargeables (Guide de programmation C#)][1].

<!-- si version [gte 7.0] -->
la surcharge de `operator is` a été introduite avec le mécanisme de correspondance de modèles de C# 7.0. Pour plus de détails, voir [Modèle correspondant] [2]

Soit un type `cartésien` défini comme suit

    public class Cartesian
    {
        public int X { get; }
        public int Y { get; }
    }   

Un "opérateur is" surchargeable pourrait par exemple être défini pour les coordonnées "polaires"

    public static class Polar
    {
        public static bool operator is(Cartesian c, out double R, out double Theta)
        {
            R = Math.Sqrt(c.X*c.X + c.Y*c.Y);
            Theta = Math.Atan2(c.Y, c.X);
            return c.X != 0 || c.Y != 0;
        }
    }

qui peut être utilisé comme ça

    var c = Cartesian(3, 4);
    if (c is Polar(var R, *))
    {
        Console.WriteLine(R);
    }

(L'exemple est tiré de la [Roslyn Pattern Matching Documentation][3])
<!-- fin de version si -->

[1] : https://msdn.microsoft.com/en-us/library/8edha89s.aspx
[2] : https://www.wikiod.com/fr/docs/c%23/1936/c-sharp-7-0-features/13323/pattern-matching#t=201608081959042378203
[3] : https://github.com/dotnet/roslyn/blob/future/docs/features/patterns.md

## Surcharger les opérateurs d'égalité
Surcharger uniquement les opérateurs d'égalité ne suffit pas. Dans différentes circonstances, tous les éléments suivants peuvent être appelés :

1. `object.Equals` et `object.GetHashCode`
2. `IEquatable<T>.Equals` (facultatif, permet d'éviter la boxe)
3. `operator ==` et `operator !=` (facultatif, permet d'utiliser des opérateurs)

Lors du remplacement de `Equals`, `GetHashCode` doit également être remplacé. Lors de l'implémentation de `Equals`, il existe de nombreux cas particuliers : comparaison avec des objets d'un type différent, comparaison avec soi-même, etc.

Lorsqu'ils ne sont PAS remplacés, la méthode `Equals` et l'opérateur `==` se comportent différemment pour les classes et les structures. Pour les classes, seules les références sont comparées et pour les structures, les valeurs des propriétés sont comparées par réflexion, ce qui peut affecter négativement les performances. `==` ne peut pas être utilisé pour comparer des structures à moins qu'il ne soit remplacé.

Généralement, l'opération d'égalité doit obéir aux règles suivantes :

- Ne doit pas *lancer d'exceptions*.
- Réflexivité : `A` est toujours égal à `A` (peut ne pas être vrai pour les valeurs `NULL` dans certains systèmes).
- Transitivité : si 'A' est égal à 'B' et 'B' est égal à 'C', alors 'A' est égal à 'C'.
- Si 'A' est égal à 'B', alors 'A' et 'B' ont des codes de hachage égaux.
- Indépendance de l'arbre d'héritage : si `B` et `C` sont des instances de `Class2` héritées de `Class1` :
`Class1.Equals(A,B)` doit toujours renvoyer la même valeur que l'appel à `Class2.Equals(A,B)`.
      

    class Student : IEquatable<Student>
    {
        public string Name { get; set; } = "";
    
        public bool Equals(Student other)
        {
            if (ReferenceEquals(other, null)) return false;
            if (ReferenceEquals(other, this)) return true;
            return string.Equals(Name, other.Name);
        }
    
        public override bool Equals(object obj)
        {
            if (ReferenceEquals(null, obj)) return false;
            if (ReferenceEquals(this, obj)) return true;

            return Equals(obj as Student);
        }
    
        public override int GetHashCode()
        {
            return Name?.GetHashCode() ?? 0;
        }
    
        public static bool operator ==(Student left, Student right)
        {
            return Equals(left, right);
        }
    
        public static bool operator !=(Student left, Student right)
        {
            return !Equals(left, right);
        }
    }

## Opérateurs relationnels
**Équivaut à**

Vérifie si les opérandes fournis (arguments) sont égaux

    "a" == "b"     // Returns false.
    "a" == "a"     // Returns true.
    1 == 0         // Returns false.
    1 == 1         // Returns true.
    false == true  // Returns false.
    false == false // Returns true.

Contrairement à Java, l'opérateur de comparaison d'égalité fonctionne nativement avec les chaînes.

L'opérateur de comparaison d'égalité fonctionnera avec des opérandes de types différents si une conversion implicite existe de l'un à l'autre. Si aucun transtypage implicite approprié n'existe, vous pouvez appeler un transtypage explicite ou utiliser une méthode pour convertir en un type compatible.

    1 == 1.0              // Returns true because there is an implicit cast from int to double.
    new Object() == 1.0   // Will not compile.
    MyStruct.AsInt() == 1 // Calls AsInt() on MyStruct and compares the resulting int with 1.

Contrairement à Visual Basic.NET, l'opérateur de comparaison d'égalité n'est pas le même que l'opérateur d'affectation d'égalité.

    var x = new Object();
    var y = new Object();
    x == y // Returns false, the operands (objects in this case) have different references.
    x == x // Returns true, both operands have the same reference.

<sup>*À ne pas confondre avec l'opérateur d'affectation (`=`).*</sup>

Pour les types valeur, l'opérateur renvoie "true" si les deux opérandes ont la même valeur.
Pour les types de référence, l'opérateur renvoie "true" si les deux opérandes sont égaux dans *référence* (et non valeur). Une exception est que les objets de chaîne seront comparés avec l'égalité des valeurs.

**Pas égal à**

Vérifie si les opérandes fournis ne sont *pas* égaux.

    "a" != "b"     // Returns true.
    "a" != "a"     // Returns false.
    1 != 0         // Returns true.
    1 != 1         // Returns false.
    false != true  // Returns true.
    false != false // Returns false.

    var x = new Object();
    var y = new Object();
    x != y // Returns true, the operands have different references.
    x != x // Returns false, both operands have the same reference.

Cet opérateur renvoie effectivement le résultat opposé à celui de l'opérateur égal (`==`)

**Plus grand que**

Vérifie si le premier opérande est supérieur au deuxième opérande.

    3 > 5    //Returns false.
    1 > 0    //Returns true.
    2 > 2    //Return false.
    
    var x = 10;
    var y = 15;
    x > y    //Returns false.
    y > x    //Returns true.

**Moins que**

Vérifie si le premier opérande est inférieur au deuxième opérande.

    2 < 4     //Returns true.
    1 < -3    //Returns false.
    2 < 2     //Return false.
    
    var x = 12;
    var y = 22;
    x < y    //Returns true.
    y < x    //Returns false.

**Supérieur à égal à**

Vérifie si le premier opérande est supérieur ou égal au deuxième opérande.

    7 >= 8    //Returns false.
    0 >= 0    //Returns true.
    
**Inférieur à égal à**

Vérifie si le premier opérande est inférieur ou égal au deuxième opérande.

    2 <= 4    //Returns true.
    1 <= -3    //Returns false.
    1 <= 1     //Returns true. 
    
  
  

## Opérateurs de diffusion implicite et de diffusion explicite
C # permet aux types définis par l'utilisateur de contrôler l'affectation et la diffusion via l'utilisation des mots-clés "explicit" et "implicit". La signature de la méthode prend la forme :

    public static <implicit/explicit> operator <ResultingType>(<SourceType> myType)

La méthode ne peut plus prendre d'arguments, ni être une méthode d'instance. Il peut cependant accéder à tous les membres privés du type dans lequel il est défini.

Un exemple de distribution "implicite" et "explicite":

    public class BinaryImage 
    {
        private bool[] _pixels;

        public static implicit operator ColorImage(BinaryImage im)
        {
            return new ColorImage(im);
        }

        public static explicit operator bool[](BinaryImage im)
        {
            return im._pixels;
        }
    }

Autoriser la syntaxe de cast suivante :

    var binaryImage = new BinaryImage();
    ColorImage colorImage = binaryImage; // implicit cast, note the lack of type 
    bool[] pixels = (bool[])binaryImage; // explicit cast, defining the type

Les opérateurs de cast peuvent fonctionner dans les deux sens, allant *de* votre type et allant *vers* votre type :

    public class BinaryImage
    {
        public static explicit operator ColorImage(BinaryImage im)
        {
            return new ColorImage(im);
        }

        public static explicit operator BinaryImage(ColorImage cm)
        {
            return new BinaryImage(cm);
        }
    }

Enfin, le mot clé `as`, qui peut être impliqué dans la conversion au sein d'une hiérarchie de types, n'est ** pas ** valide dans cette situation. Même après avoir défini un cast "explicite" ou "implicite", vous ne pouvez pas :

    ColorImage cm = myBinaryImage as ColorImage;

Cela générera une erreur de compilation.

## Opérateurs en court-circuit
*Par définition, les opérateurs booléens de court-circuit n'évalueront le deuxième opérande que si le premier opérande ne peut pas déterminer le résultat global de l'expression.*

Cela signifie que, si vous utilisez l'opérateur && comme *firstCondition && secondCondition*, il évaluera *secondCondition* uniquement lorsque *firstCondition* est vrai et, bien sûr, le résultat global ne sera vrai que si les deux *firstOperand* et *secondOperand* sont évalués à vrai. Ceci est utile dans de nombreux scénarios, par exemple, imaginez que vous souhaitez vérifier alors que votre liste contient plus de trois éléments, mais vous devez également vérifier si la liste a été initialisée pour ne pas se heurter à * NullReferenceException *. Vous pouvez y parvenir comme ci-dessous :

    bool hasMoreThanThreeElements = myList != null && mList.Count > 3;

*mList.Count > 3* ne sera pas vérifié jusqu'à ce que myList != null soit satisfait.

**ET logique**

`&&` est l'équivalent de court-circuit de l'opérateur booléen ET standard (`&`).

    var x = true;
    var y = false;

    x && x // Returns true.
    x && y // Returns false (y is evaluated).
    y && x // Returns false (x is not evaluated).
    y && y // Returns false (right y is not evaluated).

**OU logique**

`||` est l'équivalent de court-circuit de l'opérateur booléen standard OR (`|`).

    var x = true;
    var y = false;

    x || x // Returns true (right x is not evaluated).
    x || y // Returns true (y is not evaluated).
    y || x // Returns true (x and y are evaluated).
    y || y // Returns false (y and y are evaluated).

**Exemple d'utilisation**

    if(object != null && object.Property)
    // object.Property is never accessed if object is null, because of the short circuit.
        Action1();
    else
        Action2();

## ? : Opérateur ternaire
Renvoie l'une des deux valeurs en fonction de la valeur d'une expression booléenne.

Syntaxe:

    condition ? expression_if_true : expression_if_false;

Exemple:

    string name = "Frank";
    Console.WriteLine(name == "Frank" ? "The name is Frank" : "The name is not Frank");

L'opérateur ternaire est associatif à droite, ce qui permet d'utiliser des expressions ternaires composées. Cela se fait en ajoutant des équations ternaires supplémentaires dans la position vraie ou fausse d'une équation ternaire parente. Des précautions doivent être prises pour assurer la lisibilité, mais cela peut être un raccourci utile dans certaines circonstances.

Dans cet exemple, une opération ternaire composée évalue une fonction `clamp` et renvoie la valeur actuelle si elle est dans la plage, la valeur `min` si elle est en dessous de la plage ou la valeur `max` si elle est au-dessus de la plage.

    light.intensity = Clamp(light.intensity, minLight, maxLight);

    public static float Clamp(float val, float min, float max)
    {
        return (val < min) ? min : (val > max) ? max : val;
    }

Les opérateurs ternaires peuvent également être imbriqués, tels que :

    a ? b ? "a is true, b is true" : "a is true, b is false" : "a is false"
    
    // This is evaluated from left to right and can be more easily seen with parenthesis:
    
    a ? (b ? x : y) : z

    // Where the result is x if a && b, y if a && !b, and z if !a

Lors de l'écriture d'instructions ternaires composées, il est courant d'utiliser des parenthèses ou des indentations pour améliorer la lisibilité.

Les types de *expression_if_true* et *expression_if_false* doivent être identiques ou il doit y avoir une conversion implicite de l'un à l'autre.

    condition ? 3 : "Not three"; // Doesn't compile because `int` and `string` lack an implicit conversion.

    condition ? 3.ToString() : "Not three"; // OK because both possible outputs are strings.

    condition ? 3 : 3.5; // OK because there is an implicit conversion from `int` to `double`. The ternary operator will return a `double`.

    condition ? 3.5 : 3; // OK because there is an implicit conversion from `int` to `double`. The ternary operator will return a `double`.

Les exigences de type et de conversion s'appliquent également à vos propres classes.

    public class Car
    {}

    public class SportsCar : Car
    {}

    public class SUV : Car
    {}

    condition ? new SportsCar() : new Car(); // OK because there is an implicit conversion from `SportsCar` to `Car`. The ternary operator will return a reference of type `Car`.

    condition ? new Car() : new SportsCar(); // OK because there is an implicit conversion from `SportsCar` to `Car`. The ternary operator will return a reference of type `Car`.

    condition ? new SportsCar() : new SUV(); // Doesn't compile because there is no implicit conversion from `SportsCar` to SUV or `SUV` to `SportsCar`. The compiler is not smart enough to realize that both of them have an implicit conversion to `Car`.

    condition ? new SportsCar() as Car : new SUV() as Car; // OK because both expressions evaluate to a reference of type `Car`. The ternary operator will return a reference of type `Car`.


## ?. (Opérateur conditionnel nul)
<!-- si version [gte 6.0] -->

[Introduit dans C# 6.0][1], l'opérateur conditionnel Null `?.` renverra immédiatement `null` si l'expression sur son côté gauche est évaluée à `null`, au lieu de lancer une `NullReferenceException`. Si son côté gauche donne une valeur autre que "null", il est traité comme un opérateur "." normal. Notez que parce qu'il peut renvoyer `null`, son type de retour est toujours un type nullable. Cela signifie que pour une structure ou un type primitif, il est enveloppé dans un `Nullable<T>`.

    var bar = Foo.GetBar()?.Value; // will return null if GetBar() returns null
    var baz = Foo.GetBar()?.IntegerValue; // baz will be of type Nullable<int>, i.e. int?
  
Cela est pratique lors du déclenchement d'événements. Normalement, vous devriez envelopper l'appel d'événement dans une instruction if vérifiant `null` et déclencher l'événement par la suite, ce qui introduit la possibilité d'une condition de concurrence. En utilisant l'opérateur conditionnel Null, cela peut être corrigé de la manière suivante :

    event EventHandler<string> RaiseMe;
    RaiseMe?.Invoke("Event raised");

<!-- fin de version si -->


[1] : https://www.wikiod.com/fr/docs/c%23/24/c-sharp-6-0-features/51/null-propagation#t=201607301051500162149

## taille de
Renvoie un `int` contenant la taille d'un type<sup>*</sup> en octets.

    sizeof(bool)    // Returns 1.
    sizeof(byte)    // Returns 1.
    sizeof(sbyte)   // Returns 1.
    sizeof(char)    // Returns 2.
    sizeof(short)   // Returns 2.
    sizeof(ushort)  // Returns 2.
    sizeof(int)     // Returns 4.
    sizeof(uint)    // Returns 4.
    sizeof(float)   // Returns 4.
    sizeof(long)    // Returns 8.
    sizeof(ulong)   // Returns 8.
    sizeof(double)  // Returns 8.
    sizeof(decimal) // Returns 16.

<sup>** Prend uniquement en charge certains types primitifs dans un contexte sécurisé.*</sup>

Dans un contexte non sécurisé, `sizeof` peut être utilisé pour renvoyer la taille d'autres types et structures primitifs.

    public struct CustomType
    {
        public int value;
    }

    static void Main()
    {
        unsafe
        {
            Console.WriteLine(sizeof(CustomType)); // outputs: 4
        }
    }

## Opérateurs de membres de classe : accès de membre conditionnel nul
    var zipcode = myEmployee?.Address?.ZipCode;
    //returns null if the left operand is null.  
    //the above is the equivalent of:
    var zipcode = (string)null;
    if (myEmployee != null && myEmployee.Address != null)
        zipcode = myEmployee.Address.ZipCode;

## Opérateurs de membres de classe : indexation conditionnelle nulle
    var letters = null;
    char? letter = letters?[1];
    Console.WriteLine("Second Letter is {0}",letter);
    //in the above example  rather than throwing an error because letters is null
    //letter is assigned the value null

## Opérateur "ou exclusif"
L'opérateur pour un "ou exclusif" (pour court XOR) est : ^

Cet opérateur renvoie vrai lorsqu'un, mais un seul, des booléens fournis est vrai.

    true ^ false   // Returns true
    false ^ true   // Returns true
    false ^ false  // Returns false
    true ^ true    // Returns false

## Opérateurs de décalage de bits
Les opérateurs de décalage permettent aux programmeurs d'ajuster un entier en décalant tous ses bits vers la gauche ou vers la droite. Le diagramme suivant montre l'effet du décalage d'une valeur vers la gauche d'un chiffre.

**Décalage à gauche**

    uint value = 15;              // 00001111
     
    uint doubled = value << 1;    // Result = 00011110 = 30
    uint shiftFour = value << 4;  // Result = 11110000 = 240

** Maj droite **

    uint value = 240;             // 11110000
     
    uint halved = value >> 1;     // Result = 01111000 = 120
    uint shiftFour = value >> 4;  // Result = 00001111 = 15

## Opérateur par défaut
Type de valeur (où T : struct)
---
Les types de données primitifs intégrés, tels que `char`, `int` et `float`, ainsi que les types définis par l'utilisateur déclarés avec `struct` ou `enum`. Leur valeur par défaut est `new T()` :

    default(int)            // 0
    default(DateTime)       // 0001-01-01 12:00:00 AM
    default(char)           // '\0' This is the "null character", not a zero or a line break.
    default(Guid)           // 00000000-0000-0000-0000-000000000000
    default(MyStruct)       // new MyStruct()

    // Note: default of an enum is 0, and not the first *key* in that enum
    // so it could potentially fail the Enum.IsDefined test
    default(MyEnum)         // (MyEnum)0

Type de référence (où T : classe)
---

N'importe quel type de `classe`, `interface`, tableau ou délégué. Leur valeur par défaut est `null` :

    default(object)         // null
    default(string)         // null
    default(MyClass)        // null
    default(IDisposable)    // null
    default(dynamic)        // null

## Incrémentation et décrémentation du suffixe et du préfixe
L'incrément de suffixe `X++` ajoutera `1` à `x`

    var x = 42;
    x++;
    Console.WriteLine(x); // 43

Le décrément suffixe `X--` soustraira un

    var x = 42
    x--; 
    Console.WriteLine(x); // 41



`++x` est appelé incrément de préfixe, il incrémente la valeur de x puis renvoie x
tandis que `x++` renvoie la valeur de x puis incrémente

    var x = 42;
    Console.WriteLine(++x); // 43
    System.out.println(x); // 43

tandis que

    var x = 42;
    Console.WriteLine(x++); // 42
    System.out.println(x); // 43

les deux sont couramment utilisés dans la boucle for

    for(int i = 0; i < 10; i++)
    {
    }


## => Opérateur Lambda
<!-- si version [gte 3.0] -->

*L'opérateur `=>` a la même priorité que l'opérateur d'affectation `=` et est associatif à droite.*

Il est utilisé pour déclarer des expressions lambda et il est également largement utilisé avec [LINQ Queries](https://www.wikiod.com/fr/docs/c%23/68/linq-queries/4735/basics#t=201607251514251028068):

    string[] words = { "cherry", "apple", "blueberry" };

    int shortestWordLength = words.Min((string w) => w.Length); //5

Lorsqu'il est utilisé dans des extensions ou des requêtes LINQ, le type des objets peut généralement être ignoré car il est déduit par le compilateur :

    int shortestWordLength = words.Min(w => w.Length); //also compiles with the same result

La forme générale de l'opérateur lambda est la suivante :

    (input parameters) => expression

Les paramètres de l'expression lambda sont spécifiés avant l'opérateur `=>`, et l'expression/l'instruction/le bloc à exécuter se trouve à droite de l'opérateur :

    // expression
    (int x, string s) => s.Length > x

    // expression
    (int x, int y) => x + y

    // statement
    (string x) => Console.WriteLine(x)

    // block
    (string x) => {
            x += " says Hello!";
            Console.WriteLine(x);
        }

Cet opérateur peut être utilisé pour définir facilement des délégués, sans écrire de méthode explicite :

    delegate void TestDelegate(string s);
    
    TestDelegate myDelegate = s => Console.WriteLine(s + " World");

    myDelegate("Hello");

à la place de

    void MyMethod(string s)
    {
        Console.WriteLine(s + " World");
    }
    
    delegate void TestDelegate(string s);

    TestDelegate myDelegate = MyMethod;

    myDelegate("Hello");

<!-- fin de version si -->

## Opérateur d'affectation '='
L'opérateur d'affectation `=` définit la valeur de l'opérande de gauche sur la valeur de l'opérande de droite et renvoie cette valeur :

    int a = 3;     // assigns value 3 to variable a
    int b = a = 5; // first assigns value 5 to variable a, then does the same for variable b
    Console.WriteLine(a = 3 + 4); // prints 7


## ?? Opérateur de fusion nulle
L'opérateur Null-Coalescing `??` renverra le côté gauche lorsqu'il n'est pas nul. S'il est nul, il renverra le côté droit.

    object foo = null;
    object bar = new object();
    
    var c = foo ?? bar;
    //c will be bar since foo was null

L'opérateur `??` peut être chaîné, ce qui permet de supprimer les vérifications `if`.

    //config will be the first non-null returned.
    var config = RetrieveConfigOnMachine() ??
                 RetrieveConfigFromService() ??
                 new DefaultConfiguration();





## Opérateurs membres du groupe : accès des membres
    var now = DateTime.UtcNow;
    //accesses member of a class.  In this case the UtcNow property.

## Opérateurs de membre de classe : Invocation de fonction
    var age = GetAge(dateOfBirth);
    //the above calls the function GetAge passing parameter dateOfBirth.

## Opérateurs de membres de classe : indexation d'objets agrégés
    var letters = "letters".ToCharArray();
    char letter = letters[1];
    Console.WriteLine("Second Letter is {0}",letter);
    //in the above example we take the second character from the array
    //by calling letters[1]
    //NB: Array Indexing starts at 0; i.e. the first letter would be given by letters[0].

## Opérateurs binaires avec affectation
C # a plusieurs opérateurs qui peuvent être combinés avec un signe `=` pour évaluer le résultat de l'opérateur, puis affecter le résultat à la variable d'origine.

Exemple:

    x += y

est le même que

    x = x + y

Opérateurs d'affectation :

 - `+=`
 - `-=`
 - `*=`
 - `/=`
 - `%=`
 - `&=`
 - `|=`
 - `^=`
 - `<<=`
 - `>>=`

## Type de
Obtient l'objet `System.Type` pour un type.
     
    System.Type type = typeof(Point)        //System.Drawing.Point      
    System.Type type = typeof(IDisposable)  //System.IDisposable
    System.Type type = typeof(Colors)       //System.Drawing.Color
    System.Type type = typeof(List<>)       //System.Collections.Generic.List`1[T]

Pour obtenir le type d'exécution, utilisez la méthode `GetType` pour obtenir le `System.Type` de l'instance actuelle.

L'opérateur `typeof` prend un nom de type comme paramètre, qui est spécifié au moment de la compilation.

    public class Animal {} 
    public class Dog : Animal {}
    
    var animal = new Dog();

    Assert.IsTrue(animal.GetType() == typeof(Animal)); // fail, animal is typeof(Dog) 
    Assert.IsTrue(animal.GetType() == typeof(Dog));    // pass, animal is typeof(Dog)
    Assert.IsTrue(animal is Animal);                   // pass, animal implements Animal


## nom de l'opérateur
Renvoie une chaîne qui représente le nom non qualifié d'une `variable`, `type` ou `member`.

    int counter = 10;
    nameof(counter); // Returns "counter"
    Client client = new Client();
    nameof(client.Address.PostalCode)); // Returns "PostalCode"

L'opérateur `nameof` a été introduit dans C# 6.0. Elle est évaluée au moment de la compilation et la valeur de chaîne renvoyée est insérée en ligne par le compilateur, elle peut donc être utilisée dans la plupart des cas où la chaîne constante peut être utilisée (par exemple, les étiquettes `case` dans une instruction `switch`, les attributs , etc...). Cela peut être utile dans des cas tels que la levée et la journalisation d'exceptions, d'attributs, de liens d'action MVC, etc.

