---
title: "Méthodes"
slug: "methodes"
draft: false
images: []
weight: 9942
type: docs
toc: true
---

## Appel d'une méthode
Appel d'une méthode statique :

    // Single argument
    System.Console.WriteLine("Hello World");  

    // Multiple arguments
    string name = "User";
    System.Console.WriteLine("Hello, {0}!", name);  

Appel d'une méthode statique et stockage de sa valeur de retour :

    string input = System.Console.ReadLine();

Appel d'une méthode d'instance :

    int x = 42;
    // The instance method called here is Int32.ToString()
    string xAsString = x.ToString();

Appel d'une méthode générique

    // Assuming a method 'T[] CreateArray<T>(int size)'
    DateTime[] dates = CreateArray<DateTime>(8);

## Méthode anonyme
Les méthodes anonymes fournissent une technique pour passer un bloc de code en tant que paramètre délégué. Ce sont des méthodes avec un corps, mais sans nom.
    
    
    delegate int IntOp(int lhs, int rhs);

<!-- séparateur -->

    class Program
    {
        static void Main(string[] args)
        {
            // C# 2.0 definition
            IntOp add = delegate(int lhs, int rhs)
            {
                return lhs + rhs;
            };

            // C# 3.0 definition
            IntOp mul = (lhs, rhs) =>
            {
                return lhs * rhs;
            };

            // C# 3.0 definition - shorthand
            IntOp sub = (lhs, rhs) => lhs - rhs;

            // Calling each method
            Console.WriteLine("2 + 3 = " + add(2, 3));
            Console.WriteLine("2 * 3 = " + mul(2, 3));
            Console.WriteLine("2 - 3 = " + sub(2, 3));
        }
    }

## Déclarer une méthode
Chaque méthode a une signature unique composée d'un accesseur (`public`, `private`, ...) , d'un modificateur facultatif (`abstract`), d'un nom et si nécessaire de paramètres de méthode.
Notez que le type de retour ne fait pas partie de la signature. Un prototype de méthode ressemble à ceci :

    AccessModifier OptionalModifier ReturnType MethodName(InputParameters)
    {
        //Method body
    }

`AccessModifier` peut être `public`, `protected`, `pirvate` ou par défaut `internal`.

`OptionalModifier` peut être `static` `abstract` `virtual` `override` `new` ou `sealed`.

`ReturnType` peut être `void` pour aucun retour ou peut être n'importe quel type parmi ceux de base, comme `int` pour les classes complexes.

une méthode peut avoir certains ou aucun paramètre d'entrée. pour définir les paramètres d'une méthode, vous devez déclarer chacun comme des déclarations de variables normales (comme `int a`), et pour plus d'un paramètre, vous devez utiliser une virgule entre eux (comme `int a, int b`).

Les paramètres peuvent avoir des valeurs par défaut. pour cela, vous devez définir une valeur pour le paramètre (comme `int a = 0`). si un paramètre a une valeur par défaut, la définition de la valeur d'entrée est facultative.

L'exemple de méthode suivant renvoie la somme de deux entiers :

    private int Sum(int a, int b)
    {
        return a + b;
    } 


## Paramètres et arguments
Une méthode peut déclarer n'importe quel nombre de paramètres (dans cet exemple, `i`, `s` et `o` sont les paramètres) :

    static void DoSomething(int i, string s, object o) {
        Console.WriteLine(String.Format("i={0}, s={1}, o={2}", i, s, o));
    }

Les paramètres peuvent être utilisés pour transmettre des valeurs dans une méthode, afin que la méthode puisse fonctionner avec eux. Il peut s'agir de tout type de travail, comme imprimer les valeurs, apporter des modifications à l'objet référencé par un paramètre ou stocker les valeurs.

Lorsque vous appelez la méthode, vous devez transmettre une valeur réelle pour chaque paramètre. À ce stade, les valeurs que vous transmettez réellement à l'appel de méthode sont appelées Arguments :

    DoSomething(x, "hello", new object());



## Types de retour
Une méthode peut renvoyer soit rien (`void`), soit une valeur d'un type spécifié :

    // If you don't want to return a value, use void as return type.
    static void ReturnsNothing() { 
        Console.WriteLine("Returns nothing");
    }

    // If you want to return a value, you need to specify its type.
    static string ReturnsHelloWorld() {
        return "Hello World";
    }

Si votre méthode spécifie une valeur de retour, la méthode *doit* retourner une valeur. Pour ce faire, utilisez l'instruction `return`. Une fois qu'une instruction `return` a été atteinte, elle renvoie la valeur spécifiée et tout code suivant ne sera plus exécuté (les exceptions sont les blocs `finally`, qui seront toujours exécutés avant le retour de la méthode).

Si votre méthode ne renvoie rien (`void`), vous pouvez toujours utiliser l'instruction `return` sans valeur si vous souhaitez revenir immédiatement de la méthode. À la fin d'une telle méthode, une instruction `return` serait cependant inutile.

Exemples d'instructions `return` valides :

    return; 
    return 0; 
    return x * 2;
    return Console.ReadLine();

La levée d'une exception peut mettre fin à l'exécution de la méthode sans renvoyer de valeur. De plus, il existe des blocs itérateurs, où les valeurs de retour sont générées à l'aide du mot clé yield, mais ce sont des cas particuliers qui ne seront pas expliqués à ce stade.

## Paramètres par défaut
Vous pouvez utiliser les paramètres par défaut si vous souhaitez fournir l'option d'omettre les paramètres :

    static void SaySomething(string what = "ehh") {
        Console.WriteLine(what);
    }  

    static void Main() {
        // prints "hello"
        SaySomething("hello"); 
        // prints "ehh"
        SaySomething(); // The compiler compiles this as if we had typed SaySomething("ehh")
    }

Lorsque vous appelez une telle méthode et omettez un paramètre pour lequel une valeur par défaut est fournie, le compilateur insère cette valeur par défaut pour vous.

Gardez à l'esprit que les paramètres avec des valeurs par défaut doivent être écrits **après** les paramètres sans valeurs par défaut.

    static void SaySomething(string say, string what = "ehh") {
            //Correct
            Console.WriteLine(say + what);
        }

    static void SaySomethingElse(string what = "ehh", string say) {
            //Incorrect
            Console.WriteLine(say + what);
        }   

**AVERTISSEMENT** : Étant donné que cela fonctionne ainsi, les valeurs par défaut peuvent être problématiques dans certains cas. Si vous modifiez la valeur par défaut d'un paramètre de méthode et que vous ne recompilez pas tous les appelants de cette méthode, ces appelants utiliseront toujours la valeur par défaut qui était présente lors de leur compilation, ce qui peut entraîner des incohérences.

## Surcharge de méthode
**Définition :** Lorsque plusieurs méthodes portant le même nom sont déclarées avec des paramètres différents, on parle de surcharge de méthode. La surcharge de méthode représente généralement des fonctions qui sont identiques dans leur objectif mais qui sont écrites pour accepter différents types de données comme paramètres.

**Facteurs affectant**

- Nombre d'arguments
- Type d'arguments
-Type de retour**

Considérez une méthode nommée `Area` qui effectuera des fonctions de calcul, qui acceptera divers arguments et renverra le résultat.

**Exemple**

    public string Area(int value1)
    {
        return String.Format("Area of Square is {0}", value1 * value1);
    }
Cette méthode acceptera un argument et renverra une chaîne, si nous appelons la méthode avec un entier (disons `5`), la sortie sera `"La surface du carré est de 25"`.

    public  double Area(double value1, double value2)
    {
        return value1 * value2;
    }
De même, si nous passons deux valeurs doubles à cette méthode, la sortie sera le produit des deux valeurs et sera de type double. Cela peut être utilisé pour la multiplication ainsi que pour trouver l'aire des rectangles

    public double Area(double value1)
    {
        return 3.14 * Math.Pow(value1,2);
    }
Cela peut être utilisé spécialement pour trouver l'aire du cercle, qui accepte une valeur double ("rayon") et renvoie une autre valeur double comme aire.

Chacune de ces méthodes peut être appelée normalement sans conflit - le compilateur examinera les paramètres de chaque appel de méthode pour déterminer quelle version de `Area` doit être utilisée.

    string squareArea = Area(2);
    double rectangleArea = Area(32.0, 17.5);
    double circleArea = Area(5.0); // all of these are valid and will compile.


----------


**Notez que le type de retour *seul* ne peut pas faire la différence entre deux méthodes. Par exemple, si nous avions deux définitions pour Area qui avaient les mêmes paramètres, comme ceci :

    public string Area(double width, double height) { ... }
    public double Area(double width, double height) { ... }
    // This will NOT compile. 

Si nous devons faire en sorte que notre classe utilise les mêmes noms de méthode qui renvoient des valeurs différentes, nous pouvons supprimer les problèmes d'ambiguïté en implémentant une interface et en définissant explicitement son utilisation.

    public interface IAreaCalculatorString {
        
        public string Area(double width, double height);

    }

    public class AreaCalculator : IAreaCalculatorString {

        public string IAreaCalculatorString.Area(double width, double height) { ... } 
        // Note that the method call now explicitly says it will be used when called through
        // the IAreaCalculatorString interface, allowing us to resolve the ambiguity.
        public double Area(double width, double height) { ... }


## Des droits d'accès
    // static: is callable on a class even when no instance of the class has been created
    public static void MyMethod()

    // virtual: can be called or overridden in an inherited class
    public virtual  void MyMethod()

    // internal: access is limited within the current assembly
    internal  void MyMethod()

    //private: access is limited only within the same class
    private  void MyMethod()

    //public: access right from every class / assembly
    public void MyMethod()

    //protected: access is limited to the containing class or types derived from it
    protected void MyMethod()

    //protected internal: access is limited to the current assembly or types derived from the containing class.
    protected internal void MyMethod()

