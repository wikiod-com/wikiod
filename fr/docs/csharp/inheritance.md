---
title: "Héritage"
slug: "heritage"
draft: false
images: []
weight: 9785
type: docs
toc: true
---

## Syntaxe
- classe DerivedClass : BaseClass
- classe DerivedClass : BaseClass, IExampleInterface
- classe DerivedClass : BaseClass, IExampleInterface, IAnotherInterface

Les classes peuvent hériter directement d'une seule classe, mais (à la place ou en même temps) peuvent implémenter une ou plusieurs interfaces.

Les structures peuvent implémenter des interfaces mais ne peuvent pas hériter explicitement de n'importe quel type. Ils héritent implicitement de `System.ValueType`, qui à son tour hérite directement de `System.Object`.

Les classes statiques [ne peuvent pas][1] implémenter des interfaces.


[1] : http://stackoverflow.com/a/259079

## Héritage. Séquence d'appels des constructeurs
Considérons que nous avons une classe `Animal` qui a une classe enfant `Dog`

    class Animal
    {
        public Animal()
        {
            Console.WriteLine("In Animal's constructor");
        }
    }
    
    class Dog : Animal
    {
        public Dog()
        {
            Console.WriteLine("In Dog's constructor");
        }
    }

Par défaut, chaque classe hérite implicitement de la classe `Object`.

C'est le même que le code ci-dessus.

    class Animal : Object
    {
        public Animal()
        {
            Console.WriteLine("In Animal's constructor");
        }
    }

Lors de la création d'une instance de la classe `Dog`, le constructeur par défaut des **classes de base (sans paramètres) sera appelé s'il n'y a pas d'appel explicite à un autre constructeur dans la classe parent**. Dans notre cas, le constructeur sera d'abord appelé `Object's`, puis `Animal's` et à la fin `Dog's`.

    public class Program
    {
        public static void Main()
        {
            Dog dog = new Dog();
        }
    }

La sortie sera
> Dans le constructeur d'Animal
> Dans le constructeur de Dog

[Voir la démo][1]

**Appelle explicitement le constructeur du parent.**

Dans les exemples ci-dessus, notre constructeur de classe `Dog` appelle le constructeur **default** de la classe `Animal`. Si vous le souhaitez, vous pouvez spécifier quel constructeur doit être appelé : il est possible d'appeler n'importe quel constructeur défini dans la classe parent.

Considérons que nous avons ces deux classes.

    class Animal
    {
        protected string name;
    
        public Animal()
        {
            Console.WriteLine("Animal's default constructor");
        }    
    
        public Animal(string name)
        {
            this.name = name;
            Console.WriteLine("Animal's constructor with 1 parameter");
            Console.WriteLine(this.name);
        }
    }

    class Dog : Animal
    {
        public Dog() : base()
        {
            Console.WriteLine("Dog's default constructor");
        }  
    
        public Dog(string name) : base(name)
        {
            Console.WriteLine("Dog's constructor with 1 parameter");
            Console.WriteLine(this.name);
        }
    }

** Qu'est-ce qui se passe ici ? **

Nous avons 2 constructeurs dans chaque classe.

**Que signifie "base" ?**

`base` est une référence à la classe parent. Dans notre cas, lorsque nous créons une instance de la classe `Dog` comme celle-ci

    Dog dog = new Dog();

Le runtime appelle d'abord `Dog()`, qui est le constructeur sans paramètre. Mais son corps ne fonctionne pas immédiatement. Après les parenthèses du constructeur, nous avons un tel appel : `base()`, ce qui signifie que lorsque nous appelons le constructeur par défaut `Dog`, il appellera à son tour le constructeur **default** du parent. Après l'exécution du constructeur du parent, il reviendra puis, enfin, exécutera le corps du constructeur `Dog()`.

La sortie ressemblera donc à ceci :
> Constructeur par défaut de l'animal
> Constructeur par défaut du chien

[Voir la démo][2]

**Et maintenant, que se passe-t-il si nous appelons le constructeur `Dog's` avec un paramètre ?**

    Dog dog = new Dog("Rex");

Vous savez que les membres de la classe parent qui ne sont pas privés sont hérités par la classe enfant, ce qui signifie que `Dog` aura également le champ `name`.
Dans ce cas, nous avons passé un argument à notre constructeur. Il passe à son tour l'argument au **constructeur de la classe parente avec un paramètre**, qui initialise le champ `name`.

La sortie sera

<!-- langue : lang-none -->
    Animal's constructor with 1 parameter
    Rex
    Dog's constructor with 1 parameter
    Rex

**Sommaire:**

Chaque création d'objet commence à partir de la classe de base. Dans l'héritage, les classes qui sont dans la hiérarchie sont chaînées. Comme toutes les classes dérivent de `Object`, le premier constructeur à être appelé lors de la création d'un objet est le constructeur de classe `Object` ; Ensuite, le constructeur suivant de la chaîne est appelé et ce n'est qu'après qu'ils sont tous appelés que l'objet est créé

**mot clé de base**

1) Le mot-clé base est utilisé pour accéder aux membres de la classe de base à partir d'une classe dérivée :
2) Appelez une méthode sur la classe de base qui a été remplacée par une autre méthode.
Spécifiez quel constructeur de classe de base doit être appelé lors de la création d'instances de la classe dérivée.


[1] : https://dotnetfiddle.net/uOL8cE
[2] : https://dotnetfiddle.net/eRKEjT

## Héritage d'une classe de base
Pour éviter la duplication de code, définissez des méthodes et des attributs communs dans une classe générale comme base :

    public class Animal 
    {
        public string Name { get; set; }
        // Methods and attributes common to all animals
        public void Eat(Object dinner)
        {
            // ...
        }
        public void Stare()
        {
            // ...
        }
        public void Roll()
        {
            // ...
        }
    }
  
Maintenant que vous avez une classe qui représente "Animal" en général, vous pouvez définir une classe qui décrit les particularités d'animaux spécifiques :
  
    public class Cat : Animal
    {
        public Cat() 
        {
            Name = "Cat";
        }
        // Methods for scratching furniture and ignoring owner
        public void Scratch(Object furniture)
        {
            // ...
        }
    }

La classe Cat a accès non seulement aux méthodes décrites explicitement dans sa définition, mais également à toutes les méthodes définies dans la classe de base générale "Animal". N'importe quel animal (qu'il s'agisse ou non d'un chat) pouvait manger, regarder ou rouler. Cependant, un animal ne pourrait pas gratter s'il n'était pas aussi un chat. Vous pourriez alors définir d'autres classes décrivant d'autres animaux. (Comme Gopher avec une méthode pour détruire les jardins de fleurs et Sloth sans aucune méthode supplémentaire.)

## Héritage d'une classe et implémentation d'une interface
    public class Animal 
    {
        public string Name { get; set; }
    }

    public interface INoiseMaker
    {
        string MakeNoise();
    }

    //Note that in C#, the base class name must come before the interface names
    public class Cat : Animal, INoiseMaker
    {
        public Cat() 
        {
            Name = "Cat";
        }

        public string MakeNoise()
        {
            return "Nyan";
        }
    }



## Héritage d'une classe et implémentation de plusieurs interfaces
    public class LivingBeing
    {
        string Name { get; set; }
    }
    
    public interface IAnimal 
    {
        bool HasHair { get; set; }
    }
    
    public interface INoiseMaker
    {
        string MakeNoise();
    }
    
    //Note that in C#, the base class name must come before the interface names
    public class Cat : LivingBeing, IAnimal, INoiseMaker
    {
        public Cat() 
        {
            Name = "Cat";
            HasHair = true;
        }
    
        public bool HasHair { get; set; }
    
        public string Name { get; set; }

        public string MakeNoise()
        {
            return "Nyan";
        }
    }

## Tester et naviguer dans l'héritage
    interface BaseInterface {}
    class BaseClass : BaseInterface {}

    interface DerivedInterface {}
    class DerivedClass : BaseClass, DerivedInterface {}
    
    var baseInterfaceType = typeof(BaseInterface);
    var derivedInterfaceType = typeof(DerivedInterface);
    var baseType = typeof(BaseClass);
    var derivedType = typeof(DerivedClass);
    
    var baseInstance = new BaseClass();
    var derivedInstance = new DerivedClass();  
    
    Console.WriteLine(derivedInstance is DerivedClass); //True
    Console.WriteLine(derivedInstance is DerivedInterface); //True
    Console.WriteLine(derivedInstance is BaseClass); //True
    Console.WriteLine(derivedInstance is BaseInterface); //True
    Console.WriteLine(derivedInstance is object); //True
    
    Console.WriteLine(derivedType.BaseType.Name);  //BaseClass
    Console.WriteLine(baseType.BaseType.Name);  //Object
    Console.WriteLine(typeof(object).BaseType);  //null
    
    Console.WriteLine(baseType.IsInstanceOfType(derivedInstance));  //True
    Console.WriteLine(derivedType.IsInstanceOfType(baseInstance));  //False

    Console.WriteLine(
        string.Join(",", 
        derivedType.GetInterfaces().Select(t => t.Name).ToArray()));
    //BaseInterface,DerivedInterface
        
    Console.WriteLine(baseInterfaceType.IsAssignableFrom(derivedType)); //True
    Console.WriteLine(derivedInterfaceType.IsAssignableFrom(derivedType)); //True
    Console.WriteLine(derivedInterfaceType.IsAssignableFrom(baseType)); //False

## Extension d'une classe de base abstraite
Contrairement aux interfaces, qui peuvent être décrites comme des contrats d'implémentation, les classes abstraites agissent comme des contrats d'extension.

Une classe abstraite ne peut pas être instanciée, elle doit être étendue et la classe résultante (ou classe dérivée) peut alors être instanciée.

Les classes abstraites sont utilisées pour fournir des implémentations génériques

    public abstract class Car
    {
        public void HonkHorn() {
            // Implementation of horn being honked
        }
    }

    public class Mustang : Car
    {
        // Simply by extending the abstract class Car, the Mustang can HonkHorn()
        // If Car were an interface, the HonkHorn method would need to be included
        // in every class that implemented it.
    }

L'exemple ci-dessus montre comment toute classe étendant Car recevra automatiquement la méthode HonkHorn avec l'implémentation. Cela signifie que tout développeur créant une nouvelle voiture n'aura pas à se soucier de la façon dont elle klaxonnera.

## Constructeurs dans une sous-classe
Lorsque vous créez une sous-classe d'une classe de base, vous pouvez construire la classe de base en utilisant `: base` après les paramètres du constructeur de la sous-classe.

    class Instrument
    {
        string type;
        bool clean;
    
        public Instrument (string type, bool clean)
        {
            this.type = type;
            this.clean = clean;
        }
    }
    
    class Trumpet : Instrument
    {
        bool oiled;
    
        public Trumpet(string type, bool clean, bool oiled) : base(type, clean)
        {
            this.oiled = oiled;
        }
    }

## Anti-modèles d'héritage
# Héritage incorrect

Disons qu'il y a 2 classes "Foo" et "Bar". `Foo` a deux fonctionnalités `Do1` et `Do2`. `Bar` doit utiliser `Do1` de `Foo`, mais il n'a pas besoin de `Do2` ou a besoin d'une fonctionnalité équivalente à `Do2` mais fait quelque chose de complètement différent.

**Mauvaise méthode** : rendez `Do2()` sur `Foo` virtuel puis remplacez-le dans `Bar` ou simplement `throw Exception` dans `Bar` pour `Do2()`

    public class Bar : Foo
    {
        public override void Do2()
        {
            //Does something completely different that you would expect Foo to do
            //or simply throws new Exception 
        }
    }

**Bonne façon**

Retirez `Do1()` de `Foo` et mettez-le dans la nouvelle classe `Baz` puis héritez à la fois de `Foo` et `Bar` de `Baz` et implémentez `Do2()` séparément

    public class Baz
    {
        public void Do1()
        {
            // magic
        }
    }

    public class Foo : Baz
    {
        public void Do2()
        {
            // foo way
        }
    }

    public class Bar : Baz
    {
        public void Do2()
        {
            // bar way or not have Do2 at all
        }
    }

Maintenant, pourquoi le premier exemple est mauvais et le second est bon : lorsque le développeur nr2 doit modifier `Foo`, il y a de fortes chances qu'il interrompe l'implémentation de `Bar` car `Bar` est désormais inséparable de `Foo`. Lorsque vous le faites par le dernier exemple, les points communs `Foo` et `Bar` ont été déplacés vers `Baz` et ils ne s'affectent pas (comme il ne devrait pas).


## Héritage des méthodes
Il existe plusieurs façons d'hériter des méthodes

    public abstract class Car
    {
        public void HonkHorn() {
            // Implementation of horn being honked
        }

        // virtual methods CAN be overridden in derived classes
        public virtual void ChangeGear() {
            // Implementation of gears being changed
        }

        // abstract methods MUST be overridden in derived classes
        public abstract void Accelerate();
    }

    public class Mustang : Car
    {
        // Before any code is added to the Mustang class, it already contains 
        // implementations of HonkHorn and ChangeGear.

        // In order to compile, it must be given an implementation of Accelerate,
        // this is done using the override keyword
        public override void Accelerate() {
            // Implementation of Mustang accelerating
        }

        // If the Mustang changes gears differently to the implementation in Car
        // this can be overridden using the same override keyword as above
        public override void ChangeGear() {
            // Implementation of Mustang changing gears
        }
    }

## Classe de base avec spécification de type récursif
Définition unique d'une classe de base générique avec un spécificateur de type récursif. Chaque nœud a un parent et plusieurs enfants.

    /// <summary>
    /// Generic base class for a tree structure
    /// </summary>
    /// <typeparam name="T">The node type of the tree</typeparam>
    public abstract class Tree<T> where T : Tree<T>
    {
        /// <summary>
        /// Constructor sets the parent node and adds this node to the parent's child nodes
        /// </summary>
        /// <param name="parent">The parent node or null if a root</param>
        protected Tree(T parent)
        {
            this.Parent=parent;
            this.Children=new List<T>();
            if(parent!=null)
            {
                parent.Children.Add(this as T);
            }
        }
        public T Parent { get; private set; }
        public List<T> Children { get; private set; }
        public bool IsRoot { get { return Parent==null; } }
        public bool IsLeaf { get { return Children.Count==0; } }
        /// <summary>
        /// Returns the number of hops to the root object
        /// </summary>
        public int Level { get { return IsRoot ? 0 : Parent.Level+1; } }
    }

Ce qui précède peut être réutilisé chaque fois qu'une arborescence d'objets doit être définie. L'objet nœud dans l'arborescence doit hériter de la classe de base avec

    public class MyNode : Tree<MyNode>
    {
        // stuff
    }

chaque classe de nœud sait où elle se trouve dans la hiérarchie, quel est l'objet parent ainsi que quels sont les objets enfants. Plusieurs types intégrés utilisent une structure arborescente, comme `Control` ou `XmlElement` et le `Tree<T>` ci-dessus peut être utilisé comme classe de base de _tout_ type dans votre code.


----------


Par exemple, pour créer une hiérarchie de pièces où le poids total est calculé à partir du poids de _tous_ les enfants, procédez comme suit :

    public class Part : Tree<Part>
    {
        public static readonly Part Empty = new Part(null) { Weight=0 };
        public Part(Part parent) : base(parent) { }
        public Part Add(float weight)
        {
            return new Part(this) { Weight=weight };
        }
        public float Weight { get; set; }

        public float TotalWeight { get { return Weight+Children.Sum((part) => part.TotalWeight); } }
    }

être utilisé comme

    // [Q:2.5] -- [P:4.2] -- [R:0.4]
    //    \
    //      - [Z:0.8]
    var Q = Part.Empty.Add(2.5f);
    var P = Q.Add(4.2f);
    var R = P.Add(0.4f);
    var Z = Q.Add(0.9f);
    
    // 2.5+(4.2+0.4)+0.9 = 8.0
    float weight = Q.TotalWeight;


----------


Un autre exemple serait dans la définition des cadres de coordonnées relatives. Dans ce cas, la vraie position du cadre de coordonnées dépend des positions de _tous_ les cadres de coordonnées parents.

    public class RelativeCoordinate : Tree<RelativeCoordinate>
    {
        public static readonly RelativeCoordinate Start = new RelativeCoordinate(null, PointF.Empty) { };
        public RelativeCoordinate(RelativeCoordinate parent, PointF local_position)
            : base(parent)
        {
            this.LocalPosition=local_position;
        }
        public PointF LocalPosition { get; set; }
        public PointF GlobalPosition
        {
            get
            {
                if(IsRoot) return LocalPosition;
                var parent_pos = Parent.GlobalPosition;
                return new PointF(parent_pos.X+LocalPosition.X, parent_pos.Y+LocalPosition.Y);
            }
        }
        public float TotalDistance
        {
            get
            {
                float dist = (float)Math.Sqrt(LocalPosition.X*LocalPosition.X+LocalPosition.Y*LocalPosition.Y);
                return IsRoot ? dist : Parent.TotalDistance+dist;
            }
        }
        public RelativeCoordinate Add(PointF local_position)
        {
            return new RelativeCoordinate(this, local_position);
        }
        public RelativeCoordinate Add(float x, float y)
        {
            return Add(new PointF(x, y));
        }
    }

être utilisé comme

    // Define the following coordinate system hierarchy
    //
    // o--> [A1] --+--> [B1] -----> [C1]
    //             |     
    //             +--> [B2] --+--> [C2]
    //                         |
    //                         +--> [C3]
    
    var A1 = RelativeCoordinate.Start;
    var B1 = A1.Add(100, 20);
    var B2 = A1.Add(160, 10);
    
    var C1 = B1.Add(120, -40);
    var C2 = B2.Add(80, -20);
    var C3 = B2.Add(60, -30);
    
    double dist1 = C1.TotalDistance;



