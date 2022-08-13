---
title: "Interfaces"
slug: "interfaces"
draft: false
images: []
weight: 9819
type: docs
toc: true
---

## Implémentation d'une interface
Une interface est utilisée pour imposer la présence d'une méthode dans toute classe qui "l'implémente". L'interface est définie avec le mot clé `interface` et une classe peut l'implémenter en ajoutant `: InterfaceName` après le nom de la classe. Une classe peut implémenter plusieurs interfaces en séparant chaque interface par une virgule.
` : NomInterface, ISecondeInterface`
    
    public interface INoiseMaker
    {
        string MakeNoise();
    }

    public class Cat : INoiseMaker
    {
        public string MakeNoise()
        {
            return "Nyan";
        }
    }

    public class Dog : INoiseMaker
    {
        public string MakeNoise()
        {
            return "Woof";
        }
    }

Parce qu'ils implémentent `INoiseMaker`, `cat` et `dog` doivent inclure la méthode `string MakeNoise()` et échoueront à se compiler sans elle.

## Implémentation d'interface explicite
L'implémentation d'interface explicite est nécessaire lorsque vous implémentez plusieurs interfaces qui définissent une méthode commune, mais différentes implémentations sont requises selon l'interface utilisée pour appeler la méthode (notez que vous n'avez pas besoin d'implémentations explicites si plusieurs interfaces partagent la même méthode et une implémentation commune est possible).

    interface IChauffeur 
    {
        string Drive();
    }

    interface IGolfPlayer
    {
        string Drive();
    }

    class GolfingChauffeur : IChauffeur, IGolfPlayer 
    {
        public string Drive()
        {
            return "Vroom!";
        }

        string IGolfPlayer.Drive()
        {
            return "Took a swing...";
        }
    }


    GolfingChauffeur obj = new GolfingChauffeur();
    IChauffeur chauffeur = obj;
    IGolfPlayer golfer = obj;
    
    Console.WriteLine(obj.Drive()); // Vroom!
    Console.WriteLine(chauffeur.Drive()); // Vroom!
    Console.WriteLine(golfer.Drive()); // Took a swing...

L'implémentation ne peut pas être appelée depuis n'importe où, sauf en utilisant l'interface :

    public class Golfer : IGolfPlayer
    {
        string IGolfPlayer.Drive()
        {
            return "Swinging hard...";
        }
        public void Swing()
        {
            Drive(); // Compiler error: No such method
        }
    }

Pour cette raison, il peut être avantageux de placer le code d'implémentation complexe d'une interface explicitement implémentée dans une méthode privée séparée.

Une implémentation d'interface explicite ne peut bien sûr être utilisée que pour les méthodes qui existent réellement pour cette interface :

    public class ProGolfer : IGolfPlayer
    {
        string IGolfPlayer.Swear() // Error
        {
            return "The ball is in the pit";
        }
    }

De même, l'utilisation d'une implémentation d'interface explicite sans déclarer cette interface sur la classe provoque également une erreur.

# Indice:
L'implémentation explicite d'interfaces peut également être utilisée pour éviter le code mort. Lorsqu'une méthode n'est plus nécessaire et est supprimée de l'interface, le compilateur se plaindra de chaque implémentation encore existante.

# Noter:

Les programmeurs s'attendent à ce que le contrat soit le même quel que soit le contexte du type et l'implémentation explicite ne doit pas exposer un comportement différent lorsqu'il est appelé.
Ainsi, contrairement à l'exemple ci-dessus, `IGolfPlayer.Drive` et `Drive` doivent faire la même chose lorsque cela est possible.



## Implémentation de plusieurs interfaces
    public interface IAnimal 
    {
        string Name { get; set; }
    }
    
    public interface INoiseMaker
    {
        string MakeNoise();
    }
    
    public class Cat : IAnimal, INoiseMaker
    {
        public Cat() 
        {
            Name = "Cat";
        }
    
        public string Name { get; set; }

        public string MakeNoise()
        {
            return "Nyan";
        }
    }

## Bases de l'interface
Une fonction d'Interface connue sous le nom de "contrat" ​​de fonctionnalité. Cela signifie qu'il déclare des propriétés et des méthodes mais qu'il ne les implémente pas.

Donc contrairement aux classes Interfaces :

- Ne peut pas être instancié
- Ne peut avoir aucune fonctionnalité
- Ne peut contenir que des méthodes * *(Les propriétés et les événements sont des méthodes en interne)*
- L'héritage d'une interface s'appelle "Implémenter"
- Vous pouvez hériter d'une classe, mais vous pouvez "implémenter" plusieurs interfaces


    public interface ICanDoThis{
        void TheThingICanDo();
        int SomeValueProperty { get; set; }
    }

A noter :
- Le préfixe "I" est une convention de nommage utilisée pour les interfaces.
- Le corps de la fonction est remplacé par un point virgule ";".
- Les propriétés sont également autorisées car en interne ce sont aussi des méthodes


    public class MyClass : ICanDoThis {
        public void TheThingICanDo(){
            // do the thing
        }

        public int SomeValueProperty { get; set; }
        public int SomeValueNotImplemtingAnything { get; set; }
    }

.

    ICanDoThis obj = new MyClass();

    // ok
    obj.TheThingICanDo();

    // ok
    obj.SomeValueProperty = 5;

    // Error, this member doesn't exist in the interface
    obj.SomeValueNotImplemtingAnything = 5;

    // in order to access the property in the class you must "down cast" it
    ((MyClass)obj).SomeValueNotImplemtingAnything = 5; // ok

Ceci est particulièrement utile lorsque vous travaillez avec des frameworks d'interface utilisateur tels que WinForms ou WPF, car il est obligatoire d'hériter d'une classe de base pour créer un contrôle utilisateur et vous perdez la possibilité de créer une abstraction sur différents types de contrôle. Un exemple? À venir :

    public class MyTextBlock : TextBlock {
        public void SetText(string str){
            this.Text = str;
        }
    }

    public class MyButton : Button {
        public void SetText(string str){
            this.Content = str;
        }
    }

Le problème proposé est que les deux contiennent un concept de "Texte" mais que les noms de propriété diffèrent. Et vous ne pouvez pas créer une * classe de base abstraite * car elles ont un héritage obligatoire sur 2 classes différentes. Une interface peut atténuer cela

    public interface ITextControl{
        void SetText(string str);
    }

    public class MyTextBlock : TextBlock, ITextControl {
        public void SetText(string str){
            this.Text = str;
        }
    }

    public class MyButton : Button, ITextControl {
        public void SetText(string str){
            this.Content = str;
        }

        public int Clicks { get; set; }
    }

Maintenant, MyButton et MyTextBlock sont interchangeables.

    var controls = new List<ITextControls>{
        new MyTextBlock(),
        new MyButton()
    };

    foreach(var ctrl in controls){
        ctrl.SetText("This text will be applied to both controls despite them being different");


        // Compiler Error, no such member in interface
        ctrl.Clicks = 0;

        // Runtime Error because 1 class is in fact not a button which makes this cast invalid
        ((MyButton)ctrl).Clicks = 0;


        /* the solution is to check the type first.
        This is usually considered bad practice since
        it's a symptom of poor abstraction */
        var button = ctrl as MyButton;
        if(button != null)
            button.Clicks = 0; // no errors

       
    }

## IComparable<T> comme exemple d'implémentation d'une interface
Les interfaces peuvent sembler abstraites jusqu'à ce que vous les voyiez dans la pratique. `IComparable` et `IComparable<T>` sont d'excellents exemples de la raison pour laquelle les interfaces peuvent nous être utiles.

Disons que dans un programme pour une boutique en ligne, nous avons une variété d'articles que vous pouvez acheter. Chaque article a un nom, un numéro d'identification et un prix.

    public class Item {
        
        public string name; // though public variables are generally bad practice,
        public int idNumber; // to keep this example simple we will use them instead
        public decimal price; // of a property.

        // body omitted for brevity        

    }

Nous avons nos `Item`s stockés dans une `List<Item>`, et dans notre programme quelque part, nous voulons trier notre liste par numéro d'identification du plus petit au plus grand. Au lieu d'écrire notre propre algorithme de tri, nous pouvons utiliser la méthode `Sort()` que `List<T>` possède déjà. Cependant, comme notre classe `Item` est en ce moment, il n'y a aucun moyen pour `List<T>` de comprendre dans quel ordre trier la liste. C'est ici qu'intervient l'interface `IComparable`.

Pour implémenter correctement la méthode `CompareTo`, `CompareTo` doit renvoyer un nombre positif si le paramètre est "inférieur à" l'actuel, zéro s'ils sont égaux et un nombre négatif si le paramètre est "supérieur à".

    Item apple = new Item();
    apple.idNumber = 15;
    Item banana = new Item();
    banana.idNumber = 4;
    Item cow = new Item();
    cow.idNumber = 15;
    Item diamond = new Item();
    diamond.idNumber = 18;

    Console.WriteLine(apple.CompareTo(banana)); // 11
    Console.WriteLine(apple.CompareTo(cow)); // 0
    Console.WriteLine(apple.CompareTo(diamond)); // -3

Voici l'exemple d'implémentation de l'interface `Item` :

    public class Item : IComparable<Item> {
        
        private string name;
        private int idNumber;
        private decimal price;

        public int CompareTo(Item otherItem) {

            return (this.idNumber - otherItem.idNumber);

        }

        // rest of code omitted for brevity    

    }

Au niveau de la surface, la méthode "CompareTo" de notre élément renvoie simplement la différence entre leurs numéros d'identification, mais que fait ce qui précède en pratique ?

Désormais, lorsque nous appelons `Sort()` sur un objet `List<Item>`, la `List` appellera automatiquement la méthode `CompareTo` de `Item` lorsqu'elle doit déterminer l'ordre dans lequel placer les objets. De plus , en plus de `List<T>`, tout autre objet nécessitant la possibilité de comparer deux objets fonctionnera avec `Item` car nous avons défini la possibilité pour deux `Item` différents d'être comparés l'un à l'autre.

## Pourquoi utilisons-nous des interfaces ?
Une interface est une définition d'un contrat entre l'utilisateur de l'interface et la classe qui l'implémente. Une façon de penser à une interface est comme une déclaration selon laquelle un objet peut exécuter certaines fonctions.

Disons que nous définissons une interface `IShape` pour représenter différents types de formes, nous nous attendons à ce qu'une forme ait une aire, nous allons donc définir une méthode pour forcer les implémentations d'interface à retourner leur aire :

    public interface IShape
    {
        double ComputeArea();
    }

Supposons que nous ayons les deux formes suivantes : un `Rectangle` et un `Cercle`

    public class Rectangle : IShape
    {
        private double length;
        private double width;

        public Rectangle(double length, double width)
        {
            this.length = length;
            this.width = width;
        }

        public double ComputeArea()
        {
            return length * width;
        }
    }

    public class Circle : IShape
    {
        private double radius;

        public Circle(double radius)
        {
            this.radius = radius;
        }

        public double ComputeArea()
        {
            return Math.Pow(radius, 2.0) * Math.PI;
        }
    }

Chacun d'eux a sa propre définition de sa zone, mais les deux sont des formes. Il est donc logique de les voir comme `IShape` dans notre programme :

    private static void Main(string[] args)
    {
        var shapes = new List<IShape>() { new Rectangle(5, 10), new Circle(5) };
        ComputeArea(shapes);

        Console.ReadKey();
    }

    private static void ComputeArea(IEnumerable<IShape> shapes) 
    {
        foreach (shape in shapes)
        {
            Console.WriteLine("Area: {0:N}, shape.ComputeArea());
        }
    }

    // Output:
    // Area : 50.00
    // Area : 78.54



## "Masquer" les membres avec une implémentation explicite
Ne détestez-vous pas que les interfaces polluent votre classe avec trop de membres dont vous ne vous souciez même pas ? Bon j'ai une solution ! Implémentations explicites

    public interface IMessageService {
        void OnMessageRecieve();
        void SendMessage();
        string Result { get; set; }
        int Encoding { get; set; }
        // yadda yadda
    }

Normalement, vous implémenteriez la classe comme ceci.

    public class MyObjectWithMessages : IMessageService {
         public void OnMessageRecieve(){

         }

         public void SendMessage(){

         }

         public string Result { get; set; }
         public int Encoding { get; set; }
    }
Chaque membre est public.

    var obj = new MyObjectWithMessages();

    // why would i want to call this function?
    obj.OnMessageRecieve();

Réponse : Je ne sais pas. Il ne doit donc pas non plus être déclaré public
mais déclarer simplement les membres comme privés fera que le compilateur lancera une erreur



La solution consiste à utiliser une implémentation explicite :

    public class MyObjectWithMessages : IMessageService{
        void IMessageService.OnMessageRecieve() {
            
        }

        void IMessageService.SendMessage() {
            
        }

        string IMessageService.Result { get; set; }
        int IMessageService.Encoding { get; set; }
    }

Alors maintenant, vous avez implémenté les membres comme requis et ils n'exposeront aucun membre en tant que public.

    var obj = new MyObjectWithMessages();

    /* error member does not exist on type MyObjectWithMessages. 
     * We've succesfully made it "private" */
    obj.OnMessageRecieve();

Si vous souhaitez toujours accéder au membre même s'il est explicitement implémenté, tout ce que vous avez à faire est de convertir l'objet en interface et vous êtes prêt à partir.

    ((IMessageService)obj).OnMessageRecieve();

