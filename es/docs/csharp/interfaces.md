---
title: "Interfaces"
slug: "interfaces"
draft: false
images: []
weight: 9819
type: docs
toc: true
---

## Implementando una interfaz
Una interfaz se utiliza para imponer la presencia de un método en cualquier clase que lo 'implemente'. La interfaz se define con la palabra clave "interfaz" y una clase puede "implementarla" agregando ": NombreInterfaz" después del nombre de la clase. Una clase puede implementar múltiples interfaces separando cada interfaz con una coma.
` : NombreInterfaz, ISegundaInterfaz`
    
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

Debido a que implementan `INoiseMaker`, tanto `cat` como `dog` deben incluir el método `string MakeNoise()` y no se podrán compilar sin él.

## Implementación de interfaz explícita
La implementación de interfaz explícita es necesaria cuando implementa varias interfaces que definen un método común, pero se requieren diferentes implementaciones según la interfaz que se utilice para llamar al método (tenga en cuenta que no necesita implementaciones explícitas si varias interfaces comparten el mismo método y es posible una implementación común).

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

La implementación no se puede llamar desde ningún otro lugar, excepto mediante la interfaz:

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

Debido a esto, puede ser ventajoso colocar el código de implementación complejo de una interfaz implementada explícitamente en un método privado separado.

Por supuesto, una implementación de interfaz explícita solo puede usarse para métodos que realmente existen para esa interfaz:

    public class ProGolfer : IGolfPlayer
    {
        string IGolfPlayer.Swear() // Error
        {
            return "The ball is in the pit";
        }
    }

De manera similar, usar una implementación de interfaz explícita sin declarar esa interfaz en la clase también genera un error.

# Insinuación:
La implementación explícita de interfaces también se puede usar para evitar el código muerto. Cuando un método ya no es necesario y se elimina de la interfaz, el compilador se quejará de cada implementación aún existente.

# Nota:

Los programadores esperan que el contrato sea el mismo independientemente del contexto del tipo y la implementación explícita no debe exponer un comportamiento diferente cuando se le llama.
Entonces, a diferencia del ejemplo anterior, `IGolfPlayer.Drive` y `Drive` deberían hacer lo mismo cuando sea posible.



## Implementando múltiples interfaces
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

## Conceptos básicos de la interfaz
Una función de interfaz conocida como "contrato" de funcionalidad. Significa que declara propiedades y métodos pero no los implementa.

Entonces, a diferencia de las interfaces de clases:

- No se puede instanciar
- No puede tener ninguna funcionalidad.
- Solo puede contener métodos * *(Properties y Events son métodos internamente)*
- Heredar una interfaz se llama "Implementar"
- Puede heredar de 1 clase, pero puede "Implementar" múltiples interfaces


    public interface ICanDoThis{
        void TheThingICanDo();
        int SomeValueProperty { get; set; }
    }

Cosas a notar:
- El prefijo "I" es una convención de nomenclatura utilizada para las interfaces.
- El cuerpo de la función se reemplaza con un punto y coma ";".
- Las propiedades también están permitidas porque internamente también son métodos.


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

Esto es especialmente útil cuando trabaja con marcos de interfaz de usuario como WinForms o WPF porque es obligatorio heredar de una clase base para crear un control de usuario y pierde la capacidad de crear abstracción sobre diferentes tipos de control. ¿Un ejemplo? Subiendo:

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

El problema propuesto es que ambos contienen algún concepto de "Texto" pero los nombres de las propiedades difieren. Y no puede crear una * clase base abstracta * porque tienen una herencia obligatoria para 2 clases diferentes. Una interfaz puede aliviar eso

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

Ahora MyButton y MyTextBlock son intercambiables.

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

## IComparable<T> como ejemplo de implementación de una interfaz
Las interfaces pueden parecer abstractas hasta que las pareces en la práctica. `IComparable` e `IComparable<T>` son excelentes ejemplos de por qué las interfaces pueden ser útiles para nosotros.

Digamos que en un programa para una tienda en línea, tenemos una variedad de artículos que puedes comprar. Cada artículo tiene un nombre, un número de identificación y un precio.

    public class Item {
        
        public string name; // though public variables are generally bad practice,
        public int idNumber; // to keep this example simple we will use them instead
        public decimal price; // of a property.

        // body omitted for brevity        

    }

Tenemos nuestros 'Item' almacenados dentro de una 'List<Item>', y en algún lugar de nuestro programa, queremos ordenar nuestra lista por número de ID del más pequeño al más grande. En lugar de escribir nuestro propio algoritmo de clasificación, podemos usar el método `Sort()` que ya tiene `List<T>`. Sin embargo, como nuestra clase `Item` está ahora, no hay forma de que `List<T>` entienda en qué orden ordenar la lista. Aquí es donde entra en juego la interfaz `IComparable`.

Para implementar correctamente el método `CompareTo`, `CompareTo` debería devolver un número positivo si el parámetro es "menor que" el actual, cero si son iguales y un número negativo si el parámetro es "mayor que".

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

Aquí está el ejemplo de implementación de `Item` de la interfaz:

    public class Item : IComparable<Item> {
        
        private string name;
        private int idNumber;
        private decimal price;

        public int CompareTo(Item otherItem) {

            return (this.idNumber - otherItem.idNumber);

        }

        // rest of code omitted for brevity    

    }

En un nivel superficial, el método `CompareTo` en nuestro elemento simplemente devuelve la diferencia en sus números de identificación, pero ¿qué hace lo anterior en la práctica?

Ahora, cuando llamamos a `Sort()` en un objeto `List<Item>`, `List` llamará automáticamente al método `CompareTo` del `Item` cuando necesite determinar en qué orden poner los objetos. Además , además de `List<T>`, cualquier otro objeto que necesite la capacidad de comparar dos objetos funcionará con el `Item` porque hemos definido la capacidad para que dos `Item` diferentes se comparen entre sí.

## Por qué usamos interfaces
Una interfaz es una definición de un contrato entre el usuario de la interfaz y la clase que la implementa. Una forma de pensar en una interfaz es como una declaración de que un objeto puede realizar ciertas funciones.

Digamos que definimos una interfaz `IShape` para representar diferentes tipos de formas, esperamos que una forma tenga un área, así que definiremos un método para obligar a las implementaciones de la interfaz a devolver su área:

    public interface IShape
    {
        double ComputeArea();
    }

Supongamos que tenemos las siguientes dos formas: un 'Rectángulo' y un 'Círculo'

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

Cada uno de ellos tiene su propia definición de su área, pero ambos son formas. Así que es lógico verlos como `IShape` en nuestro programa:

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



## "Ocultar" miembros con implementación explícita
¿No odias cuando las interfaces contaminan tu clase con demasiados miembros que ni siquiera te importan? ¡Pues tengo una solución! Implementaciones explícitas

    public interface IMessageService {
        void OnMessageRecieve();
        void SendMessage();
        string Result { get; set; }
        int Encoding { get; set; }
        // yadda yadda
    }

Normalmente implementarías la clase de esta manera.

    public class MyObjectWithMessages : IMessageService {
         public void OnMessageRecieve(){

         }

         public void SendMessage(){

         }

         public string Result { get; set; }
         public int Encoding { get; set; }
    }
Cada miembro es público.

    var obj = new MyObjectWithMessages();

    // why would i want to call this function?
    obj.OnMessageRecieve();

Respuesta: Yo no. Así que tampoco debe declararse público
pero simplemente declarar a los miembros como privados hará que el compilador arroje un error



La solución es usar una implementación explícita:

    public class MyObjectWithMessages : IMessageService{
        void IMessageService.OnMessageRecieve() {
            
        }

        void IMessageService.SendMessage() {
            
        }

        string IMessageService.Result { get; set; }
        int IMessageService.Encoding { get; set; }
    }

Así que ahora ha implementado los miembros según sea necesario y no expondrán a ningún miembro como público.

    var obj = new MyObjectWithMessages();

    /* error member does not exist on type MyObjectWithMessages. 
     * We've succesfully made it "private" */
    obj.OnMessageRecieve();

Si en serio todavía desea acceder al miembro a pesar de que está implementado explícitamente, todo lo que tiene que hacer es enviar el objeto a la interfaz y listo.

    ((IMessageService)obj).OnMessageRecieve();

