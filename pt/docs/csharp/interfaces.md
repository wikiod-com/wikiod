---
title: "Interfaces"
slug: "interfaces"
draft: false
images: []
weight: 9819
type: docs
toc: true
---

## Implementando uma interface
Uma interface é usada para impor a presença de um método em qualquer classe que o 'implementa'. A interface é definida com a palavra-chave `interface` e uma classe pode 'implementá-la' adicionando `: InterfaceName` após o nome da classe. Uma classe pode implementar várias interfaces separando cada interface com uma vírgula.
` : InterfaceName, ISecondInterface`
    
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

Como eles implementam o `INoiseMaker`, tanto `cat` quanto `dog` são obrigados a incluir o método `string MakeNoise()` e falharão ao compilar sem ele.

## Implementação de interface explícita
A implementação de interface explícita é necessária quando você implementa várias interfaces que definem um método comum, mas diferentes implementações são necessárias dependendo de qual interface está sendo usada para chamar o método (observe que você não precisa de implementações explícitas se várias interfaces compartilharem o mesmo método e uma implementação comum é possível).

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

A implementação não pode ser chamada de nenhum outro lugar, exceto usando a interface:

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

Devido a isso, pode ser vantajoso colocar código de implementação complexo de uma interface explicitamente implementada em um método privado separado.

Obviamente, uma implementação de interface explícita só pode ser usada para métodos que realmente existem para essa interface:

    public class ProGolfer : IGolfPlayer
    {
        string IGolfPlayer.Swear() // Error
        {
            return "The ball is in the pit";
        }
    }

Da mesma forma, usar uma implementação de interface explícita sem declarar essa interface na classe também causa um erro.

# Dica:
A implementação de interfaces explicitamente também pode ser usada para evitar código morto. Quando um método não é mais necessário e é removido da interface, o compilador reclamará sobre cada implementação ainda existente.

# Observação:

Os programadores esperam que o contrato seja o mesmo, independentemente do contexto do tipo, e a implementação explícita não deve expor comportamentos diferentes quando chamada.
Assim, ao contrário do exemplo acima, `IGolfPlayer.Drive` e `Drive` devem fazer a mesma coisa quando possível.



## Implementando várias interfaces
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

## Noções básicas de interface
A função de uma Interface conhecida como "contrato" de funcionalidade. Isso significa que ele declara propriedades e métodos, mas não os implementa.

Então, ao contrário das classes Interfaces:

- Não pode ser instanciado
- Não pode ter nenhuma funcionalidade
- Só pode conter métodos * *(Propriedades e Eventos são métodos internos)*
- Herdar uma interface é chamado de "Implementação"
- Você pode herdar de 1 classe, mas pode "Implementar" várias interfaces


    public interface ICanDoThis{
        void TheThingICanDo();
        int SomeValueProperty { get; set; }
    }

Coisas a observar:
- O prefixo "I" é uma convenção de nomenclatura usada para interfaces.
- O corpo da função é substituído por um ponto e vírgula ";".
- Propriedades também são permitidas porque internamente também são métodos


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

Isso é especialmente útil quando você está trabalhando com estruturas de interface do usuário, como WinForms ou WPF, porque é obrigatório herdar de uma classe base para criar controle de usuário e você perde a capacidade de criar abstração em diferentes tipos de controle. Um exemplo? Chegando:

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

O problema proposto é que ambos contêm algum conceito de "Texto", mas os nomes das propriedades são diferentes. E você não pode criar uma *classe base abstrata* porque elas têm uma herança obrigatória para 2 classes diferentes. Uma interface pode aliviar isso

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

Agora MyButton e MyTextBlock são intercambiáveis.

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

## IComparable<T> como exemplo de implementação de uma interface
As interfaces podem parecer abstratas até que você as pareça na prática. O `IComparable` e o `IComparable<T>` são ótimos exemplos de como as interfaces podem ser úteis para nós.

Digamos que em um programa para uma loja online, temos uma variedade de itens que você pode comprar. Cada item tem um nome, um número de identificação e um preço.

    public class Item {
        
        public string name; // though public variables are generally bad practice,
        public int idNumber; // to keep this example simple we will use them instead
        public decimal price; // of a property.

        // body omitted for brevity        

    }

Temos nossos `Item`s armazenados dentro de uma `List<Item>`, e em algum lugar do nosso programa, queremos ordenar nossa lista pelo número de ID do menor para o maior. Em vez de escrever nosso próprio algoritmo de ordenação, podemos usar o método `Sort()` que `List<T>` já possui. No entanto, como nossa classe `Item` está agora, não há como o `List<T>` entender em que ordem classificar a lista. Aqui é onde entra a interface `IComparable`.

Para implementar corretamente o método `CompareTo`, `CompareTo` deve retornar um número positivo se o parâmetro for "menor que" o atual, zero se forem iguais e um número negativo se o parâmetro for "maior que".

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

Aqui está o exemplo de implementação do `Item` da interface:

    public class Item : IComparable<Item> {
        
        private string name;
        private int idNumber;
        private decimal price;

        public int CompareTo(Item otherItem) {

            return (this.idNumber - otherItem.idNumber);

        }

        // rest of code omitted for brevity    

    }

Em um nível superficial, o método `CompareTo` em nosso item simplesmente retorna a diferença em seus números de ID, mas o que o acima faz na prática?

Agora, quando chamamos `Sort()` em um objeto `List<Item>`, o `List` chamará automaticamente o método `CompareTo` do `Item` quando precisar determinar em que ordem colocar os objetos. , além de `List<T>`, quaisquer outros objetos que precisem da capacidade de comparar dois objetos funcionarão com o `Item` porque definimos a capacidade de dois `Item`s diferentes serem comparados entre si.

## Por que usamos interfaces
Uma interface é uma definição de um contrato entre o usuário da interface e a classe que a implementa. Uma maneira de pensar em uma interface é como uma declaração de que um objeto pode executar determinadas funções.

Digamos que definimos uma interface `IShape` para representar diferentes tipos de formas, esperamos que uma forma tenha uma área, então definiremos um método para forçar as implementações da interface a retornar sua área:

    public interface IShape
    {
        double ComputeArea();
    }

Vamos ter as duas formas a seguir: um `Retangle` e um `Circle`

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

Cada um deles tem sua própria definição de sua área, mas ambos são formas. Portanto, é lógico vê-los como `IShape` em nosso programa:

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



## "Esconder" membros com implementação explícita
Você não odeia quando interfaces poluem sua classe com muitos membros com os quais você nem se importa? Bem, eu tenho uma solução! Implementações explícitas

    public interface IMessageService {
        void OnMessageRecieve();
        void SendMessage();
        string Result { get; set; }
        int Encoding { get; set; }
        // yadda yadda
    }

Normalmente você implementaria a classe assim.

    public class MyObjectWithMessages : IMessageService {
         public void OnMessageRecieve(){

         }

         public void SendMessage(){

         }

         public string Result { get; set; }
         public int Encoding { get; set; }
    }
Cada membro é público.

    var obj = new MyObjectWithMessages();

    // why would i want to call this function?
    obj.OnMessageRecieve();

Resposta: eu não. Portanto, também não deve ser declarado público
mas simplesmente declarar os membros como privados fará com que o compilador lance um erro



A solução é usar implementação explícita:

    public class MyObjectWithMessages : IMessageService{
        void IMessageService.OnMessageRecieve() {
            
        }

        void IMessageService.SendMessage() {
            
        }

        string IMessageService.Result { get; set; }
        int IMessageService.Encoding { get; set; }
    }

Então agora você implementou os membros conforme necessário e eles não irão expor nenhum membro como público.

    var obj = new MyObjectWithMessages();

    /* error member does not exist on type MyObjectWithMessages. 
     * We've succesfully made it "private" */
    obj.OnMessageRecieve();

Se você ainda quiser acessar o membro, mesmo que esteja implementando explicitamente, tudo o que você precisa fazer é lançar o objeto na interface e pronto.

    ((IMessageService)obj).OnMessageRecieve();

