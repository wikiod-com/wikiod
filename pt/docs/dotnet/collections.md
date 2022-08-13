---
title: "Coleções"
slug: "colecoes"
draft: false
images: []
weight: 9966
type: docs
toc: true
---

Existem vários tipos de coleção:

- `Matriz`
- `Lista`
- `Fila`
- `Lista Ordenada`
- `Pilha`
- [Dicionário][1]


[1]: https://www.wikiod.com/pt/dotnet/dicionarios

## Usando inicializadores de coleção
Alguns tipos de coleção podem ser inicializados no momento da declaração. Por exemplo, a instrução a seguir cria e inicializa os `numbers` com alguns números inteiros:

    List<int> numbers = new List<int>(){10, 9, 8, 7, 7, 6, 5, 10, 4, 3, 2, 1};

Internamente, o compilador C# realmente converte essa inicialização em uma série de chamadas para o método Add. Conseqüentemente, você pode usar esta sintaxe apenas para coleções que realmente suportam o método `Add`.

>As classes `Stack<T>` e `Queue<T>` não o suportam.

Para coleções complexas, como a classe `Dictionary<TKey, TValue>`, que aceita pares de chave/valor, você pode especificar cada par de chave/valor como um tipo anônimo na lista de inicializadores.

    Dictionary<int, string> employee = new Dictionary<int, string>()
         {{44, "John"}, {45, "Bob"}, {47, "James"}, {48, "Franklin"}};
O primeiro item em cada par é a chave e o segundo é o valor.

## Criando uma lista inicializada com tipos personalizados
    public class Model
    {
        public string Name { get; set; }
        public bool? Selected { get; set; }
    }

Aqui temos uma classe sem construtor com duas propriedades: `Name` e uma propriedade booleana anulável `Selected`. Se quisermos inicializar um `List<Model>`, existem algumas maneiras diferentes de executar isso.

    var SelectedEmployees = new List<Model>
     {
          new Model() {Name = "Item1", Selected = true},
          new Model() {Name = "Item2", Selected = false},
          new Model() {Name = "Item3", Selected = false},
          new Model() {Name = "Item4"}
     };

Aqui, estamos criando várias instâncias `novas` de nossa classe `Model` e inicializando-as com dados. E se adicionarmos um construtor?

    public class Model
    {
    
        public Model(string name, bool? selected = false)
        {
            Name = name;
            selected = Selected;
        }
        public string Name { get; set; }
        public bool? Selected { get; set; }
    }

Isso nos permite inicializar nossa Lista um *pouco* diferente.

    var SelectedEmployees = new List<Model>
    {
        new Model("Mark", true),
        new Model("Alexis"),
        new Model("")
    };

E quanto a uma classe onde uma das propriedades é uma classe em si?

    public class Model
    {
        public string Name { get; set; }
        public bool? Selected { get; set; }
    }
    
    public class ExtendedModel : Model
    {
        public ExtendedModel()
        {
            BaseModel = new Model();
        }
    
        public Model BaseModel { get; set; }
        public DateTime BirthDate { get; set; }
    }

Observe que revertemos o construtor na classe `Model` para simplificar um pouco o exemplo.

    var SelectedWithBirthDate = new List<ExtendedModel>
    {
        new ExtendedModel()
        {
            BaseModel = new Model { Name = "Mark", Selected = true},
            BirthDate = new DateTime(2015, 11, 23)
        },
                        new ExtendedModel()
        {
            BaseModel = new Model { Name = "Random"},
            BirthDate = new DateTime(2015, 11, 23)
        }
    };

Observe que podemos trocar nosso `List<ExtendedModel>` com `Collection<ExtendedModel>`, `ExtendedModel[]`, `object[]`, ou mesmo simplesmente `[]`.

## Fila
Existe uma coleção em .Net usada para gerenciar valores em uma [`Queue`][1] que usa o conceito [FIFO (first-in first-out)][2]. O básico das filas é o método [`Enqueue(T item)`][3] que é usado para adicionar elementos na fila e [`Dequeue()`][4] que é usado para obter o primeiro elemento e removê-lo da fila. A versão genérica pode ser usada como o código a seguir para uma fila de strings.

Primeiro, adicione o namespace:

    using System.Collections.Generic;

e use-o:

    Queue<string> queue = new Queue<string>();
    queue.Enqueue("John");
    queue.Enqueue("Paul");
    queue.Enqueue("George");
    queue.Enqueue("Ringo");
    
    string dequeueValue;
    dequeueValue = queue.Dequeue(); // return John
    dequeueValue = queue.Dequeue(); // return Paul
    dequeueValue = queue.Dequeue(); // return George
    dequeueValue = queue.Dequeue(); // return Ringo

Existe uma versão não genérica do tipo, que funciona com objetos.

O espaço de nomes é:

    using System.Collections;

Adicione uma amostra de código para uma fila não genérica:

    Queue queue = new Queue();
    queue.Enqueue("Hello World"); // string
    queue.Enqueue(5); // int
    queue.Enqueue(1d); // double
    queue.Enqueue(true); // bool
    queue.Enqueue(new Product()); // Product object
    
    object dequeueValue;
    dequeueValue = queue.Dequeue(); // return Hello World (string)
    dequeueValue = queue.Dequeue(); // return 5 (int)
    dequeueValue = queue.Dequeue(); // return 1d (double)
    dequeueValue = queue.Dequeue(); // return true (bool)
    dequeueValue = queue.Dequeue(); // return Product (Product type)

Existe também um método chamado [Peek()][5] que retorna o objeto no início da fila sem remover os elementos.

    Queue<int> queue = new Queue<int>();
    queue.Enqueue(10);
    queue.Enqueue(20);
    queue.Enqueue(30);
    queue.Enqueue(40);
    queue.Enqueue(50);

    foreach (int element in queue)
    {
        Console.WriteLine(i);
    }
    
A saída (sem remover):

    10
    20
    30
    40
    50

[1]: https://msdn.microsoft.com/library/system.collections.queue(v=vs.110).aspx
[2]: https://en.wikipedia.org/wiki/FIFO_(computing_and_electronics)
[3]: https://msdn.microsoft.com/library/t249c2y7(v=vs.110).aspx
[4]: https://msdn.microsoft.com/library/1c8bzx97(v=vs.110).aspx
[5]: https://msdn.microsoft.com/library/system.collections.queue.peek(v=vs.110).aspx

## Pilha
Existe uma coleção em .Net usada para gerenciar valores em um [`Stack`][1] que usa o conceito [LIFO (last-in first-out)][2]. O básico das pilhas é o método [`Push(T item)`][3] que é usado para adicionar elementos na pilha e [`Pop()`][4] que é usado para obter o último elemento adicionado e remover isso da pilha. A versão genérica pode ser usada como o código a seguir para uma fila de strings.

Primeiro, adicione o namespace:

    using System.Collections.Generic;

e use-o:

    Stack<string> stack = new Stack<string>();
    stack.Push("John");
    stack.Push("Paul");
    stack.Push("George");
    stack.Push("Ringo");
    
    string value;
    value = stack.Pop(); // return Ringo
    value = stack.Pop(); // return George
    value = stack.Pop(); // return Paul
    value = stack.Pop(); // return John

Existe uma versão não genérica do tipo, que funciona com objetos.

O espaço de nomes é:

    using System.Collections;

E uma amostra de código de pilha não genérica:

    Stack stack = new Stack();
    stack.Push("Hello World"); // string
    stack.Push(5); // int
    stack.Push(1d); // double
    stack.Push(true); // bool
    stack.Push(new Product()); // Product object
    
    object value;
    value = stack.Pop(); // return Product (Product type)
    value = stack.Pop(); // return true (bool)
    value = stack.Pop(); // return 1d (double)
    value = stack.Pop(); // return 5 (int)
    value = stack.Pop(); // return Hello World (string)

Existe também um método chamado [Peek()][5] que retorna o último elemento adicionado mas sem removê-lo do `Stack`.

    Stack<int> stack = new Stack<int>();
    stack.Push(10);
    stack.Push(20);
    
    var lastValueAdded = stack.Peek(); // 20

É possível iterar nos elementos da pilha e respeitará a ordem da pilha (LIFO).

    Stack<int> stack = new Stack<int>();
    stack.Push(10);
    stack.Push(20);
    stack.Push(30);
    stack.Push(40);
    stack.Push(50);

    foreach (int element in stack)
    {
       Console.WriteLine(element);
    }
    
A saída (sem remover):

    50
    40
    30
    20
    10
    


[1]: https://msdn.microsoft.com/library/system.collections.stack(v=vs.110).aspx
[2]: https://en.wikipedia.org/wiki/Stack_(abstract_data_type)
[3]: https://msdn.microsoft.com/library/system.collections.stack.push(v=vs.110).aspx
[4]: https://msdn.microsoft.com/library/system.collections.stack.pop(v=vs.110).aspx
[5]: https://msdn.microsoft.com/library/system.collections.stack.peek(v=vs.110).aspx

