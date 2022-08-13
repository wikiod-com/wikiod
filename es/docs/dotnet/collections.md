---
title: "Colecciones"
slug: "colecciones"
draft: false
images: []
weight: 9966
type: docs
toc: true
---

Hay varios tipos de colección:

- `Matriz`
- `Lista`
- `Cola`
- `Lista ordenada`
- `Apilar`
- [Diccionario][1]


[1]: https://www.wikiod.com/es/dotnet/diccionarios

## Usar inicializadores de colección
Algunos tipos de colección se pueden inicializar en el momento de la declaración. Por ejemplo, la siguiente instrucción crea e inicializa los `números` con algunos enteros:

    List<int> numbers = new List<int>(){10, 9, 8, 7, 7, 6, 5, 10, 4, 3, 2, 1};

Internamente, el compilador de C# en realidad convierte esta inicialización en una serie de llamadas al método Add. En consecuencia, puede usar esta sintaxis solo para colecciones que realmente admitan el método `Add`.

>Las clases `Stack<T>` y `Queue<T>` no lo admiten.

Para colecciones complejas como la clase `Dictionary<TKey, TValue>`, que acepta pares clave/valor, puede especificar cada par clave/valor como un tipo anónimo en la lista de inicializadores.

    Dictionary<int, string> employee = new Dictionary<int, string>()
         {{44, "John"}, {45, "Bob"}, {47, "James"}, {48, "Franklin"}};
El primer elemento de cada par es la clave y el segundo es el valor.

## Creación de una lista inicializada con tipos personalizados
    public class Model
    {
        public string Name { get; set; }
        public bool? Selected { get; set; }
    }

Aquí tenemos una Clase sin constructor con dos propiedades: `Nombre` y una propiedad booleana anulable `Selected`. Si quisiéramos inicializar una `Lista<Modelo>`, hay algunas formas diferentes de ejecutar esto.

    var SelectedEmployees = new List<Model>
     {
          new Model() {Name = "Item1", Selected = true},
          new Model() {Name = "Item2", Selected = false},
          new Model() {Name = "Item3", Selected = false},
          new Model() {Name = "Item4"}
     };

Aquí, estamos creando varias instancias 'nuevas' de nuestra clase 'Modelo' e inicializándolas con datos. ¿Qué pasa si agregamos un constructor?

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

Esto nos permite inicializar nuestra Lista un *poco* diferente.

    var SelectedEmployees = new List<Model>
    {
        new Model("Mark", true),
        new Model("Alexis"),
        new Model("")
    };

¿Qué pasa con una clase donde una de las propiedades es una clase en sí misma?

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

Observe que revertimos el constructor en la clase `Modelo` para simplificar un poco el ejemplo.

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

Tenga en cuenta que podemos intercambiar nuestra `List<ExtendedModel>` con `Collection<ExtendedModel>`, `ExtendedModel[]`, `object[]`, o incluso simplemente `[]`.

## Cola
Hay una colección en .Net que se usa para administrar valores en una [`Cola`][1] que usa el concepto [FIFO (primero en entrar, primero en salir)][2]. Lo básico de las colas es el método [`Enqueue(T item)`][3] que se usa para agregar elementos en la cola y [`Dequeue()`][4] que se usa para obtener el primer elemento y eliminarlo de la cola La versión genérica se puede usar como el siguiente código para una cola de cadenas.

Primero, agregue el espacio de nombres:

    using System.Collections.Generic;

y úsalo:

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

Existe una versión no genérica del tipo, que funciona con objetos.

El espacio de nombres es:

    using System.Collections;

Adn una muestra de código para una cola no genérica:

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

También hay un método llamado [Peek()][5] que devuelve el objeto al principio de la cola sin quitarle los elementos.

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
    
La salida (sin quitar):

    10
    20
    30
    40
    50

[1]: https://msdn.microsoft.com/library/system.collections.queue(v=vs.110).aspx
[2]: https://en.wikipedia.org/wiki/FIFO_(informática_y_electrónica)
[3]: https://msdn.microsoft.com/library/t249c2y7(v=vs.110).aspx
[4]: https://msdn.microsoft.com/library/1c8bzx97(v=vs.110).aspx
[5]: https://msdn.microsoft.com/library/system.collections.queue.peek(v=vs.110).aspx

## Pila
Hay una colección en .Net que se usa para administrar valores en una [`Stack`][1] que usa el concepto [LIFO (last-in first-out)][2]. Los conceptos básicos de las pilas son el método [`Push(T item)`][3] que se usa para agregar elementos en la pila y [`Pop()`][4] que se usa para agregar y eliminar el último elemento de la pila. La versión genérica se puede usar como el siguiente código para una cola de cadenas.

Primero, agregue el espacio de nombres:

    using System.Collections.Generic;

y úsalo:

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

Existe una versión no genérica del tipo, que funciona con objetos.

El espacio de nombres es:

    using System.Collections;

Y una muestra de código de pila no genérica:

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

También hay un método llamado [Peek()][5] que devuelve el último elemento añadido pero sin eliminarlo de la `Pila`.

    Stack<int> stack = new Stack<int>();
    stack.Push(10);
    stack.Push(20);
    
    var lastValueAdded = stack.Peek(); // 20

Es posible iterar sobre los elementos de la pila y se respetará el orden de la pila (LIFO).

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
    
La salida (sin quitar):

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

