---
title: "Collections"
slug: "collections"
draft: false
images: []
weight: 9966
type: docs
toc: true
---

There are several kinds of collection:

 - `Array`
 - `List`
 - `Queue`
 - `SortedList`
 - `Stack`
 - [Dictionary][1]


  [1]: https://www.wikiod.com/dotnet/dictionaries

## Using collection initializers
Some collection types can be initialized at the declaration time. For example, the following statement creates and initializes the `numbers` with some integers:

    List<int> numbers = new List<int>(){10, 9, 8, 7, 7, 6, 5, 10, 4, 3, 2, 1};

Internally, the C# compiler actually converts this initialization to a series of calls to the Add method. Consequently, you can use this syntax only for collections that actually support the `Add` method. 

>The `Stack<T>` and `Queue<T>` classes do not support it.

For complex collections such as the `Dictionary<TKey, TValue>` class, that take key/value pairs, you can specify each key/value pair as an anonymous type in the initializer list.

    Dictionary<int, string> employee = new Dictionary<int, string>()
         {{44, "John"}, {45, "Bob"}, {47, "James"}, {48, "Franklin"}};
The first item in each pair is the key, and the second is the value.

## Creating an initialized List with Custom Types
    public class Model
    {
        public string Name { get; set; }
        public bool? Selected { get; set; }
    }

Here we have a Class with no constructor with two properties: `Name` and a nullable boolean property `Selected`. If we wanted to initialize a `List<Model>`, there are a few different ways to execute this.

    var SelectedEmployees = new List<Model>
     {
          new Model() {Name = "Item1", Selected = true},
          new Model() {Name = "Item2", Selected = false},
          new Model() {Name = "Item3", Selected = false},
          new Model() {Name = "Item4"}
     };

Here, we are creating several `new` instances of our `Model` class, and initializing them with data. What if we added a constructor?

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

This allows us to initialize our List a *little* differently.

    var SelectedEmployees = new List<Model>
    {
        new Model("Mark", true),
        new Model("Alexis"),
        new Model("")
    };

What about a Class where one of the properties is a class itself? 

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

Notice we reverted the constructor on the `Model` class to simplify the example a little bit.

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

Note that we can interchange our `List<ExtendedModel>` with `Collection<ExtendedModel>`, `ExtendedModel[]`, `object[]`, or even simply `[]`.

## Queue
There is a collection in .Net used to manage values in a [`Queue`][1] that uses the [FIFO (first-in first-out)][2] concept. The basics of queues is the method [`Enqueue(T item)`][3] which is used to add elements in the queue and [`Dequeue()`][4] which is used to get the first element and remove it from the queue. The generic version can be used like the following code for a queue of strings. 

First, add the namespace:

    using System.Collections.Generic;

and use it:

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

There is a non generic version of the type, which works with objects.

The namespace is:

    using System.Collections;

Adn a code sample fo non generic queue:

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

There is also a method called [Peek()][5] which returns the object at the beginning of the queue without removing it the elements.

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
    
The output (without removing):

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

## Stack
There is a collection in .Net used to manage values in a [`Stack`][1] that uses the [LIFO (last-in first-out)][2] concept. The basics of stacks is the method [`Push(T item)`][3] which is used to add elements in the stack and [`Pop()`][4] which is used to get the last element added and remove it from the stack. The generic version can be used like the following code for a queue of strings. 

First, add the namespace:

    using System.Collections.Generic;

and use it:

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

There is a non generic version of the type, which works with objects.

The namespace is:

    using System.Collections;

And a code sample of non generic stack:

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

There is also a method called [Peek()][5] which returns the last element added but without removing it from the `Stack`.

    Stack<int> stack = new Stack<int>();
    stack.Push(10);
    stack.Push(20);
    
    var lastValueAdded = stack.Peek(); // 20

It is possible to iterate on the elements on the stack and it will respect the order of the stack (LIFO).

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
    
The output (without removing):

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

