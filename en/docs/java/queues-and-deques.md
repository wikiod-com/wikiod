---
title: "Queues and Deques"
slug: "queues-and-deques"
draft: false
images: []
weight: 9955
type: docs
toc: true
---

## The usage of the PriorityQueue
`PriorityQueue` is a data structure. Like `SortedSet`, `PriorityQueue` sorts also its elements based on their priorities. The elements, which have a higher priority, comes first. The type of the `PriorityQueue` should implement `comparable` or `comparator` interface, whose methods decides the priorities of the elements of the data structure.


    //The type of the PriorityQueue is Integer.
    PriorityQueue<Integer> queue = new PriorityQueue<Integer>();
    
    //The elements are added to the PriorityQueue
    queue.addAll( Arrays.asList( 9, 2, 3, 1, 3, 8 ) );
    
    //The PriorityQueue sorts the elements by using compareTo method of the Integer Class
    //The head of this queue is the least element with respect to the specified ordering
    System.out.println( queue );  //The Output: [1, 2, 3, 9, 3, 8]
    queue.remove();
    System.out.println( queue );  //The Output: [2, 3, 3, 9, 8]
    queue.remove();
    System.out.println( queue );  //The Output: [3, 8, 3, 9]
    queue.remove();
    System.out.println( queue );  //The Output: [3, 8, 9]
    queue.remove();
    System.out.println( queue );  //The Output: [8, 9]
    queue.remove();
    System.out.println( queue );  //The Output: [9]
    queue.remove();
    System.out.println( queue );  //The Output: []




## LinkedList as a FIFO Queue
The `java.util.LinkedList` class, while implementing `java.util.List` is a general-purpose implementation of `java.util.Queue` interface too operating on a [FIFO (First In, First Out)][1] principle.

In the example below, with `offer()` method, the elements are inserted into the `LinkedList`. This insertion operation is called `enqueue`. In the `while` loop below, the elements are removed from the `Queue` based on FIFO. This operation is called `dequeue`. 

    Queue<String> queue = new LinkedList<String>();

    queue.offer( "first element" );
    queue.offer( "second element" );
    queue.offer( "third element" );
    queue.offer( "fourth. element" );
    queue.offer( "fifth. element" );

    while ( !queue.isEmpty() ) {
      System.out.println( queue.poll() );
    }

The output of this code is

    first element
    second element
    third element
    fourth element
    fifth element

As seen in the output, the first inserted element "first element" is removed firstly, "second element" is removed in the second place etc.

[1]:https://en.wikipedia.org/wiki/FIFO

## Stacks


## BlockingQueue


## Deque
A `Deque` is a "double ended queue" which means that a elements can be added at the front or the tail of the queue. The queue only can add elements to the tail of a queue.

The `Deque` inherits the `Queue` interface which means the regular methods remain, however the Deque interface offers additional methods to be more flexible with a queue. The additional methods really speak for them self if you know how a queue works, since those methods are intended to add more flexibility:

| Method | Brief description |
| ------ | ------ |
| `getFirst()` | Gets the first item of the **head** of the queue without removing it. |
| `getLast()` | Gets the first item of the **tail** of the queue without removing it. |
| `addFirst(E e)`   | Adds an item to the **head** of the queue | 
| `addLast(E e)` | Adds an item to the **tail** of the queue |
| `removeFirst()` | Removes the first item at the **head** of the queue |
| `removeLast()` | Removes the first item at the **tail** of the queue |

Of course the same options for `offer`, `poll` and `peek` are available, however they do not work with exceptions but rather with special values. There is no point in showing what they do here.

# Adding and Accessing Elements #

To add elements to the tail of a Deque you call its `add()` method. You can also use the `addFirst()` and `addLast()` methods, which add elements to the head and tail of the deque.

    Deque<String> dequeA = new LinkedList<>();
    
    dequeA.add("element 1");      //add element at tail
    dequeA.addFirst("element 2"); //add element at head
    dequeA.addLast("element 3");  //add element at tail

You can peek at the element at the head of the queue without taking the element out of the queue. This is done via the `element()` method. You can also use the `getFirst()` and `getLast()` methods, which return the first and last element in the `Deque`. Here is how that looks:

    String firstElement0 = dequeA.element();
    String firstElement1 = dequeA.getFirst();
    String lastElement = dequeA.getLast();

# Removing Elements #

To remove elements from a deque, you call the `remove()`, `removeFirst()` and `removeLast()` methods. Here are a few examples:

    String firstElement = dequeA.remove();
    String firstElement = dequeA.removeFirst();
    String lastElement  = dequeA.removeLast();

## Queue Interface
**Basics**
 
A `Queue` is a collection for holding elements prior to processing. Queues typically, but not necessarily, order elements in a FIFO (first-in-first-out) manner.

 Head of the queue is the element that would be removed by a call to remove or poll. In a FIFO queue, all new elements are inserted at the tail of the queue.

**The Queue Interface**

    public interface Queue<E> extends Collection<E> {
        boolean add(E e);

        boolean offer(E e);

        E remove();

        E poll();

        E element();

        E peek();
    }

Each `Queue` method exists in two forms:

 - one throws an exception if the operation fails;
 - other returns a special value if the operation fails (either `null` or `false` depending on the operation.

| **Type of operation** | **Throws exception** | **Returns special value** |
| ------ | ------ |--------|
| Insert   | `add(e)`   | `offer(e)`   |
|Remove|`remove()`|`poll()`|
|Examine|`element()`|`peek()`|

