---
title: "Lists"
slug: "lists"
draft: false
images: []
weight: 9928
type: docs
toc: true
---

A _list_ is an _ordered_ collection of values. In Java, lists are part of the [Java Collections Framework][1]. Lists implement the [`java.util.List`][2] interface, which extends [`java.util.Collection`][3].


  [1]: http://docs.oracle.com/javase/8/docs/technotes/guides/collections/index.html
  [2]: http://docs.oracle.com/javase/8/docs/api/java/util/List.html
  [3]: http://docs.oracle.com/javase/8/docs/api/java/util/Collection.html

## Syntax
 - ls.add(E element); //Adds an element
 - ls.remove(E element); //Removes an element
 - for(E element : ls){} //Iterates over each element
 - ls.toArray(new String[ls.length]); //Converts a List of Strings to an array of Strings
 - ls.get(int index); //Returns the element at the specified index.
 - ls.set(int index, E element); //Replaces the element at a specified position .
 - ls.isEmpty(); //Returns true if the array contains no elements, otherwise it returns false.
 - ls.indexOf(Object o); //Returns the index of the first location of the specified element o, or, if it is not present, returns -1.
- ls.lastIndexOf(Object o); //Returns the index of the last location of the specified element o, or, if it is not present, returns -1.
 - ls.size(); //Returns the number of elements in the List.

A [list][1] is an object which stores a an ordered collection of values. "Ordered" means the values are stored in a particular order--one item comes first, one comes second, and so on. The individual values are commonly called "elements". Java lists typically provide these features:

* Lists may contain zero or more elements.
* Lists may contain duplicate values. In other words, an element can be inserted into a list more than once.
* Lists store their elements in a particular order, meaning one element comes first, one comes next, and so on.
* Each element has an _index_ indicating its position within the list. The first element has index 0, the next has index 1, and so on.
* Lists permit inserting elements at the beginning, at the end, or at any index within the list.
* Testing whether a list contains a particular value generally means examining each element in the list. This means that the time to perform this check is [O(n)][2], proportional to the size of the list.

Adding a value to a list at some point other than the end will move all of the following elements "down" or "to the right". In other words, adding an element at index _n_ moves the element which used to be at index _n_ to index _n+1_, and so on. For example:

    List<String> list = new ArrayList<>();
    list.add("world");
    System.out.println(list.indexOf("world"));      // Prints "0"
    // Inserting a new value at index 0 moves "world" to index 1
    list.add(0, "Hello");
    System.out.println(list.indexOf("world"));      // Prints "1"
    System.out.println(list.indexOf("Hello"));      // Prints "0"


  [1]: http://docs.oracle.com/javase/8/docs/api/java/util/List.html
  [2]: https://en.wikipedia.org/wiki/Big_O_notation

## Sorting a generic list
The `Collections` class offers two standard static methods to sort a list:

* `sort(List<T> list)` applicable to lists where `T extends Comparable<? super T>`, and
* `sort(List<T> list, Comparator<? super T> c)` applicable to lists of any type.

Applying the former requires amending the class of list elements being sorted, which is not always possible. It might also be undesirable as although it provides the default sorting, other sorting orders may be required in different circumstances, or sorting is just a one off task.

Consider we have a task of sorting objects that are instances of the following class:

    public class User {
        public final Long id;
        public final String username;

        public User(Long id, String username) {
            this.id = id;
            this.username = username;
        }

        @Override
        public String toString() {
            return String.format("%s:%d", username, id);
        }
    }

In order to use `Collections.sort(List<User> list)` we need to modify the `User` class to implement the `Comparable` interface.  For example

    public class User implements Comparable<User> {
        public final Long id;
        public final String username;

        public User(Long id, String username) {
            this.id = id;
            this.username = username;
        }

        @Override
        public String toString() {
            return String.format("%s:%d", username, id);
        }

        @Override
        /** The natural ordering for 'User' objects is by the 'id' field. */
        public int compareTo(User o) {
            return id.compareTo(o.id);
        }
    }

(Aside: many standard Java classes such as `String`, `Long`, `Integer` implement the `Comparable` interface.  This makes lists of those elements sortable by default, and simplifies implementation of `compare` or `compareTo` in other classes.)

With the modification above, the we can easily sort a list of `User` objects based on the classes *natural ordering*.  (In this case, we have defined that to be ordering based on `id` values).  For example:

    List<User> users = Lists.newArrayList(
        new User(33L, "A"),
        new User(25L, "B"),
        new User(28L, ""));
    Collections.sort(users);

    System.out.print(users);
    // [B:25, C:28, A:33]

However, suppose that we wanted to sort `User` objects by `name` rather than by `id`.  Alternatively, suppose that we had not been able to change the class to make it implement `Comparable`.

This is where the `sort` method with the `Comparator` argument is useful:

    Collections.sort(users, new Comparator<User>() {
        @Override
        /* Order two 'User' objects based on their names. */
        public int compare(User left, User right) {
            return left.username.compareTo(right.username);
        }
    });
    System.out.print(users);
    // [A:33, B:25, C:28]

<!-- if version [gte Java SE 8] -->

In Java 8  you can use a *lambda* instead of an anonymous class. The latter reduces to a one-liner:

    Collections.sort(users, (l, r) -> l.username.compareTo(r.username));

Further, there Java 8 adds a default `sort` method on the `List` interface, which simplifies sorting even more.

    users.sort((l, r) -> l.username.compareTo(r.username))

<!-- end version if -->

## Convert a list of integers to a list of strings
    List<Integer> nums = Arrays.asList(1, 2, 3);
    List<String> strings = nums.stream()
        .map(Object::toString)
        .collect(Collectors.toList());

That is:

1. Create a stream from the list
2. Map each element using `Object::toString`
3. Collect the `String` values into a `List` using `Collectors.toList()`

## Classes implementing List - Pros and Cons
The [`List`][1] interface is implemented by different classes. Each of them has its own way for implementing it with different strategies and providing different pros and cons.



----------
Classes implementing List
-------------

These are all of the `public` classes in Java SE 8 that implement the `java.util.List` interface: 

 1. **Abstract Classes:**
    - AbstractList
    - AbstractSequentialList
 2. **Concrete Classes:**
    - ArrayList
    - AttributeList
    - CopyOnWriteArrayList
    - LinkedList
    - RoleList
    - RoleUnresolvedList
    - Stack
    - Vector



----------

**Pros and Cons of each implementation in term of time complexity**
------------------------------------------------------------------------


ArrayList
---------

    public class ArrayList<E>
    extends AbstractList<E>
    implements List<E>, RandomAccess, Cloneable, Serializable

[ArrayList][2] is a resizable-array implementation of the List interface. Storing the list into an array, *ArrayList* provides methods (in addition to the methods implementing the *List* interface) for manipulating the size of the array.

**Initialize ArrayList of Integer with size 100**

    List<Integer> myList = new ArrayList<Integer>(100); // Constructs an empty list with the specified initial capacity.

**- PROS:**
 
The size, isEmpty, ***get***, ***set***, iterator, and listIterator operations run in constant time.
So getting and setting each element of the List has the same *time cost*:


    int e1 = myList.get(0);  //   \
    int e2 = myList.get(10); //    | => All the same constant cost => O(1)
    myList.set(2,10);        //   /

**- CONS:**

Being implemented with an array (static structure) adding elements over the size of the array has a big cost due to the fact that a new allocation need to be done for all the array.
However, from [documentation][2]:

> The add operation runs in amortized constant time, that is, adding n
> elements requires O(n) time

Removing an element requires O(n) time.

----------


AttributeList
---------

On coming


----------


CopyOnWriteArrayList
---------

On coming


----------


LinkedList
---------

    public class LinkedList<E>
    extends AbstractSequentialList<E>
    implements List<E>, Deque<E>, Cloneable, Serializable

[LinkedList][3] is implemented by a [doubly-linked list][4] a linked data structure that consists of a set of sequentially linked records called nodes.

**Iitialize LinkedList of Integer** 

    List<Integer> myList = new LinkedList<Integer>(); // Constructs an empty list.


**- PROS:**

Adding or removing an element to the front of the list or to the end has constant time.

    myList.add(10);  // \
    myList.add(0,2); //  | => constant time => O(1)
    myList.remove(); // /


**- CONS:**
From [documentation][3]:
> Operations that index into the list will traverse the list from the
> beginning or the end, whichever is closer to the specified index.

Operations such as:

    myList.get(10);    // \
    myList.add(11,25); //  | => worst case done in O(n/2)
    myList.set(15,35); // /

----------


RoleList
---------
On coming


----------


RoleUnresolvedList
---------
On coming


----------


Stack
---------
On coming


----------


Vector
---------
On coming


----------


  [1]: https://docs.oracle.com/javase/7/docs/api/java/util/List.html
  [2]: https://docs.oracle.com/javase/7/docs/api/java/util/ArrayList.html
  [3]: https://docs.oracle.com/javase/7/docs/api/java/util/LinkedList.html
  [4]: https://en.wikipedia.org/wiki/Doubly_linked_list


## Finding common elements between 2 lists
Suppose you have two lists: A and B, and you need to find the elements that exist in both lists. 

You can do it by just invoking the method `List.retainAll()`.



**Example:**


    public static void main(String[] args) {
        List<Integer> numbersA = new ArrayList<>();
        List<Integer> numbersB = new ArrayList<>();
        numbersA.addAll(Arrays.asList(new Integer[] { 1, 3, 4, 7, 5, 2 }));
        numbersB.addAll(Arrays.asList(new Integer[] { 13, 32, 533, 3, 4, 2 }));
    
        System.out.println("A: " + numbersA);
        System.out.println("B: " + numbersB);
        List<Integer> numbersC = new ArrayList<>();
        numbersC.addAll(numbersA);
        numbersC.retainAll(numbersB);
    
        System.out.println("List A : " + numbersA);
        System.out.println("List B : " + numbersB);
        System.out.println("Common elements between A and B: " + numbersC);
    
    }

## Creating a List

**Giving your list a type**

To create a list you need a type (any class, e.g. [`String`](https://www.wikiod.com/java/strings)). This is the type of your `List`. The `List` will only store objects of the specified type. For example:

    List<String> strings;

Can store `"string1"`, `"hello world!"`, `"goodbye"`, etc, but it can't store `9.2`, however:

    List<Double> doubles;

Can store `9.2`, but not `"hello world!"`.

**Initialising your list**

If you try to add something to the lists above you will get a NullPointerException, because `strings` and `doubles` both equal **null**!

There are two ways to initialise a list:

**Option 1: Use a class that implements List**

`List` is an interface, which means that does not have a constructor, rather methods that a class must override. `ArrayList` is the most commonly used `List`, though `LinkedList` is also common. So we initialise our list like this:

    List<String> strings = new ArrayList<String>();

or

    List<String> strings = new LinkedList<String>();

<!-- if version [gte Java SE 7] -->
Starting from Java SE 7, you can use a [*diamond operator*][1]:

    List<String> strings = new ArrayList<>();

or

    List<String> strings = new LinkedList<>();

<!-- end version if -->

**Option 2: Use the Collections class**

The `Collections` class provides two useful methods for creating Lists without a `List` variable:

 - `emptyList()`: returns an empty list.
 - `singletonList(T)`: creates a list of type T and adds the element specified.

And a method which uses an existing `List` to fill data in:

 - `addAll(L, T...)`: adds all the specified elements to the list passed as the first parameter.

***Examples:***

```java
import java.util.List;
import java.util.Collections;

List<Integer> l = Collections.emptyList();
List<Integer> l1 = Collections.singletonList(42);
Collections.addAll(l1, 1, 2, 3);
```

[1]: https://www.wikiod.com/java/generics#Creating a Generic Class

## Positional Access Operations
The List API has eight methods for positional access operations:

 - `add(T type)`
 - `add(int index, T type)`
 - `remove(Object o)`
 - `remove(int index)`
 - `get(int index)`
 - `set(int index, E element)`
 - `int indexOf(Object o)`
 - `int lastIndexOf(Object o)`

So, if we have a List:

    List<String> strings = new ArrayList<String>();

And we wanted to add the strings "Hello world!" and "Goodbye world!" to it, we would do it as such:

    strings.add("Hello world!");
    strings.add("Goodbye world!");

And our list would contain the two elements.
Now lets say we wanted to add "Program starting!" at the **front** of the list. We would do this like this:

    strings.add(0, "Program starting!");

**NOTE: The first element is 0.**

Now, if we wanted to remove the "Goodbye world!" line, we could do it like this:

    strings.remove("Goodbye world!");

And if we wanted to remove the first line (which in this case would be "Program starting!", we could do it like this:

    strings.remove(0);

Note:

1.  Adding and removing list elements modify the list, and this can lead to a `ConcurrentModificationException` if the list is being iterated concurrently.

2.  Adding and removing elements can be `O(1)` or `O(N)` depending on the list class, the method used, and whether you are adding / removing an element at the start, the end, or in the middle of the list.

In order to retrieve an element of the list at a specified position you can use the `E get(int index);` method of the List API. For example:

    strings.get(0);

will return the first element of the list.

You can replace any element at a specified position by using the `set(int index, E element);`. For example:

    strings.set(0,"This is a replacement");

This will set the String "This is a replacement" as the first element of the list.

Note:
    The set method will overwrite the element at the position 0. It will not add the new String at the position 0 and push the old one to the position 1.

The `int indexOf(Object o);` returns the position of the first occurrence of the object passed as argument. If there are no occurrences of the object in the list then the -1 value is returned. In continuation of the previous example if you call:

    strings.indexOf("This is a replacement")

the 0 is expected to be returned as we set the String "This is a replacement" in the position 0 of our list.
In case where there are more than one occurrence in the list when `int indexOf(Object o);` is called then as mentioned the index of the first occurrence will be returned. By calling the `int lastIndexOf(Object o)` you can retrieve the index of the last occurrence in the list. So if we add another "This is a replacement":

    strings.add("This is a replacement");
    strings.lastIndexOf("This is a replacement");

This time the 1 will be returned and not the 0;




## Iterating over elements in a list
For the example, lets say that we have a List of type String that contains four elements: "hello, ", "how ", "are ", "you?"

The best way to iterate over each element is by using a for-each loop:

    public void printEachElement(List<String> list){
        for(String s : list){
            System.out.println(s);
        }
    }

Which would print:

    hello,
    how
    are
    you?

To print them all in the same line, you can use a StringBuilder:

    public void printAsLine(List<String> list){
        StringBuilder builder = new StringBuilder();
        for(String s : list){
            builder.append(s);
        }
        System.out.println(builder.toString());
    }

Will print:

    hello, how are you?


Alternatively, you can use element indexing ( as described in https://www.wikiod.com/java/lists ) to iterate a list.  Warning: this approach is inefficient for linked lists.

## Removing elements from list B that are present in the list A
 Lets suppose you have 2 Lists A and B, and you want to remove from **B** all the elements that you have in **A**  the method in this case is

     List.removeAll(Collection c);


**#Example:**

    public static void main(String[] args) {
        List<Integer> numbersA = new ArrayList<>();
        List<Integer> numbersB = new ArrayList<>();
        numbersA.addAll(Arrays.asList(new Integer[] { 1, 3, 4, 7, 5, 2 }));
        numbersB.addAll(Arrays.asList(new Integer[] { 13, 32, 533, 3, 4, 2 }));
        System.out.println("A: " + numbersA);
        System.out.println("B: " + numbersB);
    
        numbersB.removeAll(numbersA);
        System.out.println("B cleared: " + numbersB);
        }



this will print

> A: [1, 3, 4, 7, 5, 2] 
> 
> B: [13, 32, 533, 3, 4, 2] 
> 
> B cleared: [13, 32, 533]


## Creating, Adding and Removing element from an ArrayList
`ArrayList` is one of the inbuilt data structures in Java. It is a dynamic array (where the size of the data structure not needed to be declared first) for storing elements (Objects).

It extends `AbstractList` class and implements `List` interface. An `ArrayList` can contain duplicate elements where it maintains insertion order. It should be noted that the class `ArrayList` is non-synchronized, so care should be taken when handling concurrency with `ArrayList`. `ArrayList` allows random access because array works at the index basis. Manipulation is slow in `ArrayList` because of shifting that often occurs when an element is removed from the array list.

An `ArrayList` can be created as follows:

    List<T> myArrayList = new ArrayList<>();

Where `T` ( https://www.wikiod.com/java/generics ) is the type that will be stored inside `ArrayList`.

The type of the `ArrayList` can be any Object. The type can't be a primitive type (use their [*wrapper classes*](https://en.wikipedia.org/wiki/Primitive_wrapper_class) instead).

To add an element to the `ArrayList`, use `add()` method:

    myArrayList.add(element);

Or to add item to a certain index:

    myArrayList.add(index, element); //index of the element should be an int (starting from 0)

To remove an item from the `ArrayList`, use the `remove()` method:

    myArrayList.remove(element);

Or to remove an item from a certain index:

    myArrayList.remove(index); //index of the element should be an int (starting from 0)



## In-place replacement of a List element
This example is about replacing a `List` element while ensuring that the replacement element is at the same position as the element that is replaced. 

This can be done using these methods:

 - set(int index, T type)
 - int indexOf(T type)

Consider an `ArrayList` containing the elements "Program starting!", "Hello world!" and "Goodbye world!"

    List<String> strings = new ArrayList<String>();
    strings.add("Program starting!");
    strings.add("Hello world!");
    strings.add("Goodbye world!");

If we know the index of the element we want to replace, we can simply use `set` as follows:

    strings.set(1, "Hi world");

If we don't know the index, we can search for it first.  For example:

    int pos = strings.indexOf("Goodbye world!");
    if (pos >= 0) {
        strings.set(pos, "Goodbye cruel world!");
    }

Notes:

 1. The `set` operation will not cause a `ConcurrentModificationException`.
 2. The `set` operation is fast ( `O(1)` ) for `ArrayList` but slow ( `O(N)` ) for a `LinkedList`.
 3. An `indexOf` search on an `ArrayList` or `LinkedList` is slow ( `O(N)` ).

## Making a list unmodifiable
The Collections class provides a way to make a list unmodifiable:

    List<String> ls = new ArrayList<String>();
    List<String> unmodifiableList = Collections.unmodifiableList(ls);

If you want an unmodifiable list with one item you can use:
    
    List<String> unmodifiableList = Collections.singletonList("Only string in the list");

## Moving objects around in the list
The Collections class allows for you to move objects around in the list using various methods (ls is the List):

**Reversing a list:**

    Collections.reverse(ls);
**Rotating positions of elements in a list**

The rotate method requires an integer argument. This is how many spots to move it along the line by. An example of this is below:
    
    List<String> ls = new ArrayList<String>();
    ls.add(" how");
    ls.add(" are");
    ls.add(" you?");
    ls.add("hello,");
    Collections.rotate(ls, 1);

    for(String line : ls) System.out.print(line);
    System.out.println();

This will print "hello, how are you?"

**Shuffling elements around in a list**

Using the same list above, we can shuffle the elements in a list:

    Collections.shuffle(ls);

We can also give it a java.util.Random object that it uses to randomly place objects in spots:
   
    Random random = new Random(12); 
    Collections.shuffle(ls, random);

