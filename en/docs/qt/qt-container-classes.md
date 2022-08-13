---
title: "Qt Container Classes"
slug: "qt-container-classes"
draft: false
images: []
weight: 9978
type: docs
toc: true
---

Qt provides its own template container classes. They are all implicitly shared. They provide two kinds of iterators (Java style and STL style.)

Qt sequential containers include: QVector, QList, QLinkedList, QStack, QQueue.

Qt associative containers include: QMap, QMultiMap, QHash, QMultiHash, QSet. 



## QStack usage
`QStack<T>` is a template Qt class providing stack. Its analogue in STL is `std::stack`. It is last in, first out structure (LIFO). 

    QStack<QString> stack;
    stack.push("First");
    stack.push("Second");
    stack.push("Third");
    while (!stack.isEmpty())
    {
        cout << stack.pop() << endl;
    }

It will output: Third, Second, First.

`QStack` inherits from `QVector` so its implementation is quite different from STL. In STL `std::stack` is implemented as a wrapper to type passed as a template argument (deque by default). Still main operations are the same for `QStack` and for `std::stack`.

## QVector usage
`QVector<T>` provides dynamic array template class. It provides better performance in most cases than `QList<T>` so it should be first choice. 

It can be initialized in various ways:

    QVector<int> vect;
    vect << 1 << 2 << 3;
    
    QVector<int> v {1, 2, 3, 4};

The latest involves initialization list.

    QVector<QString> stringsVector;
    stringsVector.append("First");
    stringsVector.append("Second");

You can get i-th element of vector this way:

`v[i]` or `at[i]`

Make sure that `i` is valid position, even `at(i)` doesn't make a check, this is a difference from `std::vector`.





## QLinkedList usage
In Qt you should use QLinkedList in case you need to implement [linked list](https://en.wikipedia.org/wiki/Linked_list). 

It is fast to append, prepend, insert elements into `QLinkedList` - O(1), but index lookup is slower than in `QList` or `QVector` - O(n). This is normal taking into attention you have to iterate through nodes to find something in linked list.

Full algorithmic compexity table can be found [here](http://doc.qt.io/qt-5/containers.html#algorithmic-complexity).

Just to insert some elements into `QLinkedList` you can use operator `<<()`:

    QLinkedList<QString> list;
    list << "string1" << "string2" << "string3";

To insert elements in the middle of `QLinkedList` or modify all or some of its elements you can use Java style or STL style iterators. Here is a simple example how we multiply all the elements of `QLinkedList` by 2:

    QLinkedList<int> integerList {1, 2, 3};
    QLinkedList<int>::iterator it;
    for (it = integerList.begin(); it != integerList.end(); ++it)
    {
        *it *= 2;
    }



## QList
The `QList` class is a template class that provides lists. It stores items in a list that provides fast index-based access and index-based insertions and removals.

To insert items into the list, you can use `operator<<()`, `insert()`, `append()` or `prepend()`. For example:

**`operator<<()`**

    QList<QString> list;
    list << "one" << "two" << "three";

**`insert()`**

    QList<QString> list;
    list << "alpha" << "beta" << "delta";
    list.insert(2, "gamma");

**`append()`**

    QList<QString> list;
    list.append("one");
    list.append("two");
    list.append("three");

**`prepend()`**

    QList<QString> list;
    list.prepend("one");
    list.prepend("two");
    list.prepend("three");

To access the item at a particular index position, you can use `operator[]()` or `at()`. `at()` may be faster than `operator[]()`, it never causes deep copy of container and should work in constant-time. Neither of them does argument-check. Examples:

    if (list[0] == "mystring")
        cout << "mystring found" << endl;

Or

    if (list.at(i) == "mystring")
        cout << "mystring found at position " << i << endl;

To remove items, there are functions such as `removeAt()`, `takeAt()`, `takeFirst()`, `takeLast()`, `removeFirst()`, `removeLast()`, or `removeOne()`. Examples:

**`takeFirst()`** 

    // takeFirst() removes the first item in the list and returns it
    QList<QWidget *> list;
    ...
    while (!list.isEmpty())
        delete list.takeFirst();

**`removeOne()`**

    // removeOne() removes the first occurrence of value in the list
    QList<QString> list;
    list << "sun" << "cloud" << "sun" << "rain";
    list.removeOne("sun");

To find all occurrences of a particular value in a list, you can use `indexOf()` or `lastIndexOf()`.  Example:

**`indexOf()`**

    int i = list.indexOf("mystring");
    if (i != -1)
        cout << "First occurrence of mystring is at position " << i << endl;



