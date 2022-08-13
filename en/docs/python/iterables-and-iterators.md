---
title: "Iterables and Iterators"
slug: "iterables-and-iterators"
draft: false
images: []
weight: 9908
type: docs
toc: true
---

## Iterator vs Iterable vs Generator
An **iterable** is an object that can return an **iterator**. Any object with state that has an `__iter__`  method and returns an iterator is an iterable. It may also be an object *without* state that implements a `__getitem__` method. - The method can take indices (starting from zero) and raise an `IndexError` when the indices are no longer valid.

Python's `str` class is an example of a `__getitem__` iterable.

An **Iterator** is an object that produces the next value in a sequence when you call `next(*object*)` on some object. Moreover, any object with a `__next__` method is an iterator. An iterator raises `StopIteration` after exhausting the iterator and *cannot* be re-used at this point.

**Iterable classes:**

Iterable classes define an `__iter__` and a `__next__` method. Example of an iterable class :

    class MyIterable:

        def __iter__(self):

             return self

        def __next__(self):
             #code

    #Classic iterable object in older versions of python, __getitem__ is still supported...
    class MySequence:

        def __getitem__(self, index):

             if (condition):
                 raise IndexError
             return (item)

     #Can produce a plain `iterator` instance by using iter(MySequence())

Trying to instantiate the abstract class from the `collections` module to better see this.

Example:

<!-- if version <Python 2.x> [gte 2.3] -->
    import collections
    >>> collections.Iterator()
    >>> TypeError: Cant instantiate abstract class Iterator with abstract methods next
<!-- end version if -->

<!-- if version <Python 3.x> [gte 3.0] -->
    >>> TypeError: Cant instantiate abstract class Iterator with abstract methods __next__
<!-- end version if -->


Handle Python 3 compatibility for iterable classes in Python 2 by doing the following:


<!-- if version <Python 2.x> [gte 2.3] -->
    class MyIterable(object): #or collections.Iterator, which I'd recommend....

         ....

         def __iter__(self): 

              return self
   
         def next(self): #code

         __next__ = next
<!-- end version if -->


Both of these are now iterators and can be looped through: 

    ex1 = MyIterableClass()
    ex2 = MySequence()

    for (item) in (ex1): #code
    for (item) in (ex2): #code

**Generators** are simple ways to create iterators. A generator *is* an iterator and an iterator is an iterable.


## Extract values one by one


## Iterating over entire iterable


## Verify only one element in iterable


## What can be iterable


## Iterator isn't reentrant!


