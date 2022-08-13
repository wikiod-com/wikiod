---
title: "Spaceship Operator"
slug: "spaceship-operator"
draft: false
images: []
weight: 9983
type: docs
toc: true
---

## Spaceship operator for custom sortings
    class User {
      String name
      int age
    }
    
    def users = [
      new User(name: "Bob", age: 20),
      new User(name: "Tom", age: 50),
      new User(name: "Bill", age: 45)
    ]
    
    // sort by age
    users.sort { a, b -> a.age <=> b.age }

## Basic usage
the spaceship operator returns `-1` when the left operator is smaller, `0` when the operators are equal and `1` otherwise:

    assert 10 <=> 20 == -1
    assert 10 <=> 10 ​== 0
    assert 30 <=> 10 == 1

    assert 'a' <=> 'b' == -1
    assert 'a' <=> 'a'​== 0
    assert 'b' <=> 'a' == 1​​

It is equivalent to the Comparable.compareTo method:

    assert 10.compareTo(20) == (10 <=> 20)
    assert 'a'.compareTo('b') == ('a' <=> 'b')



## Usage with Comparator and SortedSet
    Comparator cmp = [ compare:{ a, b -> a <=> b } ] as Comparator
    def col = [ 'aa', 'aa', 'nn', '00' ]
    SortedSet sorted = new TreeSet( cmp )
    sorted.addAll col
    assert '[00, aa, nn]' == sorted.toString()

