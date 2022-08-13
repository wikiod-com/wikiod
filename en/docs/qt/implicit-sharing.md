---
title: "Implicit sharing"
slug: "implicit-sharing"
draft: false
images: []
weight: 9986
type: docs
toc: true
---

STL style iterators on Qt Container can have some negative side effect due to the implicit-sharing. It is advised to avoid copying a Qt container while you have iterators active on them.

    QVector<int> a,b; //2 vectors
    a.resize(1000); 
    b = a; // b and a now point to the same memory internally

    auto iter = a.begin(); //iter also points to the same memory a and b do
    a[4] = 1; //a creates a new copy and points to different memory.
    //Warning 1: b and iter point sill to the same even if iter was "a.begin()"

    b.clear(); //delete b-memory
    //Warning 2: iter only holds a pointer to the memory but does not increase ref-count. 
    //           so now the memory iter points to is invalid. UB!

## Basic Concept
Several Qt Objects and Containers use a concept calles **implicit sharing**, which can also be refered to as **copy-on-write**.

Implicit sharing means that the classes who use this concept share the same data on initialization. 

One of these classes to use the concept is QString.

    QString s1("Hello World");

[![One QString initialization][1]][1]

This is a simplified model of a QString. Internally it has a memory block, with the actual string data and and a reference counter. 

    QString s2 = s1;

[![copy QString][2]][2]

If we now copy this `QString` both objects will internally point to the same content, thus avoiding unnecessary copy operations. Note how the reference count also got upped. So in case the first string gets deleted the shared-data still knows it is referenced by another `QString`.

    s2 += " and all the other Worlds!"

[![Copy on Write][3]][3]

Now when the `QString` is actually modified the object "detaches" itself from the memory block, copying it's content and modifies the content. 


  [1]: http://i.stack.imgur.com/P40g5.png
  [2]: http://i.stack.imgur.com/rvDtH.png
  [3]: http://i.stack.imgur.com/wzrRD.png

