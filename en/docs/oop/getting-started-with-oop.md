---
title: "Getting started with oop"
slug: "getting-started-with-oop"
draft: false
images: []
weight: 1
type: docs
toc: true
---

## OOP Introduction
# Intoduction
Object Oriented Programming (mostly referred as OOP) is a programming paradigm for solving problems.<br>
The beauty an OO (object oriented) program, is that we think about the program as a bunch of objects communicating with each other, instead of as a sequential script following specific orders.

There are lots of programming languages which support OOP, some of the popular are:

 - Java
 - C++
 - c#

Python is also known to support OOP but it lacks a few properties.
- - -
# OOP Terminology
The most basic term in OOP is a <kbd>class</kbd>.<br>
A class is basically an *object*, which has a state and it works according to its state.

Another important term is an <kbd>instance</kbd>.<br>
Think of a class as a template used to create instances of itself. The class is a template and the instance(s) is the concrete objects.

An instance created from the class *A* is usually referred to as from the 'type A', exactly like the type of *5* is *int* and the type of *"abcd"* is a *string*.


>An example of creating an instance named *insance1* of type (class) *ClassA*:
>
>### Java
><!-- language: lang-or-tag-here -->
>     ClassA instance1 = new ClassA();
>
>### C++
><!-- language: lang-or-tag-here -->
>     ClassA instance1;
>or
>
><!-- language: lang-or-tag-here -->
>     ClassA *instance1 = new ClassA(); # On the heap
>
>### Python
><!-- language: lang-or-tag-here -->
>     instance1 = ClassA()

As you can see in the example above, in all cases the name of the class was mentioned and after it there was empty parentheses (except for the C++ where if they are empty the parentheses can be dropped). In these parentheses we can pass `arguments` to the <kbd>constructor</kbd> of our class.

A constructor is a method of a class which is called every time an instance is created. It can either take arguments or not. If the programmer does not specify any constructor for a class they build, an empty constructor will be created (a constructor which does nothing).<br>
In most languages the constructor is defined as a method without defining its return type and with the same name of the class (example in a few sections).

>An example of creating an instance named *b1* of type (class) *ClassB*. The constructor of *ClassB* takes one argument of type *int*:
>
>### Java
><!-- language: lang-or-tag-here -->
>     ClassA instance1 = new ClassA(5);
>or
>
><!-- language: lang-or-tag-here -->
>     int i = 5;
>     ClassA instance1 = new ClassA(i);
>### C++
><!-- language: lang-or-tag-here -->
>     ClassA instance1(5);
>### Python
><!-- language: lang-or-tag-here -->
>     instance1 = ClassA(5)
As you can see, the process of creating an instance is very similar to the process of calling a function.
- - -
# Functions vs Methods
Both functions and methods are very similar, but in Object Oriented Design (OOD) they each have their own meaning.<br>
A method is an operation performed on an instance of a class. The method itself usually uses the state of the instance to operate.<br>
Meanwhile, a function belongs to a class and not to a specific instance. This means it does not use the state of the class or any data stored in an instance.

From now on we will show our examples only in *Java* since OOP is very clear in this language, but the same principles work in any other OOP language.

In Java, a function has the word <kbd>static</kbd> in its definition, like so:
<!-- language: lang-or-tag-here -->
    // File's name is ClassA
    public static int add(int a, int b) {
        return a + b;
    }
This means you can call it from anywhere in the script.
<!-- language: lang-or-tag-here -->
    // From the same file
    System.out.println(add(3, 5));

    // From another file in the same package (or after imported)
    System.out.println(ClassA.add(3, 5));
When we call the function from another file we use the name of the class (in Java this is also the name of the file) it belongs to, this gives the intuition that the function belongs to the class and not any of its instances.

In contrast, we can define a mehod in *ClassA* like so:
<!-- language: lang-or-tag-here -->
    // File's name is ClassA
    public int subtract(int a, int b){
        return a - b;
    }
After this decleration we can call this method like so:
<!-- language: lang-or-tag-here -->
    ClassA a = new ClassA();
    System.out.println(a.subtract(3, 5));
Here we needed to create an instance of *ClassA* in order to call its method subtract. Notice we **CAN'T** do the following:
<!-- language: lang-or-tag-here -->
    System.out.println(ClassA.subtract(3, 5));
This line will produce a compilation error complaining we called this *non-static* method without an instance.

- - - 
# Using the state of a class
Let's suppose we want to implement our *subtract* method again, but this time we always want to subtract the same number (for each instance). We can create the following class:
<!-- language: lang-or-tag-here -->
    class ClassB {

        private int sub_amount;

        public ClassB(int sub_amount) {
            this.sub_amount = sub_amount;
        }

        public int subtract(int a) {
            return a - sub_amount;
        }
 
        public static void main(String[] args) {
            ClassB b = new ClassB(5);
            System.out.println(b.subtract(3)); // Ouput is -2
        }
    }
When we run this code, a new instance named *b* of class *ClassB* is created and its constructor is fed with the value *5*.<br>
The constructor now takes the given *sub_amount* and stores it as its own private field, also called *sub_amount* (this convention is very known in Java, to name the arguments the same as the fields).<br>
After that, we print to the console the result of calling the method *subtract* on *b* with the value of *3*.

Notice that in the implementation of *subtract* we don't use `this.` like in the constructor.<br>
In Java, `this` only needs to be written when there is another variable with the same name defined in that scope. The same works with Python's `self`.<br>
So when we use *sub_amount* in subtract, we reference the private field which is different for each class.

Another example to emphasize.<br>
Let's just change the main function in the above code to the following:
<!-- language: lang-or-tag-here -->
    ClassB b1 = new ClassB(1);
    ClassB b2 = new ClassB(2);

    System.out.println(b1.subtract(10)); // Output is 9
    System.out.println(b2.subtract(10)); // Output is 8
As we can see, *b1* and *b2* are independent and each have their own *state*.

- - - 
# Interfaces and Inheritance
An <kbd>interface</kbd> is a contract, it defines which methods a class will have and therefore its capabilities. An interface does not have an implementation, it only defined what needs to be done.<br>
An example in Java would be:
<!-- language: lang-or-tag-here -->
    interface Printalbe {
        public void print();
    }
The *Printalbe* interface defines a method called *print* but it doesn't give its implementation (pretty weird for Java). Every class which declares itself as `implementing` this interface must provide an implementation to the draw method. For example:
<!-- language: lang-or-tag-here -->
    class Person implements Printalbe {

        private String name;

        public Person(String name) {
            this.name = name;
        }

        public void print() {
            System.out.println(name);
        }
    }
If *Person* would declare itself as implementing *Drawable* but didn't provide an implementation to *print*, there would be a compilation error and the program wouldn't compile.

Inheritance is a term which points to a class *extending* another class. For example, let's say we now have a person who has an age. One way to implement a person like that would be to copy the *Person* class and write a new class called *AgedPerson* which has the same fields and methods but it has another property -age.<br>
This would be awful since we duplicate our **entire** code just to add a simple feature to our class.<br>
We can use inheritance to inherit from *Person* and thus get all of its features, then enhance them with our new feature, like so:
<!-- language: lang-or-tag-here -->
    class AgedPerson extends Person {

        private int age;

        public AgedPerson(String name, int age) {
            super(name);
            this.age = age;
        }

        public void print() {
            System.out.println("Name: " + name + ", age:" + age);
        }
    }
There are a few new things going on:
 - We used the saved word `extends` to indicate we are inheriting from Person (and also its implementation to *Printable*, so we do not need to declare `implementing Printable` again).
 - We used the save word `super` to call *Person*'s constructor.
 - We overridden the *print* method of *Person* with a new one.

This is getting pretty Java technical so I will not go any deeper into this topic. But I will mention that there a lot of extreme cases that should be learned about inheritance and interfaces before starting to use them. For example which methods and functions are inherited? What happens to private/public/protected fields when inheriting from a class? and so on.

## Abstract Class
An <kbd>abstract class</kbd> is pretty advanced term in OOP which describes a combination of both interfaces and inheritance. It lets you write a class which has both implemented and unimplemented methods/functions in. In Java this is done by using the keyword `abstract` and I won't explain it more that a quick example:
<!-- language: lang-or-tag-here -->
    abstract class AbstractIntStack {

        abstract public void push(int element);

        abstract public void pop();

        abstract public int top();

        final public void replaceTop(int element) {
            pop();
            push(element);
        }
    }

Note: the `final` keyword states that you can't override this method when you inherit from this class. If a class is declared final, then no class can inherit from it at all.

## Introduction
OOP - Object Oriented Programming is a vastly used programming paradigm in these days. In OOP, we model real world problems using Objects and there behaviors, in order to solve them, programmatically.

There are four main __OOP Concepts__
1. Inheritance
2. Polymorphism
3. Abstraction
4. Encapsulation

These four concepts together are used to develop programs in OOP.

There are various languages which supprots Object Oriented Programming. Most popular languages are
* C++
* Java
* C#
* Python (Python isn't fully Object Oriented, but has most of the OOP features of it)

