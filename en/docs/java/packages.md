---
title: "Packages"
slug: "packages"
draft: false
images: []
weight: 9980
type: docs
toc: true
---

package in java is used to group class and interfaces. This helps developer to avoid conflict when there are huge numbers of classes. If we use this package the classes we can create a class/interface with same name in different packages.

By using packages we can import the piece of again in another class.

There many *built in packages* in java like 

> 1.java.util
> 2.java.lang
> 3.java.io

We can define our own *user defined packages*.
 

Packages provide access protection.

> package statement must be first line of source code. There can only be one package in one source file.

With help of packages conflict between different modules can be avoided.



## Using Packages to create classes with the same name
First Test.class: 

    package foo.bar
    
    public class Test {

    }


Also Test.class in another package

    package foo.bar.baz

    public class Test {
    
    }

The above is fine because the two classes exist in different packages.

## Using Package Protected Scope
In Java if you don't provide an access modifier the default scope for variables is package-protected level. This means that classes can access the variables of other classes within the same package as if those variables were publicly available.

    package foo.bar

    public class ExampleClass {
        double exampleNumber;
        String exampleString;

        public ExampleClass() {
            exampleNumber = 3;
            exampleString = "Test String";
        }
        //No getters or setters
    }

    package foo.bar

    public class AnotherClass {
        ExampleClass clazz = new ExampleClass();

        System.out.println("Example Number: " + clazz.exampleNumber);
        //Prints Example Number: 3
        System.out.println("Example String: " + clazz.exampleString);
        //Prints Example String: Test String
    }

This method will not work for a class in another package:

    package baz.foo

    public class ThisShouldNotWork {
        ExampleClass clazz = new ExampleClass();

        System.out.println("Example Number: " + clazz.exampleNumber);
        //Throws an exception
        System.out.println("Example String: " + clazz.exampleString);
        //Throws an exception
    }

