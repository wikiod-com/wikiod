---
title: "Classes"
slug: "classes"
draft: false
images: []
weight: 9985
type: docs
toc: true
---

## Members
A class can have members.

Instance variables can be declared with/without type annotations, and optionally initialized. Uninitialised members have the value of `null`, unless set to another value by the constructor.

    class Foo {
      var member1;
      int member2;
      String member3 = "Hello world!";
    }

Class variables are declared using the `static` keyword.

    class Bar {
      static var member4;
      static String member5;
      static int member6 = 42;
    }


If a method takes no arguments, is fast, returns a value, and doesn't have visible side-effects, then a getter method can be used:

    class Foo {
      String get bar {
        var result;
        // ...
        return result;
      }
    }

Getters never take arguments, so the parentheses for the (empty) parameter list are omitted both for declaring getters, as above, and for calling them, like so:

    main() {
      var foo = new Foo();
      print(foo.bar); // prints "bar"
    }

There are also setter methods, which must take exactly one argument:

    class Foo {
      String _bar;

      String get bar => _bar;

      void set bar(String value) {
        _bar = value;
      }
    }

The syntax for calling a setter is the same as variable assignment:

    main() {
      var foo = new Foo();
      foo.bar = "this is calling a setter method";
    }



## Creating a class
Classes can be created as follow:

    class InputField {
      int maxLength;
      String name;
    }

The class can be instantiated using the `new` keyword after which the field values will be null by default.

    var field = new InputField();

Field values can then be accessed:

    // this will trigger the setter
    field.name = "fieldname";
    
    // this will trigger the getter
    print(field.name);




## Constructors
A class constructor must have the same name as its class.

Let's create a constructor for a class Person:

    class Person {
      String name;
      String gender;
      int age;

      Person(this.name, this.gender, this.age);
    }

The example above is a simpler, better way of defining the constructor than the following way, which is also possible:

    class Person {
      String name;
      String gender;
      int age;

      Person(String name, String gender, int age) {
        this.name = name;
        this.gender = gender;
        this.age = age;
      }
    }

Now you can create an instance of Person like this:

    var alice = new Person('Alice', 'female', 21);

