---
title: "Constructors"
slug: "constructors"
draft: false
images: []
weight: 9928
type: docs
toc: true
---

While not required, constructors in Java are methods recognized by the compiler to instantiate specific values for the class which may be essential to the role of the object. This topic demonstrates proper usage of Java class constructors.

The Java Language Specification talks at length about the exact nature of constructor semantics. They can be found in [JLS §8.8][1]


  [1]: https://docs.oracle.com/javase/specs/jls/se8/html/jls-8.html#jls-8.8

## Default Constructor
The "default" for constructors is that they do not have any arguments. In case you do not specify **any** constructor, the compiler will generate a default constructor for you.  
This means the following two snippets are semantically equivalent:

    public class TestClass {
        private String test;
    }

<!-- -->

    public class TestClass {
        private String test;
        public TestClass() {

        }
    }

The visibility of the default constructor is the same as the visibility of the class. Thus a class defined package-privately has a package-private default constructor

However, if you have non-default constructor, the compiler will not generate a default constructor for you. So these are not equivalent:

    public class TestClass {
        private String test;
        public TestClass(String arg) {
        }
    }

<!-- -->

    public class TestClass {
        private String test;
        public TestClass() {
        }
        public TestClass(String arg) {
        }
    }

Beware that the generated constructor performs no non-standard initialization. This means all fields of your class will have their default value, unless they have an initializer.

    public class TestClass {

        private String testData;

        public TestClass() {
            testData = "Test"
        }
    }

Constructors are called like this:

    TestClass testClass = new TestClass();

## Call parent constructor
Say you have a Parent class and a Child class. To construct a Child instance always requires some Parent constructor to be run at the very gebinning of the Child constructor. We can select the Parent constructor we want by explicitly calling `super(...)` with the appropriate arguments as our first Child constructor statement. Doing this saves us time by reusing the Parent classes' constructor instead of rewriting the same code in the Child classes' constructor.

**Without** `super(...)` **method:**

(implicitly, the no-args version `super()` is called invisibly)

```
class Parent {
    private String name;
    private int age;
    
    public Parent() {} // necessary because we call super() without arguments
    
    public Parent(String tName, int tAge) {
        name = tName;
        age = tAge;
    }
}

// This does not even compile, because name and age are private,
// making them invisible even to the child class.
class Child extends Parent {
    public Child() {
        // compiler implicitly calls super() here
        name = "John";
        age = 42;
    }
}
```

**With** `super()` **method:**
```
class Parent {
    private String name;
    private int age;
    public Parent(String tName, int tAge) {
        name = tName;
        age = tAge;
    }
}

class Child extends Parent {
    public Child() {
        super("John", 42);   // explicit super-call
    }
}
```

**Note:** Calls to another constructor (chaining) or the super constructor **MUST** be the first statement inside the constructor.  

If you call the `super(...)` constructor explicitly, a matching parent constructor must exist (that's straightforward, isn't it?).

If you don't call any `super(...)` constructor explicitly, your parent class must have a no-args constructor - and this can be either written explicitly or created as a default by the compiler if the parent class doesn't provide any constructor.


    class Parent{
        public Parent(String tName, int tAge) {}
    }

    class Child extends Parent{
        public Child(){}
    }

The class Parent has no default constructor, so, the compiler can't add `super` in the Child constructor. This code will not compile. You must change the constructors to fit both sides, or write your own `super` call, like that:

    class Child extends Parent{
        public Child(){
              super("",0);
        }
    }

## Constructor with Arguments
Constructors can be created with any kinds of arguments.

```
public class TestClass {

    private String testData;

    public TestClass(String testData) {
        this.testData = testData;
    }
}
```

Called like this:

```
TestClass testClass = new TestClass("Test Data");
```

A class can have multiple constructors with different signatures. To chain constructor calls (call a different constructor of the same class when instantiating) use `this()`.

```
public class TestClass {

    private String testData;

    public TestClass(String testData) {
        this.testData = testData;
    }

    public TestClass() {
        this("Test"); // testData defaults to "Test"
    }
}
```

Called like this:

```
TestClass testClass1 = new TestClass("Test Data");
TestClass testClass2 = new TestClass();
```

