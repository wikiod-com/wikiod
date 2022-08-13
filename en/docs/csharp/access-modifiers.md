---
title: "Access Modifiers"
slug: "access-modifiers"
draft: false
images: []
weight: 9953
type: docs
toc: true
---

If the access modifier is omitted, 

 - classes are by default `internal`
 - methods are by deault `private`
 - getters and setters inherit the modifier of the property, by default this is `private`


Access modifiers on setters or getters of properties can only restrict access, not widen it:
`public string someProperty {get; private set;}`

## Access Modifiers Diagrams
Here are all access modifiers in venn diagrams, from more limiting to more accessible:

| Access Modifier| Diagram |
| ------: | ------ |
|  private  | ![private][1]  |
|  internal  | ![internal][4]  |
|  protected  |  ![protected][5] |
|  protected internal |  ![protected internal][6] |
|  public  | ![public][7]  |

Below you could find more information.



<!--
| `private protected` (**∗** *suggested feature* - currently not available in C#)  |  ![private protected][3] |
-->


  [1]: http://i.stack.imgur.com/SdeM9.png
  [2]: http://i.stack.imgur.com/qsGH0.png
  [3]: http://i.stack.imgur.com/ACp0t.png
  [4]: http://i.stack.imgur.com/8o7Dm.png
  [5]: http://i.stack.imgur.com/uniOu.png
  [6]: http://i.stack.imgur.com/VaQQ9.png
  [7]: http://i.stack.imgur.com/VGgjh.png

## public
The `public` keyword makes a class (including nested classes), property, method or field available to every consumer:

    public class Foo()
    {
        public string SomeProperty { get; set; }

        public class Baz
        {
            public int Value { get; set; }
        }
    }

    public class Bar()
    {
        public Bar()
        {
            var myInstance = new Foo();
            var someValue = foo.SomeProperty;
            var myNestedInstance = new Foo.Baz();
            var otherValue = myNestedInstance.Value;
        }        
    }

## private
The `private` keyword marks properties, methods, fields and nested classes for use inside the class only:

    public class Foo()
    {
        private string someProperty { get; set; }

        private class Baz
        {
            public string Value { get; set; }
        }

        public void Do()
        {
            var baz = new Baz { Value = 42 };
        }
    }

    public class Bar()
    {
        public Bar()
        {
            var myInstance = new Foo();

            // Compile Error - not accessible due to private modifier
            var someValue = foo.someProperty;
            // Compile Error - not accessible due to private modifier
            var baz = new Foo.Baz();
        }
    }

## internal

The internal keyword makes a class (including nested classes), property, method or field available to every consumer in the same assembly:

    internal class Foo
    {
        internal string SomeProperty {get; set;}
    }

    internal class Bar
    {
        var myInstance = new Foo();
        internal string SomeField = foo.SomeProperty;

        internal class Baz
        {
            private string blah;
            public int N { get; set; }
        }
    }

This can be broken to allow a testing assembly to access the code via adding code to AssemblyInfo.cs file:

    using System.Runtime.CompilerServices;
    
    [assembly:InternalsVisibleTo("MyTests")]

## protected
The `protected` keyword marks field, methods properties and nested classes for use inside the same class and derived classes only:

    public class Foo()
    {
        protected void SomeFooMethod()
        {
            //do something
        }

        protected class Thing
        {
            private string blah;
            public int N { get; set; }
        }
    }

    public class Bar() : Foo
    {
        private void someBarMethod()
        {
            SomeFooMethod(); // inside derived class
            var thing = new Thing(); // can use nested class
        }
    }

    public class Baz()
    {
        private void someBazMethod()
        {
            var foo = new Foo();
            foo.SomeFooMethod(); //not accessible due to protected modifier
        }
    }

## protected internal
The `protected internal` keyword marks field, methods, properties and nested classes for use inside the same assembly or derived classes in another assembly:

> Assembly 1

    public class Foo
    {
        public string MyPublicProperty { get; set; }
        protected internal string MyProtectedInternalProperty  { get; set; }

        protected internal class MyProtectedInternalNestedClass
        {
            private string blah;
            public int N { get; set; }
        }
    }

    public class Bar
    {
        void MyMethod1()
        {
            Foo foo = new Foo();
            var myPublicProperty = foo.MyPublicProperty;
            var myProtectedInternalProperty = foo.MyProtectedInternalProperty;
            var myProtectedInternalNestedInstance =
                new Foo.MyProtectedInternalNestedClass();
        }
    }

> Assembly 2

    public class Baz : Foo
    {
        void MyMethod1()
        {
            var myPublicProperty = MyPublicProperty;
            var myProtectedInternalProperty = MyProtectedInternalProperty;
            var thing = new MyProtectedInternalNestedClass();
        }

        void MyMethod2()
        {
            Foo foo = new Foo();
            var myPublicProperty = foo.MyPublicProperty;

            // Compile Error
            var myProtectedInternalProperty = foo.MyProtectedInternalProperty;
            // Compile Error
            var myProtectedInternalNestedInstance =
                new Foo.MyProtectedInternalNestedClass();
        }

    }

    public class Qux
    {
        void MyMethod1()
        {
            Baz baz = new Baz();
            var myPublicProperty = baz.MyPublicProperty;

            // Compile Error
            var myProtectedInternalProperty = baz.MyProtectedInternalProperty;
            // Compile Error
            var myProtectedInternalNestedInstance =
                new Baz.MyProtectedInternalNestedClass();
        }

        void MyMethod2()
        {
            Foo foo = new Foo();
            var myPublicProperty = foo.MyPublicProperty;

            //Compile Error
            var myProtectedInternalProperty = foo.MyProtectedInternalProperty;
            // Compile Error
            var myProtectedInternalNestedInstance =
                new Foo.MyProtectedInternalNestedClass();
        }
    }

