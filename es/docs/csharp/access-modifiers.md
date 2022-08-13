---
title: "Modificadores de acceso"
slug: "modificadores-de-acceso"
draft: false
images: []
weight: 9953
type: docs
toc: true
---

Si se omite el modificador de acceso,

- las clases son por defecto `internas`
- los métodos son por defecto `privados`
- los getters y setters heredan el modificador de la propiedad, por defecto es `private`


Los modificadores de acceso en establecedores o captadores de propiedades solo pueden restringir el acceso, no ampliarlo:
`public string algunaPropiedad {get; conjunto privado;}`

## Diagramas de modificadores de acceso
Aquí están todos los modificadores de acceso en los diagramas de Venn, desde más limitantes hasta más accesibles:

| Modificador de acceso| Diagrama |
| ------: | ------ |
| privado | ![privado][1] |
| interno | ![interno][4] |
| protegido | ![protegido][5] |
| interno protegido | ![interno protegido][6] |
| publico | ![público][7] |

A continuación puede encontrar más información.



<!--
| `protegido privado` (**∗** *característica sugerida* - actualmente no disponible en C#) | ![protegido privado][3] |
-->


[1]: http://i.stack.imgur.com/SdeM9.png
[2]: http://i.stack.imgur.com/qsGH0.png
[3]: http://i.stack.imgur.com/ACp0t.png
[4]: http://i.stack.imgur.com/8o7Dm.png
[5]: http://i.stack.imgur.com/uniOu.png
[6]: http://i.stack.imgur.com/VaQQ9.png
[7]: http://i.stack.imgur.com/VGgjh.png

## público
La palabra clave `public` hace que una clase (incluidas las clases anidadas), una propiedad, un método o un campo esté disponible para todos los consumidores:

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

## privado
La palabra clave `private` marca propiedades, métodos, campos y clases anidadas para usar solo dentro de la clase:

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

## interno

La palabra clave interna hace que una clase (incluidas las clases anidadas), propiedad, método o campo esté disponible para todos los consumidores en el mismo ensamblaje:

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

Esto se puede romper para permitir que un ensamblaje de prueba acceda al código agregando código al archivo AssemblyInfo.cs:

    using System.Runtime.CompilerServices;
    
    [assembly:InternalsVisibleTo("MyTests")]

## protegido
El campo de marcas de palabras clave `protegido`, propiedades de métodos y clases anidadas para uso dentro de la misma clase y clases derivadas únicamente:

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

## interno protegido
La palabra clave `protected internal` marca el campo, los métodos, las propiedades y las clases anidadas para usar dentro del mismo ensamblaje o clases derivadas en otro ensamblaje:

> Asamblea 1

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

> Asamblea 2

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

