---
title: "Modificadores de acesso"
slug: "modificadores-de-acesso"
draft: false
images: []
weight: 9953
type: docs
toc: true
---

Se o modificador de acesso for omitido,

- as classes são por padrão `internas`
- métodos são por padrão `privados`
- getters e setters herdam o modificador da propriedade, por padrão este é `private`


Modificadores de acesso em setters ou getters de propriedades só podem restringir o acesso, não ampliá-lo:
`string pública algumaPropriedade {get; conjunto privado;}`

## Diagramas de modificadores de acesso
Aqui estão todos os modificadores de acesso nos diagramas de Venn, de mais limitantes a mais acessíveis:

| Modificador de acesso| Diagrama |
| ------: | ------ |
| privado | ![privado][1] |
| interno | ![interno][4] |
| protegido | ![protegido][5] |
| interno protegido | ![protegido interno][6] |
| público | ![público][7] |

Abaixo você pode encontrar mais informações.



<!--
| `private protected` (**∗** *recurso sugerido* - atualmente não disponível em C#) | ![protegido privado][3] |
-->


[1]: http://i.stack.imgur.com/SdeM9.png
[2]: http://i.stack.imgur.com/qsGH0.png
[3]: http://i.stack.imgur.com/ACp0t.png
[4]: http://i.stack.imgur.com/8o7Dm.png
[5]: http://i.stack.imgur.com/uniOu.png
[6]: http://i.stack.imgur.com/VaQQ9.png
[7]: http://i.stack.imgur.com/VGgjh.png

## público
A palavra-chave `public` torna uma classe (incluindo classes aninhadas), propriedade, método ou campo disponível para cada consumidor:

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
A palavra-chave `private` marca propriedades, métodos, campos e classes aninhadas para uso somente dentro da classe:

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

A palavra-chave internal torna uma classe (incluindo classes aninhadas), propriedade, método ou campo disponível para cada consumidor no mesmo assembly:

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

Isso pode ser quebrado para permitir que um assembly de teste acesse o código adicionando código ao arquivo AssemblyInfo.cs:

    using System.Runtime.CompilerServices;
    
    [assembly:InternalsVisibleTo("MyTests")]

## protegido
A palavra-chave `protected` marca campo, propriedades de métodos e classes aninhadas para uso dentro da mesma classe e somente classes derivadas:

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
A palavra-chave `protected internal` marca campos, métodos, propriedades e classes aninhadas para uso dentro do mesmo assembly ou classes derivadas em outro assembly:

> Montagem 1

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

> Montagem 2

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

