---
title: "Modificateurs d'accès"
slug: "modificateurs-dacces"
draft: false
images: []
weight: 9953
type: docs
toc: true
---

Si le modificateur d'accès est omis,

- les classes sont par défaut "internes"
- les méthodes sont par défaut "privées"
- les getters et les setters héritent du modificateur de la propriété, par défaut c'est `private`


Les modificateurs d'accès sur les setters ou les getters de propriétés peuvent uniquement restreindre l'accès, pas l'élargir :
` public string someProperty {get; ensemble privé ;}`

## Diagrammes des modificateurs d'accès
Voici tous les modificateurs d'accès dans les diagrammes de Venn, du plus limitant au plus accessible :

| Modificateur d'accès | Diagramme |
| ------ : | ------ |
| privé | ![privé][1] |
| interne | ![interne][4] |
| protégé | ![protégé][5] |
| interne protégé | ![interne protégé][6] |
| public | ![public][7] |

Vous trouverez ci-dessous plus d'informations.



<!--
| `private protected` (**∗** *fonctionnalité suggérée* - actuellement non disponible en C#) | ![privé protégé][3] |
-->


[1] : http://i.stack.imgur.com/SdeM9.png
[2] : http://i.stack.imgur.com/qsGH0.png
[3] : http://i.stack.imgur.com/ACp0t.png
[4] : http://i.stack.imgur.com/8o7Dm.png
[5] : http://i.stack.imgur.com/uniOu.png
[6] : http://i.stack.imgur.com/VaQQ9.png
[7] : http://i.stack.imgur.com/VGgjh.png

## Publique
Le mot-clé `public` rend une classe (y compris les classes imbriquées), une propriété, une méthode ou un champ disponible pour chaque consommateur :

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

## privé
Le mot-clé `private` marque les propriétés, les méthodes, les champs et les classes imbriquées à utiliser uniquement à l'intérieur de la classe :

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

## interne

Le mot-clé interne rend une classe (y compris les classes imbriquées), une propriété, une méthode ou un champ disponible pour chaque consommateur dans le même assembly :

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

Cela peut être cassé pour permettre à un assembly de test d'accéder au code via l'ajout de code au fichier AssemblyInfo.cs :

    using System.Runtime.CompilerServices;
    
    [assembly:InternalsVisibleTo("MyTests")]

## protégé
Le mot clé `protected` marque le champ, les propriétés des méthodes et les classes imbriquées pour une utilisation à l'intérieur de la même classe et des classes dérivées uniquement :

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

## interne protégé
Le mot-clé `protected internal` marque le champ, les méthodes, les propriétés et les classes imbriquées à utiliser dans le même assembly ou dans des classes dérivées d'un autre assembly :

> Assemblage 1

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

> Assemblage 2

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

