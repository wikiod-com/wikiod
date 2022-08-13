---
title: "Erişim Değiştiricileri"
slug: "erisim-degistiricileri"
draft: false
images: []
weight: 9953
type: docs
toc: true
---

Erişim değiştirici atlanırsa,

- sınıflar varsayılan olarak "dahili"dir
- yöntemler varsayılan olarak "özel"dir
- alıcılar ve ayarlayıcılar özelliğin değiştiricisini devralır, varsayılan olarak bu "özel"dir


Özellik ayarlayıcıları veya alıcıları üzerindeki erişim değiştiricileri, erişimi genişletemez, yalnızca erişimi kısıtlayabilir:
`public string someProperty {get; özel küme;}`

## Erişim Değiştirici Diyagramları
Venn şemalarındaki tüm erişim değiştiricileri, daha sınırlayıcıdan daha erişilebilire kadar aşağıda verilmiştir:

| Erişim Değiştirici| diyagram |
| ------: | ------ |
| özel | ![özel][1] |
| iç | ![dahili][4] |
| korumalı | ![korumalı][5] |
| korumalı dahili | ![korumalı dahili][6] |
| kamu | ![genel][7] |

Aşağıda daha fazla bilgi bulabilirsiniz.



<!--
| `özel korumalı` (**∗** *önerilen özellik* - şu anda C#'da mevcut değil) | ![özel korumalı][3] |
-->


[1]: http://i.stack.imgur.com/SdeM9.png
[2]: http://i.stack.imgur.com/qsGH0.png
[3]: http://i.stack.imgur.com/ACp0t.png
[4]: http://i.stack.imgur.com/8o7Dm.png
[5]: http://i.stack.imgur.com/uniOu.png
[6]: http://i.stack.imgur.com/VaQQ9.png
[7]: http://i.stack.imgur.com/VGgjh.png

## halka açık
'public' anahtar sözcüğü, bir sınıfı (iç içe sınıflar dahil), özelliği, yöntemi veya alanı her tüketicinin kullanımına sunar:

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

## özel
"private" anahtar sözcüğü, yalnızca sınıf içinde kullanım için özellikleri, yöntemleri, alanları ve iç içe sınıfları işaretler:

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

## dahili

Dahili anahtar kelime, bir sınıfı (iç içe sınıflar dahil), özelliği, yöntemi veya alanı aynı derlemedeki her tüketici için kullanılabilir hale getirir:

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

Bu, bir test derlemesinin AssemblyInfo.cs dosyasına kod ekleyerek koda erişmesine izin vermek için bozulabilir:

    using System.Runtime.CompilerServices;
    
    [assembly:InternalsVisibleTo("MyTests")]

## korumalı
'protected' anahtar sözcüğü, alanı, yöntem özelliklerini ve yalnızca aynı sınıf ve türetilmiş sınıflar içinde kullanım için iç içe sınıfları işaretler:

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

## korumalı dahili
"Korumalı dahili" anahtar sözcüğü, aynı derleme içinde veya başka bir derlemede türetilmiş sınıflar içinde kullanım için alanı, yöntemleri, özellikleri ve iç içe sınıfları işaretler:

> Montaj 1

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

> Montaj 2

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

