---
title: "Statik Sınıflar"
slug: "statik-snflar"
draft: false
images: []
weight: 9944
type: docs
toc: true
---

## Statik Sınıflar
Bir sınıfa atıfta bulunurken "static" anahtar sözcüğünün üç etkisi vardır:
1. Statik bir sınıfın örneğini **oluşturamazsınız** (bu, varsayılan kurucuyu bile kaldırır)
2. Sınıftaki tüm özellikler ve yöntemler de **zorunlu** olmalıdır.
3. "Statik" bir sınıf, "mühürlü" bir sınıftır, yani miras alınamaz.

<pre><kod>
genel statik sınıf Foo
{
    //Notice there is no constructor as this cannot be an instance
    public static int Counter { get; set; }
    public static int GetCount()
    {
        return Counter;
    }
}

genel sınıf Programı
{
    static void Main(string[] args)
    {
        Foo.Counter++;
        Console.WriteLine(Foo.GetCount()); //this will print 1
        
        //var foo1 = new Foo(); 
        //this line would break the code as the Foo class does not have a constructor
    }
}
</code></pre>

## Statik anahtar kelime
Statik anahtar kelime 2 anlama gelir:
1. Bu değer nesneden nesneye değişmez, bir bütün olarak sınıf üzerinde değişir.
2. Statik özellikler ve yöntemler bir örnek gerektirmez.
<pre><kod>
kamu sınıfı Foo
{
    public Foo{
        Counter++;
        NonStaticCounter++;
    }

    public static int Counter { get; set; }
    public int NonStaticCounter { get; set; }
}

genel sınıf Programı
{
    static void Main(string[] args)
    {
        //Create an instance
        var foo1 = new Foo();
        Console.WriteLine(foo1.NonStaticCounter); //this will print "1"

        //Notice this next call doesn't access the instance but calls by the class name.
        Console.WriteLine(Foo.Counter); //this will also print "1"

        //Create a second instance
        var foo2 = new Foo();

        Console.WriteLine(foo2.NonStaticCounter); //this will print "1"

        Console.WriteLine(Foo.Counter); //this will now print "2"
        //The static property incremented on both instances and can persist for the whole class

    }
}
</code></pre>

## Statik sınıf ömrü
Bir "statik" sınıf, üye erişiminde tembelce başlatılır ve uygulama etki alanı süresince yaşar.

    void Main()
    {
        Console.WriteLine("Static classes are lazily initialized");
        Console.WriteLine("The static constructor is only invoked when the class is first accessed");
        Foo.SayHi();
    
        Console.WriteLine("Reflecting on a type won't trigger its static .ctor");
        var barType = typeof(Bar);
    
        Console.WriteLine("However, you can manually trigger it with System.Runtime.CompilerServices.RuntimeHelpers");
        RuntimeHelpers.RunClassConstructor(barType.TypeHandle);
    }
    
    // Define other methods and classes here
    public static class Foo
    {
        static Foo()
        {
            Console.WriteLine("static Foo.ctor");
        }
        public static void SayHi()
        {
            Console.WriteLine("Foo: Hi");
        }
    }
    public static class Bar
    {
        static Bar()
        {
            Console.WriteLine("static Bar.ctor");
        }
    }

