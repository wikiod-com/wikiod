---
title: "Clases estáticas"
slug: "clases-estaticas"
draft: false
images: []
weight: 9944
type: docs
toc: true
---

## Clases estáticas
La palabra clave "estática" cuando se refiere a una clase tiene tres efectos:
1. **No puedes** crear una instancia de una clase estática (esto incluso elimina el constructor predeterminado)
2. Todas las propiedades y métodos de la clase **deben** ser estáticos también.
3. Una clase `estática` es una clase `sellada`, lo que significa que no se puede heredar.

<pre><código>
clase estática pública Foo
{
    //Notice there is no constructor as this cannot be an instance
    public static int Counter { get; set; }
    public static int GetCount()
    {
        return Counter;
    }
}

Programa de clase pública
{
    static void Main(string[] args)
    {
        Foo.Counter++;
        Console.WriteLine(Foo.GetCount()); //this will print 1
        
        //var foo1 = new Foo(); 
        //this line would break the code as the Foo class does not have a constructor
    }
}
</código></pre>

## Palabra clave estática
La palabra clave estática significa 2 cosas:
1. Este valor no cambia de un objeto a otro, sino que cambia en una clase como un todo
2. Las propiedades y métodos estáticos no requieren una instancia.
<pre><código>
Foo de clase pública
{
    public Foo{
        Counter++;
        NonStaticCounter++;
    }

    public static int Counter { get; set; }
    public int NonStaticCounter { get; set; }
}

Programa de clase pública
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
</código></pre>

## Duración de la clase estática
Una clase 'estática' se inicializa de forma perezosa en el acceso de los miembros y vive mientras dure el dominio de la aplicación.

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

