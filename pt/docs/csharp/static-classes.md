---
title: "Classes estáticas"
slug: "classes-estaticas"
draft: false
images: []
weight: 9944
type: docs
toc: true
---

## Classes estáticas
A palavra-chave "static" ao se referir a uma classe tem três efeitos:
1. Você **não pode** criar uma instância de uma classe estática (isso até remove o construtor padrão)
2. Todas as propriedades e métodos da classe **devem** ser estáticos também.
3. Uma classe `static` é uma classe `selada`, o que significa que não pode ser herdada.

<pré><código>
classe estática pública Foo
{
    //Notice there is no constructor as this cannot be an instance
    public static int Counter { get; set; }
    public static int GetCount()
    {
        return Counter;
    }
}

programa de classe publica
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

## Palavra-chave estática
A palavra-chave estática significa 2 coisas:
1. Este valor não muda de objeto para objeto, mas sim em uma classe como um todo
2. Propriedades e métodos estáticos não requerem uma instância.
<pré><código>
classe pública Foo
{
    public Foo{
        Counter++;
        NonStaticCounter++;
    }

    public static int Counter { get; set; }
    public int NonStaticCounter { get; set; }
}

programa de classe publica
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

## Vida útil da classe estática
Uma classe `static` é inicializada preguiçosamente no acesso do membro e vive durante o domínio do aplicativo.

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

