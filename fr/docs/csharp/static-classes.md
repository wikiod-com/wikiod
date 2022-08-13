---
title: "Classes statiques"
slug: "classes-statiques"
draft: false
images: []
weight: 9944
type: docs
toc: true
---

## Classes statiques
Le mot-clé "static" lorsqu'il fait référence à une classe a trois effets :
1. Vous ** ne pouvez pas ** créer une instance d'une classe statique (cela supprime même le constructeur par défaut)
2. Toutes les propriétés et méthodes de la classe **doivent** également être statiques.
3. Une classe "statique" est une classe "scellée", ce qui signifie qu'elle ne peut pas être héritée.

<pré><code>
classe statique publique Foo
{
    //Notice there is no constructor as this cannot be an instance
    public static int Counter { get; set; }
    public static int GetCount()
    {
        return Counter;
    }
}

Programme de classe publique
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

## Mot-clé statique
Le mot clé static signifie 2 choses :
1. Cette valeur ne change pas d'un objet à l'autre mais change plutôt sur une classe dans son ensemble
2. Les propriétés et méthodes statiques ne nécessitent pas d'instance.
<pré><code>
classe publique Foo
{
    public Foo{
        Counter++;
        NonStaticCounter++;
    }

    public static int Counter { get; set; }
    public int NonStaticCounter { get; set; }
}

Programme de classe publique
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

## Durée de vie de la classe statique
Une classe « statique » est initialisée paresseusement sur l'accès des membres et vit pendant la durée du domaine d'application.

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

