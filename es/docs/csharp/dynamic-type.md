---
title: "tipo dinámico"
slug: "tipo-dinamico"
draft: false
images: []
weight: 9934
type: docs
toc: true
---

La palabra clave `dynamic` declara una variable cuyo tipo no se conoce en tiempo de compilación. Una variable `dinámica` puede contener cualquier valor, y el tipo del valor puede cambiar durante el tiempo de ejecución.

Como se indica en el libro "Metaprogramación en .NET", C# no tiene un tipo de respaldo para la palabra clave `dinámica`:

> La funcionalidad habilitada por la palabra clave `dynamic` es un conjunto inteligente de acciones del compilador que emiten y usan objetos `CallSite` en el contenedor del sitio del ámbito de ejecución local. El compilador gestiona lo que los programadores perciben como objeto dinámico.
referencias a través de esas instancias `CallSite`. Los parámetros, tipos de devolución, campos y propiedades que reciben un tratamiento dinámico en el momento de la compilación pueden marcarse con algunos metadatos para indicar que se generaron para uso dinámico, pero el tipo de datos subyacente para ellos siempre será `System.Object`.

 

## Creando un objeto dinámico con propiedades
    using System;
    using System.Dynamic;
    
    dynamic info = new ExpandoObject();
    info.Id = 123;
    info.Another = 456;
    
    Console.WriteLine(info.Another);
    // 456
    
    Console.WriteLine(info.DoesntExist);
    // Throws RuntimeBinderException

## Creando una variable dinámica
    dynamic foo = 123;
    Console.WriteLine(foo + 234);
    // 357    Console.WriteLine(foo.ToUpper())
    // RuntimeBinderException, since int doesn't have a ToUpper method

    foo = "123";
    Console.WriteLine(foo + 234);
    // 123234
    Console.WriteLine(foo.ToUpper()):
    // NOW A STRING

## Dinámica de retorno
    using System;

    public static void Main()
    {
        var value = GetValue();
        Console.WriteLine(value);
        // dynamics are useful!
    }
    
    private static dynamic GetValue()
    {
        return "dynamics are useful!";
    }

## Manejo de tipos específicos desconocidos en tiempo de compilación


