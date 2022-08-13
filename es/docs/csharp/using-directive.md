---
title: "Uso de directiva"
slug: "uso-de-directiva"
draft: false
images: []
weight: 9956
type: docs
toc: true
---

La palabra clave `using` es tanto una directiva (este tema) como una declaración.

Para la declaración `using` (es decir, para encapsular el alcance de un objeto `IDisposable`, asegurándose de que fuera de ese alcance el objeto se elimine limpiamente), consulte [Using Statement][1].


[1]: https://www.wikiod.com/es/docs/c%23/38/using-statement

## Acceder a miembros estáticos de una clase
<!-- si la versión [gte 6.0] -->

Le permite importar un tipo específico y usar los miembros estáticos del tipo sin calificarlos con el nombre del tipo. Esto muestra un ejemplo usando métodos estáticos:

    using static System.Console;

    // ...

    string GetName()
    {
        WriteLine("Enter your name.");
        return ReadLine();
    }

Y esto muestra un ejemplo usando propiedades y métodos estáticos:

    using static System.Math;

    namespace Geometry
    {
        public class Circle
        {
            public double Radius { get; set; };

            public double Area => PI * Pow(Radius, 2);
        }
    }

<!-- versión final si -->

## Asociar un alias para resolver conflictos
Si usa varios espacios de nombres que pueden tener clases con el mismo nombre (como `System.Random` y `UnityEngine.Random`), puede usar un alias para especificar que `Random` proviene de uno u otro sin tener que usar todo el espacio de nombres en la llamada.

Por ejemplo:

    using UnityEngine;
    using System;

    Random rnd = new Random();

Esto hará que el compilador no esté seguro de qué 'Random' evaluará la nueva variable. En su lugar, puedes hacer:

    using UnityEngine;
    using System;
    using Random = System.Random;

    Random rnd = new Random();

Esto no le impide llamar al otro por su espacio de nombres completamente calificado, así:

    using UnityEngine;
    using System;
    using Random = System.Random;

    Random rnd = new Random();
    int unityRandom = UnityEngine.Random.Range(0,100);

`rnd` será una variable `System.Random` y `unityRandom` será una variable `UnityEngine.Random`.

## Uso de directivas de alias
Puede usar `using` para establecer un alias para un espacio de nombres o tipo. Se pueden encontrar más detalles en [aquí][1].

Sintaxis:

    using <identifier> = <namespace-or-type-name>;

Ejemplo:

    using NewType = Dictionary<string, Dictionary<string,int>>;
    NewType multiDictionary = new NewType();
    //Use instances as you are using the original one
    multiDictionary.Add("test", new Dictionary<string,int>());
 


[1]: https://msdn.microsoft.com/en-us/library/aa664765(v=vs.71).aspx

## Uso básico
    using System;
    using BasicStuff = System;
    using Sayer = System.Console;
    using static System.Console;  //From C# 6
    
    class Program
    {
        public static void Main()
        {
            System.Console.WriteLine("Ignoring usings and specifying full type name");
            Console.WriteLine("Thanks to the 'using System' directive");
            BasicStuff.Console.WriteLine("Namespace aliasing");
            Sayer.WriteLine("Type aliasing");
            WriteLine("Thanks to the 'using static' directive (from C# 6)");
        }
    }



## Hacer referencia a un espacio de nombres
    using System.Text;
    //allows you to access classes within this namespace such as StringBuilder
    //without prefixing them with the namespace.  i.e:

    //...
    var sb = new StringBuilder();
    //instead of
    var sb = new System.Text.StringBuilder();

## Asociar un alias con un espacio de nombres
    using st = System.Text;
    //allows you to access classes within this namespace such as StringBuilder
    //prefixing them with only the defined alias and not the full namespace.  i.e:

    //...
    var sb = new st.StringBuilder();
    //instead of
    var sb = new System.Text.StringBuilder();

