---
title: "Función con múltiples valores de retorno"
slug: "funcion-con-multiples-valores-de-retorno"
draft: false
images: []
weight: 9923
type: docs
toc: true
---

No hay una respuesta inherente en C# a esta llamada necesidad. No obstante, existen soluciones alternativas para satisfacer esta necesidad.

La razón por la que califico la necesidad como "supuestamente" es que solo necesitamos métodos con 2 o más de 2 valores para regresar cuando violamos los buenos principios de programación. Especialmente el [Principio de Responsabilidad Única][1].

Por lo tanto, sería mejor recibir alertas cuando necesitemos funciones que devuelvan 2 o más valores y mejorar nuestro diseño.


[1]: https://en.wikipedia.org/wiki/Single_responsibility_principle

## Solución "objeto anónimo" + "palabra clave dinámica"
Puede devolver un objeto anónimo desde su función

    public static object FunctionWithUnknowReturnValues ()
    {
        /// anonymous object
        return new { a = 1, b = 2 };
    }

Y asigne el resultado a un objeto dinámico y lea los valores en él.

    /// dynamic object
    dynamic x = FunctionWithUnknowReturnValues();

    Console.WriteLine(x.a);
    Console.WriteLine(x.b);

## Solución de tupla
Puede devolver una instancia de la clase `Tuple` desde su función con dos parámetros de plantilla como `Tuple<string, MyClass>`:

    public Tuple<string, MyClass> FunctionWith2ReturnValues ()
    {
        return Tuple.Create("abc", new MyClass());
    }

Y lea los valores como a continuación:

    Console.WriteLine(x.Item1);
    Console.WriteLine(x.Item2);

## Parámetros de referencia y salida
La palabra clave `ref` se usa para pasar un [Argumento como referencia][1]. `out` hará lo mismo que `ref` pero no requiere un valor asignado por la persona que llama antes de llamar a la función.

**Parámetro de referencia**: si desea pasar una variable como parámetro de referencia, debe inicializarla antes de pasarla como parámetro de referencia al método.

**Parámetro de salida:-**
Si desea pasar una variable como parámetro de salida, no necesita inicializarla antes de pasarla como parámetro de salida al método.

    static void Main(string[] args)
    {
        int a = 2;
        int b = 3;
        int add = 0;
        int mult= 0;
        AddOrMult(a, b, ref add, ref mult); //AddOrMult(a, b, out add, out mult);
        Console.WriteLine(add); //5
        Console.WriteLine(mult); //6
    }
    
    private static void AddOrMult(int a, int b, ref int add, ref int mult) //AddOrMult(int a, int b, out int add, out int mult)
    {
        add = a + b;
        mult = a * b;
    }


[1]: https://www.wikiod.com/es/docs/c%23/3014/value-type-vs-reference-type#t=201607261617231313768

