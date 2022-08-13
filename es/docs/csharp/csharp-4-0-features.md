---
title: "Características de C# 4.0"
slug: "caracteristicas-de-c-40"
draft: false
images: []
weight: 9946
type: docs
toc: true
---

## Parámetros opcionales y argumentos con nombre
Podemos omitir el argumento en la llamada si ese argumento es un argumento opcional
Cada argumento opcional tiene su propio valor predeterminado
Tomará el valor predeterminado si no proporcionamos el valor
Un valor predeterminado de un argumento opcional debe ser un
1. Expresión constante.
2. Debe ser un tipo de valor como enum o struct.
3. Debe ser una expresión del formulario default(valueType)

Debe configurarse al final de la lista de parámetros.

Parámetros del método con valores predeterminados:
 
    public void ExampleMethod(int required, string optValue = "test", int optNum = 42)
    {
        //...
    }

Como dijo MSDN, un argumento con nombre,

Le permite pasar el argumento a la función asociando el nombre del parámetro
No es necesario recordar la posición de los parámetros de los que no somos conscientes siempre.
No es necesario buscar el orden de los parámetros en la lista de parámetros de la función llamada.
Podemos especificar parámetros para cada argumento por su nombre.

Argumentos con nombre:
    
    // required = 3, optValue = "test", optNum = 4
    ExampleMethod(3, optNum: 4);
    // required = 2, optValue = "foo", optNum = 42
    ExampleMethod(2, optValue: "foo");
    // required = 6, optValue = "bar", optNum = 1
    ExampleMethod(optNum: 1, optValue: "bar", required: 6);

**Limitación del uso de un argumento con nombre**

La especificación de argumento con nombre debe aparecer después de que se hayan especificado todos los argumentos fijos.

Si usa un argumento con nombre antes de un argumento fijo, obtendrá un error de tiempo de compilación de la siguiente manera.

[![ingrese la descripción de la imagen aquí][1]][1]

La especificación de argumento con nombre debe aparecer después de que se hayan especificado todos los argumentos fijos


[1]: http://i.stack.imgur.com/pzWLh.png

## Variación
Las interfaces genéricas y los delegados pueden tener sus parámetros de tipo marcados como [_covariant_](https://www.wikiod.com/es/docs/c%23/27/generics/7362/covariance#t=201607241842437571339) o [_contravariant_](http:// stackoverflow.com/documentation/c%23/27/generics/7372/contravariance#t=201607241842437571339) utilizando las palabras clave `out` e `in` respectivamente. Luego, estas declaraciones se respetan para las conversiones de tipos, tanto implícitas como explícitas, y tanto en tiempo de compilación como en tiempo de ejecución.

Por ejemplo, la interfaz existente `IEnumerable<T>` se ha redefinido como covariante:

    interface IEnumerable<out T>
    {
        IEnumerator<T> GetEnumerator();
    }

La interfaz existente IComparer<T> se ha redefinido como contravariante:

    public interface IComparer<in T>
    {
        int Compare(T x, T y);
    }

## Búsqueda dinámica de miembros
Se introduce un nuevo pseudotipo "dinámico" en el sistema de tipos de C#. Se trata como `System.Object`, pero además, se permite cualquier acceso de miembro (llamada a método, campo, propiedad o indexador, o una invocación de delegado) o la aplicación de un operador en un valor de ese tipo sin ningún tipo comprobando, y su resolución se pospone hasta el tiempo de ejecución. Esto se conoce como tipificación de pato o enlace tardío. Por ejemplo:
 
    // Returns the value of Length property or field of any object
    int GetLength(dynamic obj)
    {
        return obj.Length;
    }
      
    GetLength("Hello, world");        // a string has a Length property,
    GetLength(new int[] { 1, 2, 3 }); // and so does an array,
    GetLength(42);                    // but not an integer - an exception will be thrown
                                      // in GetLength method at run-time

En este caso, se utiliza el tipo dinámico para evitar una reflexión más detallada. Todavía usa Reflection debajo del capó, pero generalmente es más rápido gracias al almacenamiento en caché.

Esta función está dirigida principalmente a la interoperabilidad con lenguajes dinámicos.

    // Initialize the engine and execute a file
    var runtime = ScriptRuntime.CreateFromConfiguration();
    dynamic globals = runtime.Globals;
    runtime.ExecuteFile("Calc.rb");
    
    // Use Calc type from Ruby
    dynamic calc = globals.Calc.@new();
    calc.valueA = 1337;
    calc.valueB = 666;
    dynamic answer = calc.Calculate();

El tipo dinámico tiene aplicaciones incluso en la mayoría de los códigos escritos de forma estática, por ejemplo, hace que [doble envío] (https://en.wikipedia.org/wiki/Double_dispatch) sea posible sin implementar el patrón Visitor.

## Palabra clave ref opcional cuando se usa COM
La palabra clave ref para llamadores de métodos ahora es opcional cuando se llama a métodos provistos por interfaces COM. Dado un método COM con la firma

    void Increment(ref int x);
la invocación ahora se puede escribir como

    Increment(0); // no need for "ref" or a place holder variable any more

