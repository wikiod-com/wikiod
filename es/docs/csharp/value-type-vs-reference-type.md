---
title: "Tipo de valor frente a tipo de referencia"
slug: "tipo-de-valor-frente-a-tipo-de-referencia"
draft: false
images: []
weight: 9929
type: docs
toc: true
---

## Sintaxis
* Pasando por referencia: public void Double(ref int numberToDouble) { }

## Introducción

### Tipos de valor

Los tipos de valor son los más simples de los dos. Los tipos de valor se utilizan a menudo para representar los datos en sí. Un número entero, un valor booleano o un punto en el espacio 3D son ejemplos de buenos tipos de valor.

Los tipos de valor (estructuras) se declaran mediante la palabra clave struct. Consulte la sección de sintaxis para ver un ejemplo de cómo declarar una nueva estructura.

En términos generales, tenemos 2 palabras clave que se utilizan para declarar tipos de valor:

- Estructuras
- Enumeraciones

### Tipos de referencia

Los tipos de referencia son un poco más complejos. Los tipos de referencia son objetos tradicionales en el sentido de la Programación Orientada a Objetos. Por lo tanto, admiten la herencia (y los beneficios de la misma) y también admiten los finalizadores.

En C# generalmente tenemos estos tipos de referencia:
- Clases
- Delegados
- Interfaces

Los nuevos tipos de referencia (clases) se declaran utilizando la palabra clave class. Para ver un ejemplo, consulte la sección de sintaxis sobre cómo declarar un nuevo tipo de referencia.

## Grandes diferencias

Las principales diferencias entre los tipos de referencia y los tipos de valor se pueden ver a continuación.

### Los tipos de valor existen en la pila, los tipos de referencia existen en el montón

Esta es la diferencia que se menciona a menudo entre los dos, pero en realidad, se reduce a que cuando usa un tipo de valor en C#, como un int, el programa usará esa variable para referirse directamente a ese valor. Si dice int mine = 0, entonces la variable mine se refiere directamente a 0, lo cual es eficiente. Sin embargo, los tipos de referencia en realidad contienen (como sugiere el nombre) una referencia al objeto subyacente, esto es similar a los punteros en otros lenguajes como C++.

Es posible que no notes los efectos de esto de inmediato, pero los efectos están ahí, son poderosos y sutiles. Consulte el ejemplo sobre el cambio de tipos de referencia en otro lugar para ver un ejemplo.

Esta diferencia es la razón principal de las siguientes otras diferencias y vale la pena conocerla.

### Los tipos de valor no cambian cuando los cambia en un método, los tipos de referencia sí

Cuando se pasa un tipo de valor a un método como parámetro, si el método cambia el valor de alguna manera, el valor no cambia. Por el contrario, pasar un tipo de referencia a ese mismo método y cambiarlo cambiará el objeto subyacente, de modo que otras cosas que usan ese mismo objeto tendrán el objeto recién cambiado en lugar de su valor original.

Consulte el ejemplo de tipos de valor frente a tipos de referencia en métodos para obtener más información.

#### ¿Qué pasa si quiero cambiarlos?

Simplemente páselos a su método usando la palabra clave "ref", y luego pasará este objeto por referencia. Es decir, es el mismo objeto en la memoria. Así que las modificaciones que hagas serán respetadas. Consulte el ejemplo sobre pasar por referencia para ver un ejemplo.

### Los tipos de valor no pueden ser nulos, los tipos de referencia pueden

Más o menos como dice, puede asignar nulo a un tipo de referencia, lo que significa que la variable que ha asignado no puede tener ningún objeto real asignado. En el caso de tipos de valor, sin embargo, esto no es posible. Sin embargo, puede usar Nullable<Type>, para permitir que su tipo de valor sea anulable, si este es un requisito, aunque si esto es algo que está considerando, piense seriamente si una clase podría no ser el mejor enfoque aquí, si es es tu propio tipo.

## Pasando por referencia usando la palabra clave ref.

De la [documentación][1] :


> En C#, los argumentos se pueden pasar a los parámetros ya sea por valor o por
> referencia. Pasar por referencia habilita miembros de función, métodos,
> propiedades, indexadores, operadores y constructores para cambiar el valor
> de los parámetros y hacer que ese cambio persista en la llamada
> ambiente. Para pasar un parámetro por referencia, use `ref` o `out`
> palabra clave.

La diferencia entre `ref` y `out` es que `out` significa que el parámetro pasado debe asignarse antes de que finalice la función. Por el contrario, los parámetros pasados ​​con `ref` se pueden cambiar o dejar sin cambios.


    using System;

    class Program
    {
        static void Main(string[] args)
        {
            int a = 20;
            Console.WriteLine("Inside Main - Before Callee: a = {0}", a);
            Callee(a);
            Console.WriteLine("Inside Main - After Callee: a = {0}", a);
            
            Console.WriteLine("Inside Main - Before CalleeRef: a = {0}", a);
            CalleeRef(ref a);
            Console.WriteLine("Inside Main - After CalleeRef: a = {0}", a);
         
            Console.WriteLine("Inside Main - Before CalleeOut: a = {0}", a);
            CalleeOut(out a);
            Console.WriteLine("Inside Main - After CalleeOut: a = {0}", a);
            
            Console.ReadLine();
        }
    
        static void Callee(int a)
        {
            a = 5;
            Console.WriteLine("Inside Callee a : {0}", a);
        }
    
        static void CalleeRef(ref int a)
        {
            a = 6;
            Console.WriteLine("Inside CalleeRef a : {0}", a);
        }
        
        static void CalleeOut(out int a)
        {
            a = 7;
            Console.WriteLine("Inside CalleeOut a : {0}", a);
        }
    }

**Producción** :

    Inside Main - Before Callee: a = 20
    Inside Callee a : 5
    Inside Main - After Callee: a = 20
    Inside Main - Before CalleeRef: a = 20
    Inside CalleeRef a : 6
    Inside Main - After CalleeRef: a = 6
    Inside Main - Before CalleeOut: a = 6
    Inside CalleeOut a : 7
    Inside Main - After CalleeOut: a = 7

[1]: https://msdn.microsoft.com/en-IN/library/0f66670z.aspx


## Cambiar valores en otro lugar
<!-- lenguaje-todo: c# -->
```
public static void Main(string[] args)
{
    var studentList = new List<Student>();
    studentList.Add(new Student("Scott", "Nuke"));
    studentList.Add(new Student("Vincent", "King"));
    studentList.Add(new Student("Craig", "Bertt"));

    // make a separate list to print out later
    var printingList = studentList; // this is a new list object, but holding the same student objects inside it

    // oops, we've noticed typos in the names, so we fix those
    studentList[0].LastName = "Duke";
    studentList[1].LastName = "Kong";
    studentList[2].LastName = "Brett";

    // okay, we now print the list
    PrintPrintingList(printingList);
}

private static void PrintPrintingList(List<Student> students)
{
    foreach (Student student in students)
    {
        Console.WriteLine(string.Format("{0} {1}", student.FirstName, student.LastName));
    }
}
```

Notará que a pesar de que la lista printList se hizo antes de las correcciones a los nombres de los estudiantes después de los errores tipográficos, el método PrintPrintingList todavía imprime los nombres corregidos:

    Scott Duke
    Vincent Kong
    Craig Brett

Esto se debe a que ambas listas contienen una lista de referencias de los mismos estudiantes. Por lo tanto, cambiar el objeto de estudiante subyacente se propaga a los usos de cualquiera de las listas.

Así es como se vería la clase de estudiantes.

```
public class Student
{
    public string FirstName { get; set; }
    public string LastName { get; set; }

    public Student(string firstName, string lastName)
    {
        this.FirstName = firstName;
        this.LastName = lastName;
    }
}
```


## Pasando por referencia
Si desea que el ejemplo de tipos de valor frente a tipos de referencia en métodos funcione correctamente, use la palabra clave ref en la firma de su método para el parámetro que desea pasar por referencia, así como cuando llame al método.
<!-- lenguaje-todo: c# -->

```
public static void Main(string[] args)
{
    ...
    DoubleNumber(ref number); // calling code
    Console.WriteLine(number); // outputs 8
    ...
}
```

```
public void DoubleNumber(ref int number)
{
    number += number;
}
```

Hacer estos cambios haría que el número se actualizara como se esperaba, lo que significa que la salida de la consola para el número sería 8.

## Asignación


## Diferencia con los parámetros del método ref y out
Hay dos formas posibles de pasar un tipo de valor por referencia: `ref` y `out`. La diferencia es que al pasarlo con `ref` se debe inicializar el valor pero no al pasarlo con `out`. Usar `out` asegura que la variable tenga un valor después de la llamada al método:

    public void ByRef(ref int value)
    {
        Console.WriteLine(nameof(ByRef) + value);
        value += 4;
        Console.WriteLine(nameof(ByRef) + value);
    }

    public void ByOut(out int value)
    {
        value += 4 // CS0269: Use of unassigned out parameter `value'  
        Console.WriteLine(nameof(ByOut) + value); // CS0269: Use of unassigned out parameter `value'  

        value = 4;
        Console.WriteLine(nameof(ByOut) + value);
    }

    public void TestOut()
    {
        int outValue1;
        ByOut(out outValue1); // prints 4

        int outValue2 = 10;   // does not make any sense for out
        ByOut(out outValue2); // prints 4
    }

    public void TestRef()
    {
        int refValue1;
        ByRef(ref refValue1); // S0165  Use of unassigned local variable 'refValue'

        int refValue2 = 0;
        ByRef(ref refValue2); // prints 0 and 4

        int refValue3 = 10;
        ByRef(ref refValue3); // prints 10 and 14
    }

El problema es que al usar `out`, el parámetro `debe` inicializarse antes de salir del método, por lo tanto, el siguiente método es posible con `ref` pero no con `out`:


    public void EmtyRef(bool condition, ref int value)
    {
        if (condition)
        {
            value += 10;
        }
    }

    public void EmtyOut(bool condition, out int value)
    {
        if (condition)
        {
            value = 10;
        }
    } //CS0177: The out parameter 'value' must be assigned before control leaves the current method

Esto se debe a que si la "condición" no se cumple, el "valor" no se asigna.

## ref vs parámetros de salida


