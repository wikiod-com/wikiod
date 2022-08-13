---
title: "Tipos personalizados"
slug: "tipos-personalizados"
draft: false
images: []
weight: 9975
type: docs
toc: true
---

Por lo general, una `estructura` se usa solo cuando el rendimiento es muy importante. Dado que los tipos de valor viven en la pila, se puede acceder a ellos mucho más rápido que a las clases. Sin embargo, la pila tiene mucho menos espacio que el montón, por lo que las estructuras deben mantenerse pequeñas (Microsoft recomienda que las estructuras no ocupen más de 16 bytes).

Una `clase` es el tipo más utilizado (de estos tres) en C# y, por lo general, es lo primero que debe elegir.

Un `enum` se usa siempre que pueda tener una lista distinta y claramente definida de elementos que solo necesitan definirse una vez (en tiempo de compilación). Las enumeraciones son útiles para los programadores como una referencia ligera a algún valor: en lugar de definir una lista de variables "constantes" para comparar, puede usar una enumeración y obtener soporte de Intellisense para asegurarse de no usar accidentalmente un valor incorrecto.

## Definición de estructura
Las estructuras se heredan de System.ValueType, son tipos de valor y viven en la pila. Cuando los tipos de valor se pasan como parámetro, se pasan por valor.
-------------------------------------------------- -------------------------------------

    Struct MyStruct
    {
        public int x;
        public int y;
    }

**Pasado por valor** significa que el valor del parámetro se *copia* para el método, y los cambios realizados en el parámetro en el método no se reflejan fuera del método. Por ejemplo, considere el siguiente código, que llama a un método llamado `AddNumbers`, pasando las variables `a` y `b`, que son del tipo `int`, que es un tipo Value.

    int a = 5;
    int b = 6;
    
    AddNumbers(a,b);

    public AddNumbers(int x, int y)
    {
        int z = x + y; // z becomes 11
        x = x + 5; // now we changed x to be 10
        z = x + y; // now z becomes 16
    } 

Aunque agregamos 5 a `x` dentro del método, el valor de `a` permanece sin cambios, porque es un tipo de valor, y eso significa que `x` era una *copia* del valor de `a`, pero no realmente `a`.

Recuerde, los tipos de valor viven en la pila y se pasan por valor.

    

## Definición de clase
Las clases heredan de System.Object, son tipos de referencia y viven en el montón. Cuando los tipos de referencia se pasan como parámetro, se pasan por referencia.
-------------------------------------------------- -------------------------------------


    public Class MyClass
    {
        public int a;
        public int b;
    }

**Pasado por referencia** significa que una *referencia* al parámetro se pasa al método, y cualquier cambio en el parámetro se reflejará fuera del método cuando regrese, porque la referencia es *exactamente al mismo objeto en memoria*. Usemos el mismo ejemplo que antes, pero primero "envolveremos" los `int`s en una clase.

    MyClass instanceOfMyClass = new MyClass();
    instanceOfMyClass.a = 5;
    instanceOfMyClass.b = 6;
    
    AddNumbers(instanceOfMyClass);
    
    public AddNumbers(MyClass sample)
    {
        int z = sample.a + sample.b; // z becomes 11
        sample.a = sample.a + 5; // now we changed a to be 10
        z = sample.a + sample.b; // now z becomes 16
    } 

Esta vez, cuando cambiamos `sample.a` a `10`, el valor de `instanceOfMyClass.a` *también* cambia, porque *pasó por referencia*. Pasado por referencia significa que se pasó al método una *referencia* (también llamada a veces *puntero*) al objeto, en lugar de una copia del objeto mismo.

Recuerde, los tipos de referencia viven en el montón y se pasan por referencia.

## Definición de enumeración
Una enumeración es un tipo especial de clase. La palabra clave `enum` le dice al compilador que esta clase hereda de la clase abstracta System.Enum. Las enumeraciones se utilizan para distintas listas de elementos.
-------------------------------------------------- -------------------------------------

    
    public enum MyEnum
    {
        Monday = 1,
        Tuesday,
        Wednesday,
        //...
    }

Puede pensar en una enumeración como una forma conveniente de asignar constantes a algún valor subyacente. La enumeración definida anteriormente declara valores para cada día de la semana y comienza con `1`. 'Martes' se asignaría automáticamente a '2', 'Miércoles' a '3', etc.

De forma predeterminada, las enumeraciones usan `int` como el tipo subyacente y comienzan en 0, pero puede usar cualquiera de los siguientes *tipos integrales*: `byte, sbyte, short, ushort, int, uint, long o ulong`, y puede especificar valores explícitos para cualquier elemento. Si algunos elementos se especifican explícitamente, pero otros no, cada elemento después del último definido se incrementará en 1.

Usaríamos este ejemplo *convirtiendo* algún otro valor en *MyEnum* así:

    MyEnum instance = (MyEnum)3; // the variable named 'instance' gets a 
                                 //value of MyEnum.Wednesday, which maps to 3.

    int x = 2;
    instance = (MyEnum)x; // now 'instance' has a value of MyEnum.Tuesday

Otro tipo de enumeración útil, aunque más complejo, se llama `Flags`. Al *decorar* una enumeración con el atributo `Flags`, puede asignar a una variable más de un valor a la vez. Tenga en cuenta que al hacer esto, *debe* definir valores explícitamente en la representación de base 2.

    [Flags]
    public enum MyEnum
    {
        Monday = 1,
        Tuesday = 2,
        Wednesday = 4,
        Thursday = 8,
        Friday = 16,
        Saturday = 32, 
        Sunday = 64
    }

Ahora puede comparar más de un valor a la vez, ya sea usando *comparaciones bit a bit* o, si está usando .NET 4.0 o posterior, el método integrado `Enum.HasFlag`.

    MyEnum instance = MyEnum.Monday | MyEnum.Thursday; // instance now has a value of
                                                       // *both* Monday and Thursday,
                                                       // represented by (in binary) 0100. 

    if (instance.HasFlag(MyEnum.Wednesday))
    {
        // it doesn't, so this block is skipped
    }
    else if (instance.HasFlag(MyEnum.Thursday))
    {
        // it does, so this block is executed
    }



Dado que la clase Enum es una subclase de `System.ValueType`, se trata como un tipo de valor y se pasa por valor, no por referencia. El objeto base se crea en el montón, pero cuando pasa un valor de enumeración a una llamada de función, se inserta en la pila una copia del valor que usa el tipo de valor subyacente de Enum (normalmente System.Int32). El compilador rastrea la asociación entre este valor y el objeto base que se creó en la pila. Consulte [ValueType Class (System) (MSDN)][1] para obtener más información.


[1]: https://msdn.microsoft.com/en-us/library/system.valuetype(v=vs.110).aspx

