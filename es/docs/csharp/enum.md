---
title: "enumeración"
slug: "enumeracion"
draft: false
images: []
weight: 9420
type: docs
toc: true
---

Una enumeración puede derivar de cualquiera de los siguientes tipos: byte, sbyte, short, ushort, int, uint, long, ulong. El valor predeterminado es int y se puede cambiar especificando el tipo en la definición de enumeración:

public enum Día de la semana: byte {lunes = 1, martes = 2, miércoles = 3, jueves = 4, viernes = 5}

Esto es útil cuando se invoca/P a código nativo, se asigna a fuentes de datos y circunstancias similares. En general, se debe usar el int predeterminado, porque la mayoría de los desarrolladores esperan que una enumeración sea un int.

## Sintaxis
- enum Colors { Red, Green, Blue } // Declaración de enumeración
- enum Colors : byte { Red, Green, Blue } // Declaración con tipo específico
- enum Colors { Red = 23, Green = 45, Blue = 12 } // Declaración con valores definidos
- Colors.Red // Accede a un elemento de un Enum
- int value = (int)Colors.Red // Obtener el valor int de un elemento enumerado
- Colors color = (Colors)intValue // Obtener un elemento de enumeración de int

Un Enum (abreviatura de "tipo enumerado") es un tipo que consta de un conjunto de constantes con nombre, representadas por un identificador específico del tipo.

Las enumeraciones son más útiles para representar conceptos que tienen un número (generalmente pequeño) de posibles valores discretos. Por ejemplo, se pueden utilizar para representar un día de la semana o un mes del año. También se pueden usar como banderas que se pueden combinar o verificar, usando operaciones bit a bit.

## Enumerar como banderas
El `FlagsAttribute` se puede aplicar a una enumeración cambiando el comportamiento de `ToString()` para que coincida con la naturaleza de la enumeración:

    [Flags]
    enum MyEnum
    {
        //None = 0, can be used but not combined in bitwise operations
        FlagA = 1,
        FlagB = 2,
        FlagC = 4,
        FlagD = 8  
        //you must use powers of two or combinations of powers of two 
        //for bitwise operations to work
    }
    
    var twoFlags = MyEnum.FlagA | MyEnum.FlagB;
    
    // This will enumerate all the flags in the variable: "FlagA, FlagB".
    Console.WriteLine(twoFlags);

Debido a que `FlagsAttribute` se basa en que las constantes de enumeración son potencias de dos (o sus combinaciones) y los valores de enumeración son, en última instancia, valores numéricos, está limitado por el tamaño del tipo numérico subyacente. El tipo numérico disponible más grande que puede usar es `UInt64`, que le permite especificar 64 constantes de enumeración de marca distintas (no combinadas). La palabra clave `enum` tiene por defecto el tipo subyacente `int`, que es `Int32`. El compilador permitirá la declaración de valores de más de 32 bits. Esos se cerrarán sin una advertencia y darán como resultado dos o más miembros de enumeración del mismo valor. Por lo tanto, si una enumeración está destinada a acomodar un conjunto de bits de más de 32 indicadores, debe especificar un tipo más grande explícitamente:

    public enum BigEnum : ulong
    {
        BigValue = 1 << 63
    }

Aunque los indicadores suelen ser de un solo bit, se pueden combinar en "conjuntos" con nombre para facilitar su uso.

    [Flags]
    enum FlagsEnum
    {
        None = 0,
        Option1 = 1,
        Option2 = 2,
        Option3 = 4,
           
        Default = Option1 | Option3,
        All = Option1 | Option2 | Option3,
    }

Para evitar deletrear los valores decimales de potencias de dos, el [operador de desplazamiento a la izquierda (<<)](https://msdn.microsoft.com/en-gb/library/a1sway8w.aspx) también se puede usar para declarar la misma enumeración

    [Flags]
    enum FlagsEnum
    {
        None = 0,
        Option1 = 1 << 0,
        Option2 = 1 << 1,
        Option3 = 1 << 2,
           
        Default = Option1 | Option3,
        All = Option1 | Option2 | Option3,
    }

A partir de C# 7.0, también se pueden usar [literales binarios](https://www.wikiod.com/es/docs/c%23/1936/c-sharp-7-0-features/6327/binary-literals#t=201705181538117083427) .

Para verificar si el valor de la variable enum tiene un determinado indicador establecido, se puede usar el método [`HasFlag`][1]. digamos que tenemos

    [Flags]
    enum MyEnum
    {
        One = 1,
        Two = 2,
        Three = 4
    }

Y un `valor`
    
    var value = MyEnum.One | MyEnum.Two;

Con `HasFlag` podemos comprobar si alguna de las banderas está activada
    
    if(value.HasFlag(MyEnum.One))
        Console.WriteLine("Enum has One");

    if(value.HasFlag(MyEnum.Two))
        Console.WriteLine("Enum has Two");

    if(value.HasFlag(MyEnum.Three))
        Console.WriteLine("Enum has Three");

También podemos iterar a través de todos los valores de enumeración para obtener todas las banderas que están configuradas

    var type = typeof(MyEnum);
    var names = Enum.GetNames(type);

    foreach (var name in names)
    {
        var item = (MyEnum)Enum.Parse(type, name);

        if (value.HasFlag(item))
            Console.WriteLine("Enum has " + name);
    }
    
O

    foreach(MyEnum flagToCheck in Enum.GetValues(typeof(MyEnum)))
    {
        if(value.HasFlag(flagToCheck))
        {
             Console.WriteLine("Enum has " + flagToCheck);
        }
    }

Los tres ejemplos se imprimirán:

    Enum has One
    Enum has Two


[1]: https://msdn.microsoft.com/en-us/library/system.enum.hasflag(v=vs.110).aspx

## Conceptos básicos de enumeración

Desde [MSDN][1]:
> Un tipo de enumeración (también llamado enumeración o enum) proporciona una manera eficiente de definir un conjunto de **constantes integrales** nombradas que se pueden **asignar a una variable**.

Esencialmente, una enumeración es un tipo que solo permite un conjunto finito de opciones, y cada opción corresponde a un número. De forma predeterminada, esos números aumentan en el orden en que se declaran los valores, comenzando desde cero. Por ejemplo, se podría declarar una enumeración para los días de la semana:

    public enum Day
    {
        Monday,
        Tuesday,
        Wednesday,
        Thursday,
        Friday,
        Saturday,
        Sunday
    }

Esa enumeración podría usarse así:

    // Define variables with values corresponding to specific days
    Day myFavoriteDay = Day.Friday;
    Day myLeastFavoriteDay = Day.Monday;
    
    // Get the int that corresponds to myFavoriteDay
    // Friday is number 4
    int myFavoriteDayIndex = (int)myFavoriteDay;
    
    // Get the day that represents number 5
    Day dayFive = (Day)5;

De forma predeterminada, el tipo subyacente de cada elemento en `enum` es `int`, pero `byte`, `sbyte`, `short`, `ushort`, `uint`, `long` y `ulong` se pueden usar como bien. Si usa un tipo que no sea `int`, debe especificar el tipo usando dos puntos después del nombre de la enumeración:

    public enum Day : byte 
    {
        // same as before 
    }

Los números después del nombre ahora son bytes en lugar de números enteros. Puede obtener el tipo subyacente de la enumeración de la siguiente manera:

    Enum.GetUnderlyingType(typeof(Days)));

Producción:

<!-- idioma: ninguno -->
    System.Byte

Demostración: [violín .NET][2]

[1]: https://msdn.microsoft.com/en-us/library/cc138362.aspx

[2]: https://dotnetfiddle.net/EGi301

## Usando la notación << para banderas
El operador de desplazamiento a la izquierda (`<<`) se puede usar en declaraciones de enumeración de banderas para garantizar que cada bandera tenga exactamente un `1` en representación binaria, como deberían hacer las banderas.

Esto también ayuda a mejorar la legibilidad de enumeraciones grandes con muchas banderas en ellas.


    [Flags]
    public enum MyEnum 
    {
        None  = 0,
        Flag1 = 1 << 0,
        Flag2 = 1 << 1,
        Flag3 = 1 << 2,
        Flag4 = 1 << 3,
        Flag5 = 1 << 4,
        ...
        Flag31 = 1 << 30
    }

Ahora es obvio que 'MyEnum' contiene solo las banderas adecuadas y no cosas desordenadas como 'Flag30 = 1073741822' (o 1111111111111111111111111111110 en binario) que es inapropiado.

## Pruebe los valores de enumeración de estilo de banderas con lógica bit a bit
Un valor de enumeración de estilo de banderas debe probarse con lógica bit a bit porque es posible que no coincida con ningún valor único.

    [Flags]
    enum FlagsEnum
    {
        Option1 = 1,
        Option2 = 2,
        Option3 = 4,
        Option2And3 = Option2 | Option3;
    
        Default = Option1 | Option3,
    }
    
El valor `Predeterminado` es en realidad una combinación de otros dos _fusionados_ con un OR bit a bit. Por lo tanto, para probar la presencia de una bandera, necesitamos usar un AND bit a bit.

    var value = FlagsEnum.Default;

    bool isOption2And3Set = (value & FlagsEnum.Option2And3) == FlagsEnum.Option2And3;

    Assert.True(isOption2And3Set);



## Enumeración a cadena y viceversa
    public enum DayOfWeek
    {
        Sunday,
        Monday,
        Tuesday,
        Wednesday,
        Thursday,
        Friday,
        Saturday
    }
    
        
    // Enum to string
    string thursday = DayOfWeek.Thursday.ToString(); // "Thursday"
    
    string seventhDay = Enum.GetName(typeof(DayOfWeek), 6); // "Saturday"
    
    string monday = Enum.GetName(typeof(DayOfWeek), DayOfWeek.Monday); // "Monday"
    
    
    // String to enum (.NET 4.0+ only - see below for alternative syntax for earlier .NET versions)
    DayOfWeek tuesday;
    Enum.TryParse("Tuesday", out tuesday); // DayOfWeek.Tuesday
    
    DayOfWeek sunday;
    bool matchFound1 = Enum.TryParse("SUNDAY", out sunday); // Returns false (case-sensitive match)
    
    DayOfWeek wednesday;
    bool matchFound2 = Enum.TryParse("WEDNESDAY", true, out wednesday); // Returns true; DayOfWeek.Wednesday (case-insensitive match)
    
    
    // String to enum (all .NET versions)
    DayOfWeek friday = (DayOfWeek)Enum.Parse(typeof(DayOfWeek), "Friday"); // DayOfWeek.Friday

    DayOfWeek caturday = (DayOfWeek)Enum.Parse(typeof(DayOfWeek), "Caturady"); // Thows ArgumentException
    
    // All names of an enum type as strings
    string[] weekdays = Enum.GetNames(typeof(DayOfWeek));

## Agregar y eliminar valores de la enumeración marcada
Este código es para agregar y eliminar un valor de una instancia de enumeración marcada:

    [Flags]
    public enum MyEnum
    {
        Flag1 = 1 << 0,
        Flag2 = 1 << 1,
        Flag3 = 1 << 2
    }

    var value = MyEnum.Flag1;

    // set additional value
    value |= MyEnum.Flag2;  //value is now Flag1, Flag2
    value |= MyEnum.Flag3;  //value is now Flag1, Flag2, Flag3

    // remove flag
    value &= ~MyEnum.Flag2; //value is now Flag1, Flag3    


## Valor predeterminado para enumeración == CERO
**El valor predeterminado para una enumeración es cero**. Si una enumeración no define un elemento con un valor de cero, su valor predeterminado será cero.
    
    public class Program
    {        
        enum EnumExample
        {
            one = 1,
            two = 2
        }
        
        public void Main()
        {              
            var e = default(EnumExample);
            
            if (e == EnumExample.one)
                Console.WriteLine("defaults to one");
            else
                Console.WriteLine("Unknown");    
        }    
    }

Ejemplo:
https://dotnetfiddle.net/l5Rwie

## Agregar información de descripción adicional a un valor de enumeración
En algunos casos, es posible que desee agregar una descripción adicional a un valor de enumeración, por ejemplo, cuando el valor de enumeración en sí mismo es menos legible de lo que desea mostrar al usuario. En tales casos, puede usar la clase [`System.ComponentModel.DescriptionAttribute`](https://msdn.microsoft.com/en-us/library/system.componentmodel.descriptionattribute(v=vs.110).aspx).

Por ejemplo:

    public enum PossibleResults
    {
        [Description("Success")]
        OK = 1,
        [Description("File not found")]
        FileNotFound = 2,
        [Description("Access denied")]
        AccessDenied = 3
    }

Ahora, si desea devolver la descripción de un valor de enumeración específico, puede hacer lo siguiente:

    public static string GetDescriptionAttribute(PossibleResults result)
    {
            return ((DescriptionAttribute)Attribute.GetCustomAttribute((result.GetType().GetField(result.ToString())), typeof(DescriptionAttribute))).Description;
    }

    static void Main(string[] args)
    {
        PossibleResults result = PossibleResults.FileNotFound;
        Console.WriteLine(result); // Prints "FileNotFound"
        Console.WriteLine(GetDescriptionAttribute(result)); // Prints "File not found"
    }

Esto también se puede transformar fácilmente en un método de extensión para todas las enumeraciones:

    static class EnumExtensions
    {
        public static string GetDescription(this Enum enumValue)
        {
            return ((DescriptionAttribute)Attribute.GetCustomAttribute((enumValue.GetType().GetField(enumValue.ToString())), typeof(DescriptionAttribute))).Description;
        }
    }

Y luego se usa fácilmente así:
`Consola.WriteLine(resultado.GetDescription());`


## Las enumeraciones pueden tener valores inesperados
Dado que una enumeración se puede convertir hacia y desde su tipo integral subyacente, el valor puede quedar fuera del rango de valores dado en la definición del tipo de enumeración.

Aunque el siguiente tipo de enumeración `DaysOfWeek` solo tiene 7 valores definidos, aún puede contener cualquier valor `int`.

    public enum DaysOfWeek
    {
        Monday = 1,
        Tuesday = 2,
        Wednesday = 3,
        Thursday = 4,
        Friday = 5,
        Saturday = 6,
        Sunday = 7
    }

    DaysOfWeek d = (DaysOfWeek)31;
    Console.WriteLine(d); // prints 31

    DaysOFWeek s = DaysOfWeek.Sunday;
    s++; // No error

Actualmente no hay forma de definir una enumeración que no tenga este comportamiento.

Sin embargo, los valores de enumeración no definidos se pueden detectar utilizando el método `Enum.IsDefined`. Por ejemplo,

    DaysOfWeek d = (DaysOfWeek)31;
    Console.WriteLine(Enum.IsDefined(typeof(DaysOfWeek),d)); // prints False

## Obtener todos los valores de los miembros de una enumeración
    enum MyEnum
    {
        One,
        Two,
        Three
    }
    
    foreach(MyEnum e in Enum.GetValues(typeof(MyEnum)))
        Console.WriteLine(e);

Esto imprimirá:

    One
    Two
    Three

## Manipulación bit a bit usando enumeraciones
El [FlagsAttribute][1] debe usarse siempre que el enumerable represente una colección de banderas, en lugar de un solo valor.
El valor numérico asignado a cada valor de enumeración ayuda cuando se manipulan enumeraciones mediante operadores bit a bit.


**Ejemplo 1: Con [Banderas]**

    [Flags]
    enum Colors
    {
        Red=1,
        Blue=2,
        Green=4,
        Yellow=8
    }

    var color = Colors.Red | Colors.Blue;
    Console.WriteLine(color.ToString());

> estampados Rojo,Azul

    

****Ejemplo 2: Sin [Banderas]****

  
    enum Colors
    {
        Red=1,
        Blue=2,
        Green=4,
        Yellow=8
    }
    var color = Colors.Red | Colors.Blue;
    Console.WriteLine(color.ToString());

> estampados 3


[1]: https://msdn.microsoft.com/en-us/library/system.flagsattribute(v=vs.110).aspx

