---
title: "Convenciones de nombres"
slug: "convenciones-de-nombres"
draft: false
images: []
weight: 9868
type: docs
toc: true
---

Este tema describe algunas convenciones de nomenclatura básicas que se usan al escribir en el lenguaje C#. Como todas las convenciones, el compilador no las hace cumplir, pero garantizará la legibilidad entre los desarrolladores.

Para conocer las pautas integrales de diseño de .NET Framework, consulte [docs.microsoft.com/dotnet/standard/design-guidelines](https://docs.microsoft.com/dotnet/standard/design-guidelines/).

## Elija nombres de identificadores fáciles de leer
Por ejemplo, una propiedad denominada HorizontalAlignment es más legible en inglés que AlignmentHorizontal.

## Prefiere la legibilidad a la brevedad
El nombre de propiedad `CanScrollHorizontally` es mejor que `ScrollableX` (una oscura referencia al eje X).

Evite el uso de guiones bajos, guiones o cualquier otro carácter no alfanumérico.

## **No** utilice notación húngara
La notación húngara es la práctica de incluir un prefijo en los identificadores para codificar algunos metadatos sobre el parámetro, como el tipo de datos del identificador, p. `string strNombre`.

Además, evite usar identificadores que entren en conflicto con palabras clave que ya se usan en C#.

## Abreviaciones y acronimos
En general, no debe utilizar abreviaturas o siglas; estos hacen que sus nombres sean menos legibles. De manera similar, es difícil saber cuándo es seguro asumir que un acrónimo es ampliamente reconocido.

## Convenciones de mayúsculas
Los siguientes términos describen diferentes formas de identificar casos.
## Caja Pascal
La primera letra del identificador y la primera letra de cada palabra concatenada subsiguiente están en mayúscula. Puede usar el caso de Pascal para identificadores de tres o más caracteres. Por ejemplo: `BackColor`

## Carcasa de camello
La primera letra de un identificador está en minúscula y la primera letra de cada palabra concatenada posterior está en mayúscula. Por ejemplo: `backColor`

## Mayúsculas
Todas las letras del identificador están en mayúscula. Por ejemplo: `IO`

---

## Normas
Cuando un identificador consta de varias palabras, no utilice separadores, como guiones bajos ("_") o guiones ("-"), entre palabras. En su lugar, utilice mayúsculas y minúsculas para indicar el comienzo de cada palabra.

La siguiente tabla resume las reglas de uso de mayúsculas para los identificadores y proporciona ejemplos para los diferentes tipos de identificadores:

Identificador | Caso | Ejemplo
------------------------------------- | ------ | -------
Variable local | Camello | nombre del coche
Clase | Pascual | Dominio de la aplicación
Tipo de enumeración | Pascual | Nivel de error
Valores de enumeración | Pascual | Error fatal
Evento | Pascual | ValorCambiado
Clase de excepción | Pascual | WebException
Campo estático de solo lectura | Pascual | valor rojo
Interfaz | Pascual | Desechable
Método | Pascual | Encadenar
Espacio de nombres | Pascual | Sistema.Dibujo
Parámetro | Camello | escribe un nombre
Propiedad | Pascual | BackColor

Se puede encontrar más información en [MSDN][1].


[1]: https://msdn.microsoft.com/library/ms229043(v=vs.110).aspx

## Enumeraciones
## Use un nombre singular para la mayoría de las enumeraciones

    public enum Volume
    {
       Low,
       Medium,
       High
    }

## Use un nombre plural para los tipos Enum que son campos de bits

    [Flags]
    public enum MyColors
    {
        Yellow = 1,
        Green = 2,
        Red = 4,
        Blue = 8
    }
*Nota: siempre agregue [`FlagsAttribute`][1] a un tipo de enumeración de campo de bit.*

## **No** agregue 'enumeración' como sufijo

    public enum VolumeEnum // Incorrect

## **No** use el nombre de enumeración en cada entrada

    public enum Color
    {
        ColorBlue, // Remove Color, unnecessary
        ColorGreen,
    }


[1]: https://msdn.microsoft.com/en-us/library/system.flagsattribute(v=vs.110).aspx

## Interfaces
Las interfaces deben nombrarse con sustantivos o frases nominales, o adjetivos que describan el comportamiento. Por ejemplo, `IComponent` usa un sustantivo descriptivo, `ICustomAttributeProvider` usa una frase nominal y `IPersistable` usa un adjetivo.

Los nombres de las interfaces deben tener como prefijo la letra `I`, para indicar que el tipo es una interfaz, y se debe usar el caso de Pascal.

A continuación se muestran las interfaces correctamente nombradas:

    public interface IServiceProvider
    public interface IFormatable

## Excepciones
## Añadir 'excepción' como sufijo
Los nombres de excepción personalizados deben tener el sufijo "-Exception".

A continuación se muestran las excepciones correctamente nombradas:

    public class MyCustomException : Exception
    public class FooException : Exception

## Campos privados
Hay dos convenciones comunes para los campos privados: `camelCase` y `_camelCaseWithLeadingUnderscore`.

## El caso de Carmel

    public class Rational
    {
        private readonly int numerator;
        private readonly int denominator;

        public Rational(int numerator, int denominator)
        {
            // "this" keyword is required to refer to the class-scope field
            this.numerator = numerator;
            this.denominator = denominator;
        }
    }

## Caso camello con guión bajo

    public class Rational
    {
        private readonly int _numerator;
        private readonly int _denominator;

        public Rational(int numerator, int denominator)
        {
            // Names are unique, so "this" keyword is not required
            _numerator = numerator;
            _denominator = denominator;
        }
    }

## Espacios de nombres
El formato general para los espacios de nombres es:

    <Company>.(<Product>|<Technology>)[.<Feature>][.<Subnamespace>].

Ejemplos incluyen:

    Fabrikam.Math
    Litware.Security

El prefijo de nombres de espacios de nombres con el nombre de una empresa evita que los espacios de nombres de diferentes empresas tengan el mismo nombre.



