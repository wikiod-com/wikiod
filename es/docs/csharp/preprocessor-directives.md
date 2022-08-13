---
title: "Directivas del pre procesador"
slug: "directivas-del-pre-procesador"
draft: false
images: []
weight: 9933
type: docs
toc: true
---

## Sintaxis
- #define *[símbolo]* // Define un símbolo del compilador.
- #undef *[símbolo]* // Anula la definición de un símbolo del compilador.
- #warning *[mensaje de advertencia]* // Genera una advertencia del compilador. Útil con #if.
- #error *[mensaje de error]* // Genera un error de compilación. Útil con #if.
- #line *[line number] (nombre de archivo)* // Anula el número de línea del compilador (y, opcionalmente, el nombre del archivo de origen). Se usa con [plantillas de texto T4] (https://msdn.microsoft.com/en-us/library/bb126445.aspx).
- #pragma advertencia [deshabilitar|restaurar] *[números de advertencia]* // Deshabilita/restaura las advertencias del compilador.
- #pragma checksum "*[filename]*" "*[guid]*" "*[checksum]*" // Valida el contenido de un archivo fuente.
- #region *[nombre de la región]* // Define una región de código contraíble.
- #endregion // Finaliza un bloque de región de código.
- #if *[condición]* // Ejecuta el siguiente código si la condición es verdadera.
- #else // Usado después de un #if.
- #elif *[condición]* // Usado después de un #if.
- #endif // Finaliza un bloque condicional iniciado con #if.

Las directivas de preprocesador se utilizan normalmente para hacer que los programas fuente sean fáciles de cambiar y compilar en diferentes entornos de ejecución. Las directivas en el archivo fuente le dicen al preprocesador que realice acciones específicas. Por ejemplo, el preprocesador puede reemplazar tokens en el texto, insertar el contenido de otros archivos en el archivo de origen o suprimir la compilación de parte del archivo mediante la eliminación de secciones de texto. Las líneas del preprocesador se reconocen y ejecutan antes de la macroexpansión. Por lo tanto, si una macro se expande en algo que parece un comando de preprocesador, el preprocesador no reconoce ese comando.
 
Las instrucciones del preprocesador utilizan el mismo conjunto de caracteres que las instrucciones del archivo de origen, con la excepción de que no se admiten las secuencias de escape. El juego de caracteres utilizado en las sentencias del preprocesador es el mismo que el juego de caracteres de ejecución. El preprocesador también reconoce valores de caracteres negativos.

## Expresiones condicionales

Las expresiones condicionales (`#if`, `#elif`, etc.) admiten un subconjunto limitado de operadores booleanos. Están:

- `==` y `!=`. Estos solo se pueden usar para probar si el símbolo es verdadero (definido) o falso (no definido)
- `&&`, `||`, `!`
- `()`

Por ejemplo:

    #if !DEBUG && (SOME_SYMBOL || SOME_OTHER_SYMBOL) && RELEASE == true
    Console.WriteLine("OK!");
    #endif

compilaría el código que imprime "¡OK!" a la consola si `DEBUG` no está definido, ya sea `SOME_SYMBOL` o `SOME_OTHER_SYMBOL` está definido, y `RELEASE` está definido.

Nota: estas sustituciones se realizan _en tiempo de compilación_ y, por lo tanto, no están disponibles para su inspección en tiempo de ejecución. El código eliminado mediante el uso de `#if` no forma parte de la salida del compilador.

Consulte también: [Directivas de preprocesador de C#](https://msdn.microsoft.com/en-us/library/ed8yd1ha.aspx) en MSDN.


## Expresiones condicionales
Cuando se compila lo siguiente, devolverá un valor diferente según las directivas definidas.

    // Compile with /d:A or /d:B to see the difference
    string SomeFunction() 
    {
    #if A
        return "A";
    #elif B
        return "B";
    #else
        return "C";
    #endif
    }

Las expresiones condicionales se utilizan normalmente para registrar información adicional para compilaciones de depuración.

    void SomeFunc()
    {
        try
        {
            SomeRiskyMethod();
        }
        catch (ArgumentException ex)
        {
            #if DEBUG
            log.Error("SomeFunc", ex);
            #endif

            HandleException(ex);
        }
    }



## Otras instrucciones del compilador
# Línea

`#line` controla el número de línea y el nombre de archivo informado por el compilador al generar advertencias y errores.

    void Test()
    {
        #line 42 "Answer"
        #line filename "SomeFile.cs"
        int life; // compiler warning CS0168 in "SomeFile.cs" at Line 42
        #line default
        // compiler warnings reset to default
    }

# Suma de comprobación de pragma

`#pragma checksum` permite la especificación de una suma de verificación específica para una base de datos de programa generada (PDB) para la depuración.

    #pragma checksum "MyCode.cs" "{00000000-0000-0000-0000-000000000000}" "{0123456789A}"

## Símbolos que definen y no definen
Un símbolo de compilador es una palabra clave que se define en tiempo de compilación y que se puede verificar para ejecutar condicionalmente secciones específicas de código.
 
Hay tres formas de definir un símbolo de compilador. Se pueden definir mediante código:
 
    #define MYSYMBOL
 
Se pueden definir en Visual Studio, en Propiedades del proyecto > Generar > Símbolos de compilación condicional:
 
![Símbolos del compilador VS](http://i.imgur.com/PHG04dI.png)
 
*(Tenga en cuenta que `DEBUG` y `TRACE` tienen sus propias casillas de verificación y no es necesario especificarlas explícitamente).*
 
O se pueden definir en tiempo de compilación usando el modificador `/define:[name]` en el compilador de C#, `csc.exe`.

También puedes usar símbolos indefinidos usando la directiva `#undefine`.
 
El ejemplo más frecuente de esto es el símbolo `DEBUG`, que Visual Studio define cuando una aplicación se compila en modo de depuración (frente al modo de lanzamiento).
 
    public void DoBusinessLogic()
    {
        try
        {
            AuthenticateUser();
            LoadAccount();
            ProcessAccount();
            FinalizeTransaction();
        }
        catch (Exception ex)
        {
    #if DEBUG
            System.Diagnostics.Trace.WriteLine("Unhandled exception!");
            System.Diagnostics.Trace.WriteLine(ex);
            throw;
    #else
            LoggingFramework.LogError(ex);
            DisplayFriendlyErrorMessage();
    #endif
        }
    }
 
En el ejemplo anterior, cuando se produce un error en la lógica empresarial de la aplicación, si la aplicación se compila en modo de depuración (y se establece el símbolo `DEBUG`), el error se escribirá en el registro de seguimiento y la excepción volver a lanzar para la depuración. Sin embargo, si la aplicación se compila en modo de lanzamiento (y no se establece ningún símbolo `DEBUG`), se utiliza un marco de registro para registrar silenciosamente el error y se muestra un mensaje de error amistoso al usuario final.

## Bloques de región
Use `#region` y `#endregion` para definir una región de código contraíble.
 
    #region Event Handlers
 
    public void Button_Click(object s, EventArgs e)
    {
        // ...
    }
 
    public void DropDown_SelectedIndexChanged(object s, EventArgs e)
    {
        // ...
    }
 
    #endregion
 
Estas directivas solo son beneficiosas cuando se usa un IDE que admite regiones contraíbles (como [Visual Studio](https://www.visualstudio.com/en-us/visual-studio-homepage-vs.aspx)) para editar el código.

## Deshabilitar y restaurar las advertencias del compilador
Puede deshabilitar las advertencias del compilador usando `#pragma warning disabled` y restaurarlas usando `#pragma warning restore`:
 
    #pragma warning disable CS0168
 
    // Will not generate the "unused variable" compiler warning since it was disabled
    var x = 5;
 
    #pragma warning restore CS0168
 
    // Will generate a compiler warning since the warning was just restored
    var y = 8;
 
Se permiten números de advertencia separados por comas:
 
    #pragma warning disable CS0168, CS0219
 
El prefijo 'CS' es opcional e incluso se puede mezclar (aunque esto no es una buena práctica):
 
    #pragma warning disable 0168, 0219, CS0414

## Generación de advertencias y errores del compilador
Las advertencias del compilador se pueden generar usando la directiva `#warning`, y los errores también se pueden generar usando la directiva `#error`.

<!-- idioma: lang-none -->

    #if SOME_SYMBOL
    #error This is a compiler Error.
    #elif SOME_OTHER_SYMBOL
    #warning This is a compiler Warning.
    #endif

## Usando el atributo Condicional
Agregar un atributo 'Condicional' del espacio de nombres 'System.Diagnostics' a un método es una forma clara de controlar qué métodos se llaman en sus compilaciones y cuáles no.

    #define EXAMPLE_A

    using System.Diagnostics;
    class Program
    {
        static void Main()
        {
            ExampleA(); // This method will be called
            ExampleB(); // This method will not be called
        }

        [Conditional("EXAMPLE_A")]
        static void ExampleA() {...}

        [Conditional("EXAMPLE_B")]
        static void ExampleB() {...}
    }

## Preprocesadores personalizados a nivel de proyecto
Es conveniente establecer un preprocesamiento condicional personalizado a nivel de proyecto cuando es necesario omitir algunas acciones, por ejemplo, para las pruebas.

Vaya a `Solution Explorer` -> Haga clic en <kbd>Ratón derecho</kbd> en el proyecto en el que desea establecer la variable -> `Properties` -> `Build` -> En general, busque el campo `Símbolos de compilación condicionales` e ingrese su variable condicional aquí

[![ingrese la descripción de la imagen aquí][1]][1]


Ejemplo de código que omitirá algo de código:

    public void Init()
    {
        #if !IGNOREREFRESHDB
        // will skip code here
         db.Initialize();
        #endif
    }

[1]: http://i.stack.imgur.com/B2pi1.png


