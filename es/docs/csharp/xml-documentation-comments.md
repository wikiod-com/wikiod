---
title: "Comentarios de documentación XML"
slug: "comentarios-de-documentacion-xml"
draft: false
images: []
weight: 9846
type: docs
toc: true
---

Algunas veces necesita **crear documentación de texto extendida** a partir de sus comentarios xml. Desafortunadamente, ***no hay una forma estándar para hacerlo***.

Pero hay algunos proyectos separados que puede usar para este caso:

- [Castillo de arena][1]
- [Documento][2]
- [Ndoc][1]
- [DocFX][4]


[1]: http://sandcastle.codeplex.com/
[2]: http://docu.jagregory.com/
[3]: http://ndoc.sourceforge.net/
[4]: https://dotnet.github.io/docfx/

## Anotación de método simple
Los comentarios de documentación se colocan directamente sobre el método o la clase que describen. Comienzan con tres barras diagonales `///` y permiten que la metainformación se almacene a través de XML.

    /// <summary>
    /// Bar method description
    /// </summary>
    public void Bar()
    { 
            
    }

Visual Studio y otras herramientas pueden usar la información dentro de las etiquetas para proporcionar servicios como IntelliSense:

[![Ejemplo de anotación xml de método][1]][1]


[1]: https://i.stack.imgur.com/NDAnP.png


Consulte también [la lista de etiquetas de documentación comunes de Microsoft] (https://msdn.microsoft.com/en-us/library/5ast78ax.aspx).

## Generación de XML a partir de comentarios de documentación
Para generar un archivo de documentación XML a partir de comentarios de documentación en el código, utilice la opción `/doc` con el compilador C# `csc.exe`.

En Visual Studio 2013/2015, en **Proyecto** -> **Propiedades** -> **Compilación** -> **Salida**, marque la casilla de verificación `Archivo de documentación XML`:

[![Archivo de documentación XML][1]][1]

Cuando construya el proyecto, el compilador producirá un archivo XML con un nombre correspondiente al nombre del proyecto (por ejemplo, `XMLDocumentation.dll` -> `XMLDocumentation.xml`).

Cuando use el ensamblado en otro proyecto, asegúrese de que el archivo XML esté en el mismo directorio que la DLL a la que se hace referencia.

Este ejemplo:

    /// <summary>
    /// Data class description
    /// </summary>
    public class DataClass
    {
        /// <summary>
        /// Name property description
        /// </summary>
        public string Name { get; set; }
    }


    /// <summary>
    /// Foo function
    /// </summary>
    public class Foo
    {
        /// <summary>
        /// This method returning some data
        /// </summary>
        /// <param name="id">Id parameter</param>
        /// <param name="time">Time parameter</param>
        /// <returns>Data will be returned</returns>
        public DataClass GetData(int id, DateTime time)
        {
            return new DataClass();
        }
    }


Produce este xml en la compilación:

    <?xml version="1.0"?>
    <doc>
        <assembly>
            <name>XMLDocumentation</name>
        </assembly>
        <members>
            <member name="T:XMLDocumentation.DataClass">
                <summary>
                Data class description
                </summary>
            </member>
            <member name="P:XMLDocumentation.DataClass.Name">
                <summary>
                Name property description
                </summary>
            </member>
            <member name="T:XMLDocumentation.Foo">
                <summary>
                Foo function
                </summary>
            </member>
            <member name="M:XMLDocumentation.Foo.GetData(System.Int32,System.DateTime)">
                <summary>
                This method returning some data
                </summary>
                <param name="id">Id parameter</param>
                <param name="time">Time parameter</param>
                <returns>Data will be returned</returns>
            </member>
        </members>
    </doc>

[1]: https://i.stack.imgur.com/tXXQy.png

## Comentario de documentación del método con param y elementos devueltos
    /// <summary>
    /// Returns the data for the specified ID and timestamp.
    /// </summary>
    /// <param name="id">The ID for which to get data. </param>
    /// <param name="time">The DateTime for which to get data. </param>
    /// <returns>A DataClass instance with the result. </returns>
    public DataClass GetData(int id, DateTime time)
    {
       // ...
    }

**IntelliSense** le muestra la descripción de cada parámetro:

[![comentario de parámetro][1]][1]

Sugerencia: si Intellisense no se muestra en Visual Studio, elimine el primer corchete o coma y luego escríbalo nuevamente.

[1]: https://i.stack.imgur.com/cH3OQ.png

## Comentarios sobre la documentación de la interfaz y la clase
    /// <summary>
    /// This interface can do Foo
    /// </summary>
    public interface ICanDoFoo
    {
        // ... 
    }

    /// <summary>
    /// This Bar class implements ICanDoFoo interface
    /// </summary>
    public class Bar : ICanDoFoo
    {
        // ...
    }

**Resultado**

Resumen de la interfaz

[![resumen de la interfaz][1]][1]

Resumen de clase

[![resumen de la clase][2]][2]

[1]: https://i.stack.imgur.com/ExpwI.png
[2]: https://i.stack.imgur.com/730eY.png

## Haciendo referencia a otra clase en la documentación
La etiqueta `<ver>` se puede usar para vincular a otra clase. Contiene el miembro `cref` que debe contener el nombre de la clase a la que se hará referencia. Visual Studio proporcionará Intellsense al escribir esta etiqueta y dichas referencias también se procesarán al cambiar el nombre de la clase a la que se hace referencia.

    /// <summary>
    /// You might also want to check out <see cref="SomeOtherClass"/>.
    /// </summary>
    public class SomeClass
    {
    }
En las ventanas emergentes de Visual Studio Intellisense, dichas referencias también se mostrarán coloreadas en el texto.

Para hacer referencia a una clase genérica, use algo similar a lo siguiente:

    /// <summary>
    /// An enhanced version of <see cref="List{T}"/>.
    /// </summary>
    public class SomeGenericClass<T>
    {
    }

