---
title: "Comentarios y regiones"
slug: "comentarios-y-regiones"
draft: false
images: []
weight: 9971
type: docs
toc: true
---

## Comentarios
El uso de comentarios en sus proyectos es una forma práctica de dejar explicaciones de sus opciones de diseño, y debe tener como objetivo hacer su vida (o la de otra persona) más fácil al mantener o agregar código.

Hay dos formas de agregar un comentario a su código.

# Comentarios de una sola línea

Cualquier texto colocado después de `//` será tratado como un comentario.

    public class Program
    {
        // This is the entry point of my program.
        public static void Main()
        {
            // Prints a message to the console. - This is a comment!
            System.Console.WriteLine("Hello, World!"); 

            // System.Console.WriteLine("Hello, World again!"); // You can even comment out code.
            System.Console.ReadLine();
        }
    }

# Comentarios de varias líneas o delimitados

Cualquier texto entre `/*` y `*/` será tratado como un comentario.

    public class Program
    {
        public static void Main()
        {
            /*
                This is a multi line comment
                it will be ignored by the compiler.
            */
            System.Console.WriteLine("Hello, World!");

            // It's also possible to make an inline comment with /* */
            // although it's rarely used in practice
            System.Console.WriteLine(/* Inline comment */ "Hello, World!");
      
            System.Console.ReadLine();
        }
    }

## Regiones
Una región es un bloque de código contraíble que puede ayudar con la legibilidad y la organización de su código.

**NOTA:** La regla SA1124 DoNotUseRegions de StyleCop desaconseja el uso de regiones. Por lo general, son un signo de código mal organizado, ya que C# incluye clases parciales y otras características que hacen que las regiones queden obsoletas.

Puede usar regiones de la siguiente manera:

    class Program
    {
        #region Application entry point
        static void Main(string[] args)
        {
            PrintHelloWorld();
            System.Console.ReadLine();
        }
        #endregion

        #region My method
        private static void PrintHelloWorld()
        {
            System.Console.WriteLine("Hello, World!");
        }
        #endregion
    }

Cuando el código anterior se vea en un IDE, podrá colapsar y expandir el código usando los símbolos + y -.

**Expandido**

[![El código anterior en Visual Studio][1]][1]

**Colapsado**

[![El código anterior en Visual Studio colapsado usando regiones][2]][2]


[1]: http://i.stack.imgur.com/zYxwK.png
[2]: http://i.stack.imgur.com/T4rl5.png

## Comentarios de la documentación
Los comentarios de la documentación XML se pueden utilizar para proporcionar documentación API que las herramientas pueden procesar fácilmente:

    /// <summary>
    /// A helper class for validating method arguments.
    /// </summary>
    public static class Precondition
    {
        /// <summary>
        ///     Throws an <see cref="ArgumentOutOfRangeException"/> with the parameter
        ///     name set to <c>paramName</c> if <c>value</c> does not satisfy the 
        ///     <c>predicate</c> specified.
        /// </summary>
        /// <typeparam name="T">
        ///     The type of the argument checked
        /// </typeparam>
        /// <param name="value">
        ///     The argument to be checked
        /// </param>
        /// <param name="predicate">
        ///     The predicate the value is required to satisfy
        /// </param>
        /// <param name="paramName">
        ///     The parameter name to be passed to the
        ///     <see cref="ArgumentOutOfRangeException"/>.
        /// </param>
        /// <returns>The value specified</returns>
        public static T Satisfies<T>(T value, Func<T, bool> predicate, string paramName)
        {
            if (!predicate(value))
                throw new ArgumentOutOfRangeException(paramName);

            return value;
        }
    }

IntelliSense recoge instantáneamente la documentación:

[![Documentación del método de visualización de IntelliSense][1]][1]


[1]: https://i.stack.imgur.com/cfvnh.png

