---
title: "Comentários e regiões"
slug: "comentarios-e-regioes"
draft: false
images: []
weight: 9971
type: docs
toc: true
---

## Comentários
Usar comentários em seus projetos é uma maneira prática de deixar explicações sobre suas escolhas de design e deve ter como objetivo facilitar sua vida (ou de outra pessoa) ao manter ou adicionar ao código.

Existem duas maneiras de adicionar um comentário ao seu código.

# Comentários de linha única

Qualquer texto colocado após `//` será tratado como um comentário.

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

# Comentários de várias linhas ou delimitados

Qualquer texto entre `/*` e `*/` será tratado como um comentário.

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

## Regiões
Uma região é um bloco de código recolhível, que pode ajudar na legibilidade e organização do seu código.

**OBSERVAÇÃO:** a regra SA1124 DoNotUseRegions do StyleCop desencoraja o uso de regiões. Eles geralmente são um sinal de código mal organizado, pois o C# inclui classes parciais e outros recursos que tornam as regiões obsoletas.

Você pode usar regiões da seguinte maneira:

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

Quando o código acima for visualizado em um IDE, você poderá recolher e expandir o código usando os símbolos + e -.

**Expandido**

[![O código acima no Visual Studio][1]][1]

**Desabou**

[![O código acima no Visual Studio foi recolhido usando regiões][2]][2]


[1]: http://i.stack.imgur.com/zYxwK.png
[2]: http://i.stack.imgur.com/T4rl5.png

## Comentários da documentação
Os comentários da documentação XML podem ser usados ​​para fornecer documentação da API que pode ser facilmente processada por ferramentas:

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

A documentação é coletada instantaneamente pelo IntelliSense:

[![IntelliSense exibindo a documentação do método][1]][1]


[1]: https://i.stack.imgur.com/cfvnh.png

