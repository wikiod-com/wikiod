---
title: "Commentaires et régions"
slug: "commentaires-et-regions"
draft: false
images: []
weight: 9971
type: docs
toc: true
---

## Commentaires
L'utilisation de commentaires dans vos projets est un moyen pratique de laisser des explications sur vos choix de conception et devrait viser à vous faciliter la vie (ou celle de quelqu'un d'autre) lors de la maintenance ou de l'ajout au code.

Il existe deux façons d'ajouter un commentaire à votre code.

# Commentaires sur une seule ligne

Tout texte placé après `//` sera traité comme un commentaire.

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

# Commentaires multi-lignes ou délimités

Tout texte entre `/*` et `*/` sera traité comme un commentaire.

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

## Régions
Une région est un bloc de code réductible, qui peut aider à la lisibilité et à l'organisation de votre code.

**REMARQUE :** La règle SA1124 DoNotUseRegions de StyleCop décourage l'utilisation des régions. Ils sont généralement le signe d'un code mal organisé, car C # inclut des classes partielles et d'autres fonctionnalités qui rendent les régions obsolètes.

Vous pouvez utiliser les régions de la manière suivante :

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

Lorsque le code ci-dessus est affiché dans un IDE, vous pourrez réduire et développer le code à l'aide des symboles + et -.

**Étendu**

[![Le code ci-dessus dans Visual Studio][1]][1]

**S'est effondré**

[![Le code ci-dessus dans Visual Studio Collapsed using regions][2]][2]


[1] : http://i.stack.imgur.com/zYxwK.png
[2] : http://i.stack.imgur.com/T4rl5.png

## Commentaires sur la documentation
Les commentaires de documentation XML peuvent être utilisés pour fournir une documentation API qui peut être facilement traitée par des outils :

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

La documentation est instantanément récupérée par IntelliSense :

[![IntelliSense affichant la documentation de la méthode][1]][1]


[1] : https://i.stack.imgur.com/cfvnh.png

