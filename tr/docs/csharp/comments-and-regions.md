---
title: "Yorumlar ve bölgeler"
slug: "yorumlar-ve-bolgeler"
draft: false
images: []
weight: 9971
type: docs
toc: true
---

## Yorumlar
Projelerinizde yorum kullanmak, tasarım seçimlerinize ilişkin açıklamalar bırakmanın kullanışlı bir yoludur ve kodu korurken veya koda eklerken sizin (veya başka birinin) hayatını kolaylaştırmayı amaçlamalıdır.

Kodunuza yorum eklemenin iki yolu vardır.

# Tek satırlık yorumlar

`//` den sonra yerleştirilen herhangi bir metin yorum olarak kabul edilecektir.

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

# Çok satırlı veya sınırlandırılmış yorumlar

`/*` ve `*/` arasındaki herhangi bir metin yorum olarak kabul edilecektir.

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

## Bölgeler
Bölge, kodunuzun okunabilirliğine ve düzenine yardımcı olabilecek, daraltılabilir bir kod bloğudur.

**NOT:** StyleCop'un SA1124 DoNotUseRegions kuralı, bölgelerin kullanılmasını önermez. C#, bölgeleri geçersiz kılan kısmi sınıfları ve diğer özellikleri içerdiğinden, genellikle kötü organize edilmiş kodun bir işaretidir.

Bölgeleri aşağıdaki şekilde kullanabilirsiniz:

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

Yukarıdaki kod bir IDE'de görüntülendiğinde, + ve - sembollerini kullanarak kodu daraltabilir ve genişletebilirsiniz.

**Genişletilmiş**

[![Visual Studio'daki yukarıdaki kod][1]][1]

**Daralmış**

[![Visual Studio'daki yukarıdaki kod, bölgeler kullanılarak daraltıldı][2]][2]


[1]: http://i.stack.imgur.com/zYxwK.png
[2]: http://i.stack.imgur.com/T4rl5.png

## Dokümantasyon yorumları
XML dokümantasyon yorumları, araçlar tarafından kolayca işlenebilen API dokümantasyonu sağlamak için kullanılabilir:

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

Belgeler anında IntelliSense tarafından alınır:

[![IntelliSense görüntüleme yöntemi belgeleri][1]][1]


[1]: https://i.stack.imgur.com/cfvnh.png

