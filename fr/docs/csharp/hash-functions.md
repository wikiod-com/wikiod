---
title: "Fonctions de hachage"
slug: "fonctions-de-hachage"
draft: false
images: []
weight: 9959
type: docs
toc: true
---

MD5 et SHA1 ne sont pas sécurisés et doivent être évités. Les exemples existent à des fins éducatives et en raison du fait que les logiciels hérités peuvent encore utiliser ces algorithmes.

## MD5
Les fonctions de hachage mappent des chaînes binaires d'une longueur arbitraire à de petites chaînes binaires d'une longueur fixe.

L'algorithme [`MD5`][1] est une fonction de hachage largement utilisée produisant une valeur de hachage de 128 bits (16 octets, 32 caractères hexadécimaux).


La méthode [`ComputeHash`][2] de la classe [`System.Security.Cryptography.MD5`][3] renvoie le hachage sous la forme d'un tableau de 16 octets.

---

**Exemple:**

    using System;
    using System.Security.Cryptography;
    using System.Text;
    
    internal class Program
    {
        private static void Main()
        {
            var source = "Hello World!";
    
            // Creates an instance of the default implementation of the MD5 hash algorithm.
            using (var md5Hash = MD5.Create())
            {
                // Byte array representation of source string
                var sourceBytes = Encoding.UTF8.GetBytes(source);
    
                // Generate hash value(Byte Array) for input data
                var hashBytes = md5Hash.ComputeHash(sourceBytes);
    
                // Convert hash byte array to string
                var hash = BitConverter.ToString(hashBytes).Replace("-", string.Empty);
    
                // Output the MD5 hash
                Console.WriteLine("The MD5 hash of " + source + " is: " + hash);
            }
        }
    }
    
> **Sortie :** Le hachage MD5 de Hello World ! est : ED076287532E86365E841E92BFC50D8C

---

***[Problèmes de sécurité :][4]***

Comme la plupart des fonctions de hachage, MD5 n'est ni cryptage ni codage. Il peut être inversé par une attaque par force brute et souffre de vulnérabilités étendues contre les attaques par collision et préimage.


[1] : https://en.wikipedia.org/wiki/MD5
[2] : https://msdn.microsoft.com/en-us/library/s02tk69a(v=vs.110).aspx
[3] : https://msdn.microsoft.com/en-us/library/system.security.cryptography.md5(v=vs.110).aspx
[4] : https://en.wikipedia.org/wiki/MD5#Security

## SHA1
    using System;
    using System.Security.Cryptography;
    using System.Text;
    
    namespace ConsoleApplication1
    {
        class Program
        {
            static void Main(string[] args)
            {
                string source = "Hello World!";
                using (SHA1 sha1Hash = SHA1.Create())
                {
                    //From String to byte array
                    byte[] sourceBytes = Encoding.UTF8.GetBytes(source);
                    byte[] hashBytes = sha1Hash.ComputeHash(sourceBytes);
                    string hash = BitConverter.ToString(hashBytes).Replace("-",String.Empty);
    
                    Console.WriteLine("The SHA1 hash of " + source + " is: " + hash);     
                }
            } 
        }
     }

**Production:**

Le hachage SHA1 de Hello Word! est : 2EF7BDE608CE5404E97D5F042F95F89F1C232871

## SHA256
    using System;
    using System.Security.Cryptography;
    using System.Text;
    
    namespace ConsoleApplication1
    {
        class Program
        {
            static void Main(string[] args)
            {
                string source = "Hello World!";
                using (SHA256 sha256Hash = SHA256.Create())
                {
                    //From String to byte array
                    byte[] sourceBytes = Encoding.UTF8.GetBytes(source);
                    byte[] hashBytes = sha256Hash.ComputeHash(sourceBytes);
                    string hash = BitConverter.ToString(hashBytes).Replace("-", String.Empty);
    
                    Console.WriteLine("The SHA256 hash of " + source + " is: " + hash);
                }
            }
        }
    }

**Production:**

Le hachage SHA256 de Hello World! est : 7F83B1657FF1FC53B92DC18148A1D65DFC2D4B1FA3D677284ADDD200126D9069

## SHA384
    using System;
    using System.Security.Cryptography;
    using System.Text;
    
    namespace ConsoleApplication1
    {
        class Program
        {
            static void Main(string[] args)
            {
                string source = "Hello World!";
                using (SHA384 sha384Hash = SHA384.Create())
                {
                    //From String to byte array
                    byte[] sourceBytes = Encoding.UTF8.GetBytes(source);
                    byte[] hashBytes = sha384Hash.ComputeHash(sourceBytes);
                    string hash = BitConverter.ToString(hashBytes).Replace("-", String.Empty);
    
                    Console.WriteLine("The SHA384 hash of " + source + " is: " + hash);
                }
            }
        }
    }

**Production:**

Le hachage SHA384 de Hello World! est : BFD76C0EBBD006FEE583410547C1887B0292BE76D582D96C242D2A792723E3FD6FD061F9D5CFD13B8F961358E6ADBA4A

## SHA512
    using System;
    using System.Security.Cryptography;
    using System.Text;
    
    namespace ConsoleApplication1
    {
        class Program
        {
            static void Main(string[] args)
            {
                string source = "Hello World!";
                using (SHA512 sha512Hash = SHA512.Create())
                {
                    //From String to byte array
                    byte[] sourceBytes = Encoding.UTF8.GetBytes(source);
                    byte[] hashBytes = sha512Hash.ComputeHash(sourceBytes);
                    string hash = BitConverter.ToString(hashBytes).Replace("-", String.Empty);
    
                    Console.WriteLine("The SHA512 hash of " + source + " is: " + hash);
                }
            }
        }
    }

**Production:**
Le hachage SHA512 de Hello World! est : 861844D6704E8573FEC34D967E20BCFEF3D424CF48BE04E6DC08F2BD58C729743371015EAD891CC3CF1C9D34B49264B510751B1FF9E537937BC46B5D6FF4ECC8

## PBKDF2 pour le hachage de mot de passe
**PBKDF2** ("Password-Based Key Derivation Function 2") est l'une des fonctions de hachage recommandées pour le hachage de mot de passe. Il fait partie de [rfc-2898][1].

La classe `Rfc2898DeriveBytes` de .NET est basée sur HMACSHA1.


        using System.Security.Cryptography;

        ...

        public const int SALT_SIZE = 24; // size in bytes
        public const int HASH_SIZE = 24; // size in bytes
        public const int ITERATIONS = 100000; // number of pbkdf2 iterations

        public static byte[] CreateHash(string input)
        {
            // Generate a salt
            RNGCryptoServiceProvider provider = new RNGCryptoServiceProvider();
            byte[] salt = new byte[SALT_SIZE];
            provider.GetBytes(salt);

            // Generate the hash
            Rfc2898DeriveBytes pbkdf2 = new Rfc2898DeriveBytes(input, salt, ITERATIONS);
            return pbkdf2.GetBytes(HASH_SIZE);
        }


PBKDF2 nécessite un [sel][2] et le nombre d'itérations.

**Itérations :**

Un nombre élevé d'itérations ralentira l'algorithme, ce qui rendra le craquage de mot de passe beaucoup plus difficile. Un nombre élevé d'itérations est donc recommandé. PBKDF2 est un ordre de grandeur plus lent que MD5 par exemple.

**Le sel:**

Un sel empêchera la recherche de valeurs de hachage dans [les tables arc-en-ciel.] [3] Il doit être stocké à côté du hachage du mot de passe. Un sel par mot de passe (pas un sel global) est recommandé.


[1] : https://tools.ietf.org/html/rfc2898#section-5.2
[2] : https://en.wikipedia.org/wiki/Salt_(cryptographie)
[3] : https://en.wikipedia.org/wiki/Rainbow_table

## Solution complète de hachage de mot de passe à l'aide de Pbkdf2
    using System;
    using System.Linq;
    using System.Security.Cryptography;

    namespace YourCryptoNamespace
    {
        /// <summary>
        /// Salted password hashing with PBKDF2-SHA1.
      /// Compatibility: .NET 3.0 and later.
      /// </summary>
      /// <remarks>See http://crackstation.net/hashing-security.htm for much more on password hashing.</remarks>
      public static class PasswordHashProvider
      {
        /// <summary>
        /// The salt byte size, 64 length ensures safety but could be increased / decreased
        /// </summary>
        private const int SaltByteSize = 64;
        /// <summary>
        /// The hash byte size, 
        /// </summary>
        private const int HashByteSize = 64;
        /// <summary>
        ///  High iteration count is less likely to be cracked
        /// </summary>
        private const int Pbkdf2Iterations = 10000;

        /// <summary>
        /// Creates a salted PBKDF2 hash of the password.
        /// </summary>
        /// <remarks>
        /// The salt and the hash have to be persisted side by side for the password. They could be persisted as bytes or as a string using the convenience methods in the next class to convert from byte[] to string and later back again when executing password validation.
        /// </remarks>
        /// <param name="password">The password to hash.</param>
        /// <returns>The hash of the password.</returns>
        public static PasswordHashContainer CreateHash(string password)
        {
          // Generate a random salt
          using (var csprng = new RNGCryptoServiceProvider())
          {
            // create a unique salt for every password hash to prevent rainbow and dictionary based attacks
            var salt = new byte[SaltByteSize];
            csprng.GetBytes(salt);

            // Hash the password and encode the parameters
            var hash = Pbkdf2(password, salt, Pbkdf2Iterations, HashByteSize);

            return new PasswordHashContainer(hash, salt);
          }
        }
        /// <summary>
        /// Recreates a password hash based on the incoming password string and the stored salt
        /// </summary>
        /// <param name="password">The password to check.</param>
        /// <param name="salt">The salt existing.</param>
        /// <returns>the generated hash based on the password and salt</returns>
        public static byte[] CreateHash(string password, byte[] salt)
        {
          // Extract the parameters from the hash
          return Pbkdf2(password, salt, Pbkdf2Iterations, HashByteSize);
        }

        /// <summary>
        /// Validates a password given a hash of the correct one.
        /// </summary>
        /// <param name="password">The password to check.</param>
        /// <param name="salt">The existing stored salt.</param>
        /// <param name="correctHash">The hash of the existing password.</param>
        /// <returns><c>true</c> if the password is correct. <c>false</c> otherwise. </returns>
        public static bool ValidatePassword(string password, byte[] salt, byte[] correctHash)
        {
          // Extract the parameters from the hash
          byte[] testHash = Pbkdf2(password, salt, Pbkdf2Iterations, HashByteSize);
          return CompareHashes(correctHash, testHash);
        }
        /// <summary>
        /// Compares two byte arrays (hashes)
        /// </summary>
        /// <param name="array1">The array1.</param>
        /// <param name="array2">The array2.</param>
        /// <returns><c>true</c> if they are the same, otherwise <c>false</c></returns>
        public static bool CompareHashes(byte[] array1, byte[] array2)
        {
          if (array1.Length != array2.Length) return false;
          return !array1.Where((t, i) => t != array2[i]).Any();
        }
      
        /// <summary>
        /// Computes the PBKDF2-SHA1 hash of a password.
        /// </summary>
        /// <param name="password">The password to hash.</param>
        /// <param name="salt">The salt.</param>
        /// <param name="iterations">The PBKDF2 iteration count.</param>
        /// <param name="outputBytes">The length of the hash to generate, in bytes.</param>
        /// <returns>A hash of the password.</returns>
        private static byte[] Pbkdf2(string password, byte[] salt, int iterations, int outputBytes)
        {
          using (var pbkdf2 = new Rfc2898DeriveBytes(password, salt))
          {
            pbkdf2.IterationCount = iterations;
            return pbkdf2.GetBytes(outputBytes);
          }
        }
      }

      /// <summary>
      /// Container for password hash and salt and iterations.
      /// </summary>
      public sealed class PasswordHashContainer
      {
        /// <summary>
        /// Gets the hashed password.
        /// </summary>
        public byte[] HashedPassword { get; private set; }
        /// <summary>
        /// Gets the salt.
        /// </summary>
        public byte[] Salt { get; private set; }

        /// <summary>
        /// Initializes a new instance of the <see cref="PasswordHashContainer" /> class.
        /// </summary>
        /// <param name="hashedPassword">The hashed password.</param>
        /// <param name="salt">The salt.</param>
        public PasswordHashContainer(byte[] hashedPassword, byte[] salt)
        {
          this.HashedPassword = hashedPassword;
          this.Salt = salt;
        }
      }

      /// <summary>
      /// Convenience methods for converting between hex strings and byte array.
      /// </summary>
      public static class ByteConverter
      {
        /// <summary>
        /// Converts the hex representation string to an array of bytes
        /// </summary>
        /// <param name="hexedString">The hexed string.</param>
        /// <returns></returns>
        public static byte[] GetHexBytes(string hexedString)
        {
          var bytes = new byte[hexedString.Length / 2];
          for (var i = 0; i < bytes.Length; i++)
          {
            var strPos = i * 2;
            var chars = hexedString.Substring(strPos, 2);
            bytes[i] = Convert.ToByte(chars, 16);
          }
          return bytes;
        }
        /// <summary>
        /// Gets a hex string representation of the byte array passed in.
        /// </summary>
        /// <param name="bytes">The bytes.</param>
        public static string GetHexString(byte[] bytes)
        {
          return BitConverter.ToString(bytes).Replace("-", "").ToUpper();
        }
      }
    }

    /* 
     * Password Hashing With PBKDF2 (http://crackstation.net/hashing-security.htm).
     * Copyright (c) 2013, Taylor Hornby
     * All rights reserved.
     *
     * Redistribution and use in source and binary forms, with or without 
     * modification, are permitted provided that the following conditions are met:
     *
     * 1. Redistributions of source code must retain the above copyright notice, 
     * this list of conditions and the following disclaimer.
     *
     * 2. Redistributions in binary form must reproduce the above copyright notice,
     * this list of conditions and the following disclaimer in the documentation 
     * and/or other materials provided with the distribution.
     *
     * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" 
     * AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE 
     * IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE 
     * ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE 
     * LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR 
     * CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF 
     * SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS 
     * INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN 
     * CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) 
     * ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE 
     * POSSIBILITY OF SUCH DAMAGE.
     */

----

Veuillez consulter cette excellente ressource [Crackstation - Salted Password Hashing - Doing it Right] [1] pour plus d'informations. Une partie de cette solution (la fonction de hachage) était basée sur le code de ce site.

[1] : https://crackstation.net/hashing-security.htm

