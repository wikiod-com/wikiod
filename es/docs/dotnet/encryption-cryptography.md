---
title: "Cifrado  Criptografía"
slug: "cifrado--criptografia"
draft: false
images: []
weight: 9957
type: docs
toc: true
---

.NET Framework proporciona la implementación de muchos algoritmos criptográficos. Incluyen básicamente algoritmos simétricos, algoritmos asimétricos y hashes.

## RijndaelGestionado
Espacio de nombres requerido: `System.Security.Cryptography`
    
    private class Encryption {
        
              private const string SecretKey = "topSecretKeyusedforEncryptions";
        
              private const string SecretIv = "secretVectorHere";
        
              public string Encrypt(string data) {
                return string.IsNullOrEmpty(data) ? data : Convert.ToBase64String(this.EncryptStringToBytesAes(data, this.GetCryptographyKey(), this.GetCryptographyIv()));
              }
        
              public string Decrypt(string data) {
                return string.IsNullOrEmpty(data) ? data : this.DecryptStringFromBytesAes(Convert.FromBase64String(data), this.GetCryptographyKey(), this.GetCryptographyIv());
              }
        
              private byte[] GetCryptographyKey() {
                return Encoding.ASCII.GetBytes(SecretKey.Replace('e', '!'));
              }
        
              private byte[] GetCryptographyIv() {
                return Encoding.ASCII.GetBytes(SecretIv.Replace('r', '!'));
              }
        
              private byte[] EncryptStringToBytesAes(string plainText, byte[] key, byte[] iv) {
                MemoryStream encrypt;
                RijndaelManaged aesAlg = null;
                try {
                  aesAlg = new RijndaelManaged {
                    Key = key,
                    IV = iv
                  };
                  var encryptor = aesAlg.CreateEncryptor(aesAlg.Key, aesAlg.IV);
                  encrypt = new MemoryStream();
                  using (var csEncrypt = new CryptoStream(encrypt, encryptor, CryptoStreamMode.Write)) {
                    using (var swEncrypt = new StreamWriter(csEncrypt)) {
                      swEncrypt.Write(plainText);
                    }
                  }
                } finally {
                  aesAlg?.Clear();
                }
                return encrypt.ToArray();
              }
        
              private string DecryptStringFromBytesAes(byte[] cipherText, byte[] key, byte[] iv) {
                RijndaelManaged aesAlg = null;
                string plaintext;
                try {
                  aesAlg = new RijndaelManaged {
                    Key = key,
                    IV = iv
                  };
                  var decryptor = aesAlg.CreateDecryptor(aesAlg.Key, aesAlg.IV);
                  using (var msDecrypt = new MemoryStream(cipherText)) {
                    using (var csDecrypt = new CryptoStream(msDecrypt, decryptor, CryptoStreamMode.Read)) {
                      using (var srDecrypt = new StreamReader(csDecrypt))
                        plaintext = srDecrypt.ReadToEnd();
                    }
                  }
                } finally {
                  aesAlg?.Clear();
                }
                return plaintext;
              }
            }

**Uso**

    var textToEncrypt = "hello World";
    
     var encrypted = new Encryption().Encrypt(textToEncrypt); //-> zBmW+FUxOvdbpOGm9Ss/vQ==
    
     var decrypted = new Encryption().Decrypt(encrypted); //-> hello World

**Nota:**

- Rijndael es el predecesor del algoritmo criptográfico simétrico estándar AES.

## Cifrar y descifrar datos usando AES (en C#)
    using System;
    using System.IO;
    using System.Security.Cryptography;

    namespace Aes_Example
    {
        class AesExample
        {
            public static void Main()
            {
                try
                {
                    string original = "Here is some data to encrypt!";

                    // Create a new instance of the Aes class.
                    // This generates a new key and initialization vector (IV).
                    using (Aes myAes = Aes.Create())
                    {
                        // Encrypt the string to an array of bytes.
                        byte[] encrypted = EncryptStringToBytes_Aes(original, 
                                                                    myAes.Key,
                                                                    myAes.IV);

                        // Decrypt the bytes to a string.
                        string roundtrip = DecryptStringFromBytes_Aes(encrypted, 
                                                                      myAes.Key, 
                                                                      myAes.IV);

                        //Display the original data and the decrypted data.
                        Console.WriteLine("Original:   {0}", original);
                        Console.WriteLine("Round Trip: {0}", roundtrip);
                    }
                }
                catch (Exception e)
                {
                    Console.WriteLine("Error: {0}", e.Message);
                }
            }

            static byte[] EncryptStringToBytes_Aes(string plainText, byte[] Key, byte[] IV)
            {
                // Check arguments.
                if (plainText == null || plainText.Length <= 0)
                    throw new ArgumentNullException("plainText");
                if (Key == null || Key.Length <= 0)
                    throw new ArgumentNullException("Key");
                if (IV == null || IV.Length <= 0)
                    throw new ArgumentNullException("IV");

                byte[] encrypted;

                // Create an Aes object with the specified key and IV.
                using (Aes aesAlg = Aes.Create())
                {
                    aesAlg.Key = Key;
                    aesAlg.IV = IV;

                    // Create a decrytor to perform the stream transform.
                    ICryptoTransform encryptor = aesAlg.CreateEncryptor(aesAlg.Key,
                                                                        aesAlg.IV);

                    // Create the streams used for encryption.
                    using (MemoryStream msEncrypt = new MemoryStream())
                    {
                        using (CryptoStream csEncrypt = new CryptoStream(msEncrypt,
                                                                         encryptor,
                                                                         CryptoStreamMode.Write))
                        {
                            using (StreamWriter swEncrypt = new StreamWriter(csEncrypt))
                            {
                                //Write all data to the stream.
                                swEncrypt.Write(plainText);
                            }

                            encrypted = msEncrypt.ToArray();
                        }
                    }
                }

                // Return the encrypted bytes from the memory stream.
                return encrypted;
            }

            static string DecryptStringFromBytes_Aes(byte[] cipherText, byte[] Key, byte[] IV)
            {
                // Check arguments.
                if (cipherText == null || cipherText.Length <= 0)
                    throw new ArgumentNullException("cipherText");
                if (Key == null || Key.Length <= 0)
                    throw new ArgumentNullException("Key");
                if (IV == null || IV.Length <= 0)
                    throw new ArgumentNullException("IV");

                // Declare the string used to hold the decrypted text.
                string plaintext = null;

                // Create an Aes object with the specified key and IV.
                using (Aes aesAlg = Aes.Create())
                {
                    aesAlg.Key = Key;
                    aesAlg.IV = IV;

                    // Create a decrytor to perform the stream transform.
                    ICryptoTransform decryptor = aesAlg.CreateDecryptor(aesAlg.Key,
                                                                        aesAlg.IV);

                    // Create the streams used for decryption.
                    using (MemoryStream msDecrypt = new MemoryStream(cipherText))
                    {
                        using (CryptoStream csDecrypt = new CryptoStream(msDecrypt,
                                                                         decryptor,
                                                                         CryptoStreamMode.Read))
                        {
                            using (StreamReader srDecrypt = new StreamReader(csDecrypt))
                            {

                                // Read the decrypted bytes from the decrypting stream
                                // and place them in a string.
                                plaintext = srDecrypt.ReadToEnd();
                            }
                        }
                    }
                }

                return plaintext;
            }
        }
    }

Este ejemplo es de [MSDN](https://msdn.microsoft.com/en-us/library/system.security.cryptography.aes(v=vs.110).aspx).

Es una aplicación de demostración de consola que muestra cómo cifrar una cadena mediante el cifrado **AES** estándar y cómo descifrarla después.

(**[AES = Estándar de cifrado avanzado](https://en.wikipedia.org/wiki/Advanced_Encryption_Standard),** una especificación para el cifrado de datos electrónicos establecida por el Instituto Nacional de Estándares y Tecnología de EE. UU. (NIST) en 2001, que sigue siendo el estándar de facto para el cifrado simétrico)

**Notas:**

- En un escenario de cifrado real, debe elegir un modo de cifrado adecuado (se puede asignar a la propiedad `Modo` seleccionando un valor de la enumeración `CipherMode`). **Nunca** use `CipherMode.ECB` (modo de libro de códigos electrónico), ya que esto produce un flujo de cifrado débil

- Para crear una "clave" buena (y no débil), utilice un generador aleatorio criptográfico o utilice el ejemplo anterior (**Crear una clave a partir de una contraseña**). El **Tamaño de clave** recomendado es de 256 bits. Los tamaños de clave admitidos están disponibles a través de la propiedad `LegalKeySizes`.

- Para inicializar el vector de inicialización `IV`, puede usar un SALT como se muestra en el ejemplo anterior (**SALT aleatorio**)

- Los tamaños de bloque admitidos están disponibles a través de la propiedad `SupportedBlockSizes`, el tamaño de bloque se puede asignar a través de la propiedad `BlockSize`

**Uso:** ver el método Main().


## Crear una clave a partir de una contraseña / SAL aleatoria (en C#)
    using System;
    using System.Security.Cryptography;
    using System.Text;

    public class PasswordDerivedBytesExample
    {
        public static void Main(String[] args)
        {
            // Get a password from the user.
            Console.WriteLine("Enter a password to produce a key:");

            byte[] pwd = Encoding.Unicode.GetBytes(Console.ReadLine());

            byte[] salt = CreateRandomSalt(7);

            // Create a TripleDESCryptoServiceProvider object.
            TripleDESCryptoServiceProvider tdes = new TripleDESCryptoServiceProvider();

            try
            {
                Console.WriteLine("Creating a key with PasswordDeriveBytes...");

                // Create a PasswordDeriveBytes object and then create
                // a TripleDES key from the password and salt.
                PasswordDeriveBytes pdb = new PasswordDeriveBytes(pwd, salt);

                // Create the key and set it to the Key property
                // of the TripleDESCryptoServiceProvider object.
                tdes.Key = pdb.CryptDeriveKey("TripleDES", "SHA1", 192, tdes.IV);

                Console.WriteLine("Operation complete.");
            }
            catch (Exception e)
            {
                Console.WriteLine(e.Message);
            }
            finally
            {
                // Clear the buffers
                ClearBytes(pwd);
                ClearBytes(salt);

                // Clear the key.
                tdes.Clear();
            }

            Console.ReadLine();
        }

        #region Helper methods

        /// <summary>
        /// Generates a random salt value of the specified length.
        /// </summary>
        public static byte[] CreateRandomSalt(int length)
        {
            // Create a buffer
            byte[] randBytes;

            if (length >= 1)
            {
                randBytes = new byte[length];
            }
            else
            {
                randBytes = new byte[1];
            }

            // Create a new RNGCryptoServiceProvider.
            RNGCryptoServiceProvider rand = new RNGCryptoServiceProvider();

            // Fill the buffer with random bytes.
            rand.GetBytes(randBytes);

            // return the bytes.
            return randBytes;
        }

        /// <summary>
        /// Clear the bytes in a buffer so they can't later be read from memory.
        /// </summary>
        public static void ClearBytes(byte[] buffer)
        {
            // Check arguments.
            if (buffer == null)
            {
                throw new ArgumentNullException("buffer");
            }

            // Set each byte in the buffer to 0.
            for (int x = 0; x < buffer.Length; x++)
            {
                buffer[x] = 0;
            }
        }

        #endregion
    }

Este ejemplo está tomado de [MSDN.](https://msdn.microsoft.com/en-us/library/system.security.cryptography.passwordderivebytes(v=vs.110).aspx)

Es una demostración de consola y muestra cómo crear una clave segura basada en una contraseña definida por el usuario y cómo crear un SALT aleatorio basado en el generador aleatorio criptográfico.

**Notas:**

- La función integrada `PasswordDeriveBytes` utiliza el algoritmo estándar PBKDF1 para generar una clave a partir de la contraseña. Por defecto, utiliza 100 iteraciones para generar la clave para ralentizar los ataques de fuerza bruta. El SALT generado aleatoriamente fortalece aún más la clave.

- La función `CryptDeriveKey` convierte la clave generada por `PasswordDeriveBytes` en una clave compatible con el algoritmo de cifrado especificado (aquí "TripleDES") mediante el uso del algoritmo hash especificado (aquí "SHA1"). El tamaño de clave en este ejemplo es de 192 bytes, y el vector de inicialización IV se toma del proveedor de cifrado triple-DES

- Por lo general, este mecanismo se utiliza para proteger una clave generada aleatoriamente más fuerte mediante una contraseña, que encripta una gran cantidad de datos. También puede usarlo para proporcionar múltiples contraseñas de diferentes usuarios para dar acceso a los mismos datos (estando protegido por una clave aleatoria diferente).

- Desafortunadamente, `CryptDeriveKey` actualmente no es compatible con AES. Consulte [aquí.](https://social.msdn.microsoft.com/Forums/vstudio/en-US/61d85001-2eae-4419-b4bf-ce98d46f4d21/passwordderivebytescryptderivekey-derives-an-aes-key-but-gets- object-identifier-oid-is-unknown?forum=netfxbcl) <br/> **NOTA:** Como solución alternativa, puede crear una clave AES aleatoria para el cifrado de los datos que se protegerán con AES y almacenar la clave AES en un contenedor TripleDES que utiliza la clave generada por `CryptDeriveKey`. Pero eso limita la seguridad a TripleDES, no aprovecha los tamaños de clave más grandes de AES y crea una dependencia a TripleDES.

**Uso:** Consulte el método Main().

## Cifrado y descifrado mediante criptografía (AES)
Código de descifrado

        public static string Decrypt(string cipherText)
        {
            if (cipherText == null)
                return null;

            byte[] cipherBytes = Convert.FromBase64String(cipherText);
            using (Aes encryptor = Aes.Create())
            {
                Rfc2898DeriveBytes pdb = new Rfc2898DeriveBytes(CryptKey, new byte[] { 0x49, 0x76, 0x61, 0x6e, 0x20, 0x4d, 0x65, 0x64, 0x76, 0x65, 0x64, 0x65, 0x76 });
                encryptor.Key = pdb.GetBytes(32);
                encryptor.IV = pdb.GetBytes(16);

                using (MemoryStream ms = new MemoryStream())
                {
                    using (CryptoStream cs = new CryptoStream(ms, encryptor.CreateDecryptor(), CryptoStreamMode.Write))
                    {
                        cs.Write(cipherBytes, 0, cipherBytes.Length);
                        cs.Close();
                    }

                    cipherText = Encoding.Unicode.GetString(ms.ToArray());
                }
            }
        
            return cipherText;
        }
Código de cifrado

        public static string Encrypt(string cipherText)
        {
            if (cipherText == null)
                return null;
          
            byte[] clearBytes = Encoding.Unicode.GetBytes(cipherText);
            using (Aes encryptor = Aes.Create())
            {
                Rfc2898DeriveBytes pdb = new Rfc2898DeriveBytes(CryptKey, new byte[] { 0x49, 0x76, 0x61, 0x6e, 0x20, 0x4d, 0x65, 0x64, 0x76, 0x65, 0x64, 0x65, 0x76 });
                encryptor.Key = pdb.GetBytes(32);
                encryptor.IV = pdb.GetBytes(16);

                using (MemoryStream ms = new MemoryStream())
                {
                    using (CryptoStream cs = new CryptoStream(ms, encryptor.CreateEncryptor(), CryptoStreamMode.Write))
                    {
                        cs.Write(clearBytes, 0, clearBytes.Length);
                        cs.Close();
                    }

                    cipherText = Convert.ToBase64String(ms.ToArray());
                }
            }
            return cipherText;
        }

Uso

   

    var textToEncrypt = "TestEncrypt";
    
    var encrypted = Encrypt(textToEncrypt);
    
    var decrypted = Decrypt(encrypted);

