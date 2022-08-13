---
title: "Criptografía (Sistema.Seguridad.Criptografía)"
slug: "criptografia-sistemaseguridadcriptografia"
draft: false
images: []
weight: 9908
type: docs
toc: true
---

## Ejemplos modernos de cifrado autenticado simétrico de una cadena
La criptografía es algo muy difícil y después de pasar mucho tiempo leyendo diferentes ejemplos y viendo lo fácil que es introducir algún tipo de vulnerabilidad, encontré una respuesta escrita originalmente por @jbtule que creo que es muy buena. Disfruta leyendo:

"La mejor práctica general para el cifrado simétrico es usar el cifrado autenticado con datos asociados (AEAD), sin embargo, esto no es parte de las bibliotecas criptográficas .net estándar. Entonces, el primer ejemplo usa [AES256] [2] y luego [HMAC256 ][3], un [Cifrar luego MAC][4] de dos pasos, que requiere más gastos generales y más claves.

El segundo ejemplo usa la práctica más simple de AES256-[GCM][1] usando el Bouncy Castle de código abierto (a través de nuget).

Ambos ejemplos tienen una función principal que toma la cadena de mensajes secretos, la(s) clave(s) y una carga útil no secreta opcional y devuelve una cadena cifrada autenticada opcionalmente antepuesta con los datos no secretos. Idealmente, los usaría con claves de 256 bits generadas aleatoriamente, vea `NewKey ()`.

Ambos ejemplos también tienen métodos auxiliares que usan una contraseña de cadena para generar las claves. Estos métodos de ayuda se proporcionan para que coincidan con otros ejemplos; sin embargo, son *mucho menos seguros* porque la seguridad de la contraseña será *mucho más débil que una clave de 256 bits*.

**Actualizar:**
Se agregaron sobrecargas `byte[]`, y solo [Gist](https://gist.github.com/4336842) tiene el formato completo con sangría de 4 espacios y documentos API debido a los límites de respuesta de StackOverflow".

----------
**.NET Cifrado incorporado (AES)-Entonces-MAC (HMAC) [[Gist]](https://gist.github.com/jbtule/4336842#file-aesthenhmac-cs)**

    /*
     * This work (Modern Encryption of a String C#, by James Tuley), 
     * identified by James Tuley, is free of known copyright restrictions.
     * https://gist.github.com/4336842
     * http://creativecommons.org/publicdomain/mark/1.0/ 
     */
    
    using System;
    using System.IO;
    using System.Security.Cryptography;
    using System.Text;
    
    namespace Encryption
    {
      public static class AESThenHMAC
      {
        private static readonly RandomNumberGenerator Random = RandomNumberGenerator.Create();
        
        //Preconfigured Encryption Parameters
        public static readonly int BlockBitSize = 128;
        public static readonly int KeyBitSize = 256;
    
        //Preconfigured Password Key Derivation Parameters
        public static readonly int SaltBitSize = 64;
        public static readonly int Iterations = 10000;
        public static readonly int MinPasswordLength = 12;
    
        /// <summary>
        /// Helper that generates a random key on each call.
        /// </summary>
        /// <returns></returns>
        public static byte[] NewKey()
        {
          var key = new byte[KeyBitSize / 8];
          Random.GetBytes(key);
          return key;
        }
    
        /// <summary>
        /// Simple Encryption (AES) then Authentication (HMAC) for a UTF8 Message.
        /// </summary>
        /// <param name="secretMessage">The secret message.</param>
        /// <param name="cryptKey">The crypt key.</param>
        /// <param name="authKey">The auth key.</param>
        /// <param name="nonSecretPayload">(Optional) Non-Secret Payload.</param>
        /// <returns>
        /// Encrypted Message
        /// </returns>
        /// <exception cref="System.ArgumentException">Secret Message Required!;secretMessage</exception>
        /// <remarks>
        /// Adds overhead of (Optional-Payload + BlockSize(16) + Message-Padded-To-Blocksize +  HMac-Tag(32)) * 1.33 Base64
        /// </remarks>
        public static string SimpleEncrypt(string secretMessage, byte[] cryptKey, byte[] authKey,
                           byte[] nonSecretPayload = null)
        {
          if (string.IsNullOrEmpty(secretMessage))
            throw new ArgumentException("Secret Message Required!", "secretMessage");
    
          var plainText = Encoding.UTF8.GetBytes(secretMessage);
          var cipherText = SimpleEncrypt(plainText, cryptKey, authKey, nonSecretPayload);
          return Convert.ToBase64String(cipherText);
        }
    
        /// <summary>
        /// Simple Authentication (HMAC) then Decryption (AES) for a secrets UTF8 Message.
        /// </summary>
        /// <param name="encryptedMessage">The encrypted message.</param>
        /// <param name="cryptKey">The crypt key.</param>
        /// <param name="authKey">The auth key.</param>
        /// <param name="nonSecretPayloadLength">Length of the non secret payload.</param>
        /// <returns>
        /// Decrypted Message
        /// </returns>
        /// <exception cref="System.ArgumentException">Encrypted Message Required!;encryptedMessage</exception>
        public static string SimpleDecrypt(string encryptedMessage, byte[] cryptKey, byte[] authKey,
                           int nonSecretPayloadLength = 0)
        {
          if (string.IsNullOrWhiteSpace(encryptedMessage))
            throw new ArgumentException("Encrypted Message Required!", "encryptedMessage");
    
          var cipherText = Convert.FromBase64String(encryptedMessage);
          var plainText = SimpleDecrypt(cipherText, cryptKey, authKey, nonSecretPayloadLength);
          return plainText == null ? null : Encoding.UTF8.GetString(plainText);
        }
    
        /// <summary>
        /// Simple Encryption (AES) then Authentication (HMAC) of a UTF8 message
        /// using Keys derived from a Password (PBKDF2).
        /// </summary>
        /// <param name="secretMessage">The secret message.</param>
        /// <param name="password">The password.</param>
        /// <param name="nonSecretPayload">The non secret payload.</param>
        /// <returns>
        /// Encrypted Message
        /// </returns>
        /// <exception cref="System.ArgumentException">password</exception>
        /// <remarks>
        /// Significantly less secure than using random binary keys.
        /// Adds additional non secret payload for key generation parameters.
        /// </remarks>
        public static string SimpleEncryptWithPassword(string secretMessage, string password,
                                 byte[] nonSecretPayload = null)
        {
          if (string.IsNullOrEmpty(secretMessage))
            throw new ArgumentException("Secret Message Required!", "secretMessage");
    
          var plainText = Encoding.UTF8.GetBytes(secretMessage);
          var cipherText = SimpleEncryptWithPassword(plainText, password, nonSecretPayload);
          return Convert.ToBase64String(cipherText);
        }
    
        /// <summary>
        /// Simple Authentication (HMAC) and then Descryption (AES) of a UTF8 Message
        /// using keys derived from a password (PBKDF2). 
        /// </summary>
        /// <param name="encryptedMessage">The encrypted message.</param>
        /// <param name="password">The password.</param>
        /// <param name="nonSecretPayloadLength">Length of the non secret payload.</param>
        /// <returns>
        /// Decrypted Message
        /// </returns>
        /// <exception cref="System.ArgumentException">Encrypted Message Required!;encryptedMessage</exception>
        /// <remarks>
        /// Significantly less secure than using random binary keys.
        /// </remarks>
        public static string SimpleDecryptWithPassword(string encryptedMessage, string password,
                                 int nonSecretPayloadLength = 0)
        {
          if (string.IsNullOrWhiteSpace(encryptedMessage))
            throw new ArgumentException("Encrypted Message Required!", "encryptedMessage");
    
          var cipherText = Convert.FromBase64String(encryptedMessage);
          var plainText = SimpleDecryptWithPassword(cipherText, password, nonSecretPayloadLength);
          return plainText == null ? null : Encoding.UTF8.GetString(plainText);
        }
    
        public static byte[] SimpleEncrypt(byte[] secretMessage, byte[] cryptKey, byte[] authKey, byte[] nonSecretPayload = null)
        {
          //User Error Checks
          if (cryptKey == null || cryptKey.Length != KeyBitSize / 8)
            throw new ArgumentException(String.Format("Key needs to be {0} bit!", KeyBitSize), "cryptKey");
    
          if (authKey == null || authKey.Length != KeyBitSize / 8)
            throw new ArgumentException(String.Format("Key needs to be {0} bit!", KeyBitSize), "authKey");
    
          if (secretMessage == null || secretMessage.Length < 1)
            throw new ArgumentException("Secret Message Required!", "secretMessage");
    
          //non-secret payload optional
          nonSecretPayload = nonSecretPayload ?? new byte[] { };
    
          byte[] cipherText;
          byte[] iv;
    
          using (var aes = new AesManaged
          {
            KeySize = KeyBitSize,
            BlockSize = BlockBitSize,
            Mode = CipherMode.CBC,
            Padding = PaddingMode.PKCS7
          })
          {
    
            //Use random IV
            aes.GenerateIV();
            iv = aes.IV;
    
            using (var encrypter = aes.CreateEncryptor(cryptKey, iv))
            using (var cipherStream = new MemoryStream())
            {
              using (var cryptoStream = new CryptoStream(cipherStream, encrypter, CryptoStreamMode.Write))
              using (var binaryWriter = new BinaryWriter(cryptoStream))
              {
                //Encrypt Data
                binaryWriter.Write(secretMessage);
              }
    
              cipherText = cipherStream.ToArray();
            }
    
          }
    
          //Assemble encrypted message and add authentication
          using (var hmac = new HMACSHA256(authKey))
          using (var encryptedStream = new MemoryStream())
          {
            using (var binaryWriter = new BinaryWriter(encryptedStream))
            {
              //Prepend non-secret payload if any
              binaryWriter.Write(nonSecretPayload);
              //Prepend IV
              binaryWriter.Write(iv);
              //Write Ciphertext
              binaryWriter.Write(cipherText);
              binaryWriter.Flush();
    
              //Authenticate all data
              var tag = hmac.ComputeHash(encryptedStream.ToArray());
              //Postpend tag
              binaryWriter.Write(tag);
            }
            return encryptedStream.ToArray();
          }
    
        }
    
        public static byte[] SimpleDecrypt(byte[] encryptedMessage, byte[] cryptKey, byte[] authKey, int nonSecretPayloadLength = 0)
        {
    
          //Basic Usage Error Checks
          if (cryptKey == null || cryptKey.Length != KeyBitSize / 8)
            throw new ArgumentException(String.Format("CryptKey needs to be {0} bit!", KeyBitSize), "cryptKey");
    
          if (authKey == null || authKey.Length != KeyBitSize / 8)
            throw new ArgumentException(String.Format("AuthKey needs to be {0} bit!", KeyBitSize), "authKey");
    
          if (encryptedMessage == null || encryptedMessage.Length == 0)
            throw new ArgumentException("Encrypted Message Required!", "encryptedMessage");
    
          using (var hmac = new HMACSHA256(authKey))
          {
            var sentTag = new byte[hmac.HashSize / 8];
            //Calculate Tag
            var calcTag = hmac.ComputeHash(encryptedMessage, 0, encryptedMessage.Length - sentTag.Length);
            var ivLength = (BlockBitSize / 8);
    
            //if message length is to small just return null
            if (encryptedMessage.Length < sentTag.Length + nonSecretPayloadLength + ivLength)
              return null;
    
            //Grab Sent Tag
            Array.Copy(encryptedMessage, encryptedMessage.Length - sentTag.Length, sentTag, 0, sentTag.Length);
    
            //Compare Tag with constant time comparison
            var compare = 0;
            for (var i = 0; i < sentTag.Length; i++)
              compare |= sentTag[i] ^ calcTag[i]; 
    
            //if message doesn't authenticate return null
            if (compare != 0)
              return null;
    
            using (var aes = new AesManaged
            {
              KeySize = KeyBitSize,
              BlockSize = BlockBitSize,
              Mode = CipherMode.CBC,
              Padding = PaddingMode.PKCS7
            })
            {
    
              //Grab IV from message
              var iv = new byte[ivLength];
              Array.Copy(encryptedMessage, nonSecretPayloadLength, iv, 0, iv.Length);
    
              using (var decrypter = aes.CreateDecryptor(cryptKey, iv))
              using (var plainTextStream = new MemoryStream())
              {
                using (var decrypterStream = new CryptoStream(plainTextStream, decrypter, CryptoStreamMode.Write))
                using (var binaryWriter = new BinaryWriter(decrypterStream))
                {
                  //Decrypt Cipher Text from Message
                  binaryWriter.Write(
                    encryptedMessage,
                    nonSecretPayloadLength + iv.Length,
                    encryptedMessage.Length - nonSecretPayloadLength - iv.Length - sentTag.Length
                  );
                }
                //Return Plain Text
                return plainTextStream.ToArray();
              }
            }
          }
        }
    
        public static byte[] SimpleEncryptWithPassword(byte[] secretMessage, string password, byte[] nonSecretPayload = null)
        {
          nonSecretPayload = nonSecretPayload ?? new byte[] {};
    
          //User Error Checks
          if (string.IsNullOrWhiteSpace(password) || password.Length < MinPasswordLength)
            throw new ArgumentException(String.Format("Must have a password of at least {0} characters!", MinPasswordLength), "password");
    
          if (secretMessage == null || secretMessage.Length ==0)
            throw new ArgumentException("Secret Message Required!", "secretMessage");
    
          var payload = new byte[((SaltBitSize / 8) * 2) + nonSecretPayload.Length];
    
          Array.Copy(nonSecretPayload, payload, nonSecretPayload.Length);
          int payloadIndex = nonSecretPayload.Length;
    
          byte[] cryptKey;
          byte[] authKey;
          //Use Random Salt to prevent pre-generated weak password attacks.
          using (var generator = new Rfc2898DeriveBytes(password, SaltBitSize / 8, Iterations))
          {
            var salt = generator.Salt;
    
            //Generate Keys
            cryptKey = generator.GetBytes(KeyBitSize / 8);
    
            //Create Non Secret Payload
            Array.Copy(salt, 0, payload, payloadIndex, salt.Length);
            payloadIndex += salt.Length;
          }
    
          //Deriving separate key, might be less efficient than using HKDF, 
          //but now compatible with RNEncryptor which had a very similar wireformat and requires less code than HKDF.
          using (var generator = new Rfc2898DeriveBytes(password, SaltBitSize / 8, Iterations))
          {
            var salt = generator.Salt;
    
            //Generate Keys
            authKey = generator.GetBytes(KeyBitSize / 8);
    
            //Create Rest of Non Secret Payload
            Array.Copy(salt, 0, payload, payloadIndex, salt.Length);
          }
    
          return SimpleEncrypt(secretMessage, cryptKey, authKey, payload);
        }
    
        public static byte[] SimpleDecryptWithPassword(byte[] encryptedMessage, string password, int nonSecretPayloadLength = 0)
        {
          //User Error Checks
          if (string.IsNullOrWhiteSpace(password) || password.Length < MinPasswordLength)
            throw new ArgumentException(String.Format("Must have a password of at least {0} characters!", MinPasswordLength), "password");
    
          if (encryptedMessage == null || encryptedMessage.Length == 0)
            throw new ArgumentException("Encrypted Message Required!", "encryptedMessage");
    
          var cryptSalt = new byte[SaltBitSize / 8];
          var authSalt = new byte[SaltBitSize / 8];
    
          //Grab Salt from Non-Secret Payload
          Array.Copy(encryptedMessage, nonSecretPayloadLength, cryptSalt, 0, cryptSalt.Length);
          Array.Copy(encryptedMessage, nonSecretPayloadLength + cryptSalt.Length, authSalt, 0, authSalt.Length);
    
          byte[] cryptKey;
          byte[] authKey;
    
          //Generate crypt key
          using (var generator = new Rfc2898DeriveBytes(password, cryptSalt, Iterations))
          {
            cryptKey = generator.GetBytes(KeyBitSize / 8);
          }
          //Generate auth key
          using (var generator = new Rfc2898DeriveBytes(password, authSalt, Iterations))
          {
            authKey = generator.GetBytes(KeyBitSize / 8);
          }
    
          return SimpleDecrypt(encryptedMessage, cryptKey, authKey, cryptSalt.Length + authSalt.Length + nonSecretPayloadLength);
        }
      }
    }

----------
**Castillo hinchable AES-GCM [[Gist]](https://gist.github.com/jbtule/4336842#file-aesgcm-cs)**

    /*
     * This work (Modern Encryption of a String C#, by James Tuley), 
     * identified by James Tuley, is free of known copyright restrictions.
     * https://gist.github.com/4336842
     * http://creativecommons.org/publicdomain/mark/1.0/ 
     */
    
    using System;
    using System.IO;
    using System.Text;
    using Org.BouncyCastle.Crypto;
    using Org.BouncyCastle.Crypto.Engines;
    using Org.BouncyCastle.Crypto.Generators;
    using Org.BouncyCastle.Crypto.Modes;
    using Org.BouncyCastle.Crypto.Parameters;
    using Org.BouncyCastle.Security;
    namespace Encryption
    {
    
      public static class AESGCM
      {
        private static readonly SecureRandom Random = new SecureRandom();
    
        //Preconfigured Encryption Parameters
        public static readonly int NonceBitSize = 128;
        public static readonly int MacBitSize = 128;
        public static readonly int KeyBitSize = 256;
    
        //Preconfigured Password Key Derivation Parameters
        public static readonly int SaltBitSize = 128;
        public static readonly int Iterations = 10000;
        public static readonly int MinPasswordLength = 12;
    
    
        /// <summary>
        /// Helper that generates a random new key on each call.
        /// </summary>
        /// <returns></returns>
        public static byte[] NewKey()
        {
          var key = new byte[KeyBitSize / 8];
          Random.NextBytes(key);
          return key;
        }
    
        /// <summary>
        /// Simple Encryption And Authentication (AES-GCM) of a UTF8 string.
        /// </summary>
        /// <param name="secretMessage">The secret message.</param>
        /// <param name="key">The key.</param>
        /// <param name="nonSecretPayload">Optional non-secret payload.</param>
        /// <returns>
        /// Encrypted Message
        /// </returns>
        /// <exception cref="System.ArgumentException">Secret Message Required!;secretMessage</exception>
        /// <remarks>
        /// Adds overhead of (Optional-Payload + BlockSize(16) + Message +  HMac-Tag(16)) * 1.33 Base64
        /// </remarks>
        public static string SimpleEncrypt(string secretMessage, byte[] key, byte[] nonSecretPayload = null)
        {
          if (string.IsNullOrEmpty(secretMessage))
            throw new ArgumentException("Secret Message Required!", "secretMessage");
    
          var plainText = Encoding.UTF8.GetBytes(secretMessage);
          var cipherText = SimpleEncrypt(plainText, key, nonSecretPayload);
          return Convert.ToBase64String(cipherText);
        }
    
    
        /// <summary>
        /// Simple Decryption & Authentication (AES-GCM) of a UTF8 Message
        /// </summary>
        /// <param name="encryptedMessage">The encrypted message.</param>
        /// <param name="key">The key.</param>
        /// <param name="nonSecretPayloadLength">Length of the optional non-secret payload.</param>
        /// <returns>Decrypted Message</returns>
        public static string SimpleDecrypt(string encryptedMessage, byte[] key, int nonSecretPayloadLength = 0)
        {
          if (string.IsNullOrEmpty(encryptedMessage))
            throw new ArgumentException("Encrypted Message Required!", "encryptedMessage");
    
          var cipherText = Convert.FromBase64String(encryptedMessage);
          var plainText = SimpleDecrypt(cipherText, key, nonSecretPayloadLength);
          return plainText == null ? null : Encoding.UTF8.GetString(plainText);
        }
    
        /// <summary>
        /// Simple Encryption And Authentication (AES-GCM) of a UTF8 String
        /// using key derived from a password (PBKDF2).
        /// </summary>
        /// <param name="secretMessage">The secret message.</param>
        /// <param name="password">The password.</param>
        /// <param name="nonSecretPayload">The non secret payload.</param>
        /// <returns>
        /// Encrypted Message
        /// </returns>
        /// <remarks>
        /// Significantly less secure than using random binary keys.
        /// Adds additional non secret payload for key generation parameters.
        /// </remarks>
        public static string SimpleEncryptWithPassword(string secretMessage, string password,
                                 byte[] nonSecretPayload = null)
        {
          if (string.IsNullOrEmpty(secretMessage))
            throw new ArgumentException("Secret Message Required!", "secretMessage");
    
          var plainText = Encoding.UTF8.GetBytes(secretMessage);
          var cipherText = SimpleEncryptWithPassword(plainText, password, nonSecretPayload);
          return Convert.ToBase64String(cipherText);
        }
    
    
        /// <summary>
        /// Simple Decryption and Authentication (AES-GCM) of a UTF8 message
        /// using a key derived from a password (PBKDF2)
        /// </summary>
        /// <param name="encryptedMessage">The encrypted message.</param>
        /// <param name="password">The password.</param>
        /// <param name="nonSecretPayloadLength">Length of the non secret payload.</param>
        /// <returns>
        /// Decrypted Message
        /// </returns>
        /// <exception cref="System.ArgumentException">Encrypted Message Required!;encryptedMessage</exception>
        /// <remarks>
        /// Significantly less secure than using random binary keys.
        /// </remarks>
        public static string SimpleDecryptWithPassword(string encryptedMessage, string password,
                                 int nonSecretPayloadLength = 0)
        {
          if (string.IsNullOrWhiteSpace(encryptedMessage))
            throw new ArgumentException("Encrypted Message Required!", "encryptedMessage");
    
          var cipherText = Convert.FromBase64String(encryptedMessage);
          var plainText = SimpleDecryptWithPassword(cipherText, password, nonSecretPayloadLength);
          return plainText == null ? null : Encoding.UTF8.GetString(plainText);
        }
    
        public static byte[] SimpleEncrypt(byte[] secretMessage, byte[] key, byte[] nonSecretPayload = null)
        {
          //User Error Checks
          if (key == null || key.Length != KeyBitSize / 8)
            throw new ArgumentException(String.Format("Key needs to be {0} bit!", KeyBitSize), "key");
    
          if (secretMessage == null || secretMessage.Length == 0)
            throw new ArgumentException("Secret Message Required!", "secretMessage");
    
          //Non-secret Payload Optional
          nonSecretPayload = nonSecretPayload ?? new byte[] { };
    
          //Using random nonce large enough not to repeat
          var nonce = new byte[NonceBitSize / 8];
          Random.NextBytes(nonce, 0, nonce.Length);
    
          var cipher = new GcmBlockCipher(new AesFastEngine());
          var parameters = new AeadParameters(new KeyParameter(key), MacBitSize, nonce, nonSecretPayload);
          cipher.Init(true, parameters);
    
          //Generate Cipher Text With Auth Tag
          var cipherText = new byte[cipher.GetOutputSize(secretMessage.Length)];
          var len = cipher.ProcessBytes(secretMessage, 0, secretMessage.Length, cipherText, 0);
          cipher.DoFinal(cipherText, len);
    
          //Assemble Message
          using (var combinedStream = new MemoryStream())
          {
            using (var binaryWriter = new BinaryWriter(combinedStream))
            {
              //Prepend Authenticated Payload
              binaryWriter.Write(nonSecretPayload);
              //Prepend Nonce
              binaryWriter.Write(nonce);
              //Write Cipher Text
              binaryWriter.Write(cipherText);
            }
            return combinedStream.ToArray();
          }
        }
    
        public static byte[] SimpleDecrypt(byte[] encryptedMessage, byte[] key, int nonSecretPayloadLength = 0)
        {
          //User Error Checks
          if (key == null || key.Length != KeyBitSize / 8)
            throw new ArgumentException(String.Format("Key needs to be {0} bit!", KeyBitSize), "key");
    
          if (encryptedMessage == null || encryptedMessage.Length == 0)
            throw new ArgumentException("Encrypted Message Required!", "encryptedMessage");
    
          using (var cipherStream = new MemoryStream(encryptedMessage))
          using (var cipherReader = new BinaryReader(cipherStream))
          {
            //Grab Payload
            var nonSecretPayload = cipherReader.ReadBytes(nonSecretPayloadLength);
    
            //Grab Nonce
            var nonce = cipherReader.ReadBytes(NonceBitSize / 8);
           
            var cipher = new GcmBlockCipher(new AesFastEngine());
            var parameters = new AeadParameters(new KeyParameter(key), MacBitSize, nonce, nonSecretPayload);
            cipher.Init(false, parameters);
    
            //Decrypt Cipher Text
            var cipherText = cipherReader.ReadBytes(encryptedMessage.Length - nonSecretPayloadLength - nonce.Length);
            var plainText = new byte[cipher.GetOutputSize(cipherText.Length)];  
    
            try
            {
              var len = cipher.ProcessBytes(cipherText, 0, cipherText.Length, plainText, 0);
              cipher.DoFinal(plainText, len);
    
            }
            catch (InvalidCipherTextException)
            {
              //Return null if it doesn't authenticate
              return null;
            }
    
            return plainText;
          }
    
        }
    
        public static byte[] SimpleEncryptWithPassword(byte[] secretMessage, string password, byte[] nonSecretPayload = null)
        {
          nonSecretPayload = nonSecretPayload ?? new byte[] {};
    
          //User Error Checks
          if (string.IsNullOrWhiteSpace(password) || password.Length < MinPasswordLength)
            throw new ArgumentException(String.Format("Must have a password of at least {0} characters!", MinPasswordLength), "password");
    
          if (secretMessage == null || secretMessage.Length == 0)
            throw new ArgumentException("Secret Message Required!", "secretMessage");
    
          var generator = new Pkcs5S2ParametersGenerator();
    
          //Use Random Salt to minimize pre-generated weak password attacks.
          var salt = new byte[SaltBitSize / 8];
          Random.NextBytes(salt);
    
          generator.Init(
            PbeParametersGenerator.Pkcs5PasswordToBytes(password.ToCharArray()),
            salt,
            Iterations);
    
          //Generate Key
          var key = (KeyParameter)generator.GenerateDerivedMacParameters(KeyBitSize);
    
          //Create Full Non Secret Payload
          var payload = new byte[salt.Length + nonSecretPayload.Length];
          Array.Copy(nonSecretPayload, payload, nonSecretPayload.Length);
          Array.Copy(salt,0, payload,nonSecretPayload.Length, salt.Length);
    
          return SimpleEncrypt(secretMessage, key.GetKey(), payload);
        }
    
        public static byte[] SimpleDecryptWithPassword(byte[] encryptedMessage, string password, int nonSecretPayloadLength = 0)
        {
          //User Error Checks
          if (string.IsNullOrWhiteSpace(password) || password.Length < MinPasswordLength)
            throw new ArgumentException(String.Format("Must have a password of at least {0} characters!", MinPasswordLength), "password");
    
          if (encryptedMessage == null || encryptedMessage.Length == 0)
            throw new ArgumentException("Encrypted Message Required!", "encryptedMessage");
    
          var generator = new Pkcs5S2ParametersGenerator();
    
          //Grab Salt from Payload
          var salt = new byte[SaltBitSize / 8];
          Array.Copy(encryptedMessage, nonSecretPayloadLength, salt, 0, salt.Length);
    
          generator.Init(
            PbeParametersGenerator.Pkcs5PasswordToBytes(password.ToCharArray()),
            salt,
            Iterations);
    
          //Generate Key
          var key = (KeyParameter)generator.GenerateDerivedMacParameters(KeyBitSize);
    
          return SimpleDecrypt(encryptedMessage, key.GetKey(), salt.Length + nonSecretPayloadLength);
        }
      }
    }

[1]: http://en.wikipedia.org/wiki/Galois/Counter_Mode
[2]: http://en.wikipedia.org/wiki/Advanced_Encryption_Standard
[3]: http://en.wikipedia.org/wiki/HMAC
[4]: http://crypto.stackexchange.com/a/205/1934

## Introducción al cifrado simétrico y asimétrico
Puede mejorar la seguridad para el tránsito o el almacenamiento de datos implementando técnicas de cifrado. Básicamente, existen dos enfoques al usar *System.Security.Cryptography*: **simétrico** y **asimétrico.**

----------

## [**Cifrado simétrico**][1] ##

Este método utiliza una clave privada para realizar la transformación de datos.

Ventajas:
- Los algoritmos simétricos consumen menos recursos y son más rápidos que los asimétricos.
- La cantidad de datos que puede cifrar es ilimitada.


Contras:
- El cifrado y el descifrado utilizan la misma clave. Alguien podrá descifrar sus datos si la clave está comprometida.
- Podría terminar con muchas claves secretas diferentes para administrar si elige usar una clave secreta diferente para datos diferentes.

En System.Security.Cryptography, tiene diferentes clases que realizan cifrado simétrico, se conocen como [cifrados de bloque][2]:
- [AesManaged][3] (algoritmo [AES][4]).
- [AesCryptoServiceProvider][5] ([AES][4] algoritmo [denuncia FIPS 140-2][6]).
- [DESCryptoServiceProvider][7] (algoritmo [DES][8]).
- [RC2CryptoServiceProvider][9] (algoritmo [Rivest Cipher 2][10]).
- [RijndaelManaged][11] (algoritmo [AES][4]). *Nota*: RijndaelManaged **no** [FIPS-197][12] reclamo.
- [TripleDES][13] (algoritmo [TripleDES][14]).



----------

## [**Cifrado asimétrico**][17] ##

Este método utiliza una combinación de claves públicas y privadas para realizar la transformación de datos.

Ventajas:
- Utiliza claves más grandes que los algoritmos simétricos, por lo que son menos susceptibles de ser descifrados mediante el uso de fuerza bruta.
- Es más fácil garantizar quién puede cifrar y descifrar los datos porque se basa en dos claves (pública y privada).

Contras:
- Hay un límite en la cantidad de datos que puede cifrar. El límite es diferente para cada algoritmo y normalmente es proporcional al tamaño de la clave del algoritmo. Por ejemplo, un objeto RSACryptoServiceProvider con una longitud de clave de 1024 bits solo puede cifrar un mensaje de menos de 128 bytes.
- Los algoritmos asimétricos son muy lentos en comparación con los algoritmos simétricos.

En System.Security.Cryptography tiene acceso a diferentes clases que realizan cifrado asimétrico:
- [DSACryptoServiceProvider][18] (algoritmo [Algoritmo de firma digital][19])
- [RSACryptoServiceProvider][21] (algoritmo [Algoritmo RSA][20])


[1]: https://en.wikipedia.org/wiki/Symmetric-key_algorithm
[2]: https://en.wikipedia.org/wiki/Block_cipher
[3]: https://msdn.microsoft.com/en-us/library/system.security.cryptography.aesmanaged
[4]: https://en.wikipedia.org/wiki/Advanced_Encryption_Standard
[5]: https://msdn.microsoft.com/en-us/library/system.security.cryptography.aescryptoserviceprovider
[6]: https://blogs.msdn.microsoft.com/winsdk/2010/05/28/behaviour-of-aescryptoserviceprovider-class-with-fips-policy-set-unset/
[7]: https://msdn.microsoft.com/en-us/library/system.security.cryptography.descryptoserviceprovider
[8]: https://en.wikipedia.org/wiki/Data_Encryption_Standard
[9]: https://msdn.microsoft.com/en-us/library/system.security.cryptography.rc2cryptoserviceprovider
[10]: https://en.wikipedia.org/wiki/RC2
[11]: https://msdn.microsoft.com/en-us/library/system.security.cryptography.rijndaelmanaged
[12]: https://blogs.msdn.microsoft.com/shawnfa/2006/10/09/las-diferencias-entre-rijndael-y-aes/
[13]: https://msdn.microsoft.com/en-us/library/system.security.cryptography.tripledes
[14]: https://en.wikipedia.org/wiki/Triple_DES
[15]: https://msdn.microsoft.com/EN-US/library/system.security.cryptography.aesmanaged
[16]: https://msdn.microsoft.com/en-us/library/system.security.cryptography.symmetricalgorithm.iv
[17]: https://en.wikipedia.org/wiki/Public-key_cryptography
[18]: https://msdn.microsoft.com/en-us/library/system.security.cryptography.dsacryptoserviceprovider.aspx
[19]: https://en.wikipedia.org/wiki/Digital_Signature_Algorithm
[20]: https://en.wikipedia.org/wiki/RSA_(criptosistema)
[21]: https://msdn.microsoft.com/en-us/library/system.security.cryptography.rsacryptoserviceprovider.aspx

## Hashing de contraseñas
¡Las contraseñas nunca deben almacenarse como texto sin formato! Deben ser hash con una sal generada aleatoriamente (para defenderse de los ataques de la tabla del arco iris) utilizando un algoritmo de hash de contraseña lento. Se puede usar una gran cantidad de iteraciones (> 10k) para ralentizar los ataques de fuerza bruta. Un retraso de ~100 ms es aceptable para un usuario que inicia sesión, pero dificulta la ruptura de una contraseña larga. Al elegir una cantidad de iteraciones, debe usar el valor máximo tolerable para su aplicación y aumentarlo a medida que mejora el rendimiento de la computadora. También deberá considerar detener las solicitudes repetidas que podrían usarse como un ataque DoS.

Cuando se genera un hash por primera vez, se puede generar una sal para usted, el hash y la sal resultantes se pueden almacenar en un archivo.


    private void firstHash(string userName, string userPassword, int numberOfItterations)
    {
        Rfc2898DeriveBytes PBKDF2 = new Rfc2898DeriveBytes(userPassword, 8, numberOfItterations);    //Hash the password with a 8 byte salt
        byte[] hashedPassword = PBKDF2.GetBytes(20);    //Returns a 20 byte hash
        byte[] salt = PBKDF2.Salt;
        writeHashToFile(userName, hashedPassword, salt, numberOfItterations); //Store the hashed password with the salt and number of itterations to check against future password entries
    }

Verificar la contraseña de un usuario existente, leer su hash y sal de un archivo y comparar con el hash de la contraseña ingresada

    private bool checkPassword(string userName, string userPassword, int numberOfItterations)
    {
        byte[] usersHash = getUserHashFromFile(userName);
        byte[] userSalt = getUserSaltFromFile(userName);
        Rfc2898DeriveBytes PBKDF2 = new Rfc2898DeriveBytes(userPassword, userSalt, numberOfItterations);    //Hash the password with the users salt
        byte[] hashedPassword = PBKDF2.GetBytes(20);    //Returns a 20 byte hash            
        bool passwordsMach = comparePasswords(usersHash, hashedPassword);    //Compares byte arrays
        return passwordsMach;
    }


## Cifrado de archivos simétrico simple
El siguiente ejemplo de código muestra un medio rápido y fácil de cifrar y descifrar archivos mediante el algoritmo de cifrado simétrico AES.

El código genera aleatoriamente los vectores Salt y de inicialización cada vez que se cifra un archivo, lo que significa que cifrar el mismo archivo con la misma contraseña siempre dará como resultado una salida diferente. El salt y el IV se escriben en el archivo de salida para que solo se requiera la contraseña para descifrarlo.

    public static void ProcessFile(string inputPath, string password, bool encryptMode, string outputPath)
    {
        using (var cypher = new AesManaged())
        using (var fsIn = new FileStream(inputPath, FileMode.Open))
        using (var fsOut = new FileStream(outputPath, FileMode.Create))
        {
            const int saltLength = 256;
            var salt = new byte[saltLength];
            var iv = new byte[cypher.BlockSize / 8];

            if (encryptMode)
            {
                // Generate random salt and IV, then write them to file
                using (var rng = new RNGCryptoServiceProvider())
                {
                    rng.GetBytes(salt);
                    rng.GetBytes(iv);
                }
                fsOut.Write(salt, 0, salt.Length);
                fsOut.Write(iv, 0, iv.Length);
            }
            else
            {
                // Read the salt and IV from the file
                fsIn.Read(salt, 0, saltLength);
                fsIn.Read(iv, 0, iv.Length);
            }

            // Generate a secure password, based on the password and salt provided
            var pdb = new Rfc2898DeriveBytes(password, salt);
            var key = pdb.GetBytes(cypher.KeySize / 8);

            // Encrypt or decrypt the file
            using (var cryptoTransform = encryptMode
                ? cypher.CreateEncryptor(key, iv)
                : cypher.CreateDecryptor(key, iv))
            using (var cs = new CryptoStream(fsOut, cryptoTransform, CryptoStreamMode.Write))
            {
                fsIn.CopyTo(cs);
            }
        }
    }

## Datos aleatorios criptográficamente seguros
Hay momentos en los que la clase Random() del marco puede no considerarse lo suficientemente aleatoria, dado que se basa en un generador de números pseudoaleatorios. Sin embargo, las clases Crypto del marco proporcionan algo más sólido en forma de RNGCryptoServiceProvider.

Los siguientes ejemplos de código demuestran cómo generar matrices de bytes, cadenas y números criptográficamente seguros.

**Arreglo de bytes aleatorios**

    public static byte[] GenerateRandomData(int length)
    {
        var rnd = new byte[length];
        using (var rng = new RNGCryptoServiceProvider())
            rng.GetBytes(rnd);
        return rnd;
    }

**Entero aleatorio** (con distribución uniforme)

    public static int GenerateRandomInt(int minVal=0, int maxVal=100)
    {
        var rnd = new byte[4];
        using (var rng = new RNGCryptoServiceProvider())
            rng.GetBytes(rnd);
        var i = Math.Abs(BitConverter.ToInt32(rnd, 0));
        return Convert.ToInt32(i % (maxVal - minVal + 1) + minVal);
    }

**Cadena aleatoria**

    public static string GenerateRandomString(int length, string allowableChars=null)
    {
        if (string.IsNullOrEmpty(allowableChars))
            allowableChars = @"ABCDEFGHIJKLMNOPQRSTUVWXYZ";

        // Generate random data
        var rnd = new byte[length];
        using (var rng = new RNGCryptoServiceProvider())
            rng.GetBytes(rnd);

        // Generate the output string
        var allowable = allowableChars.ToCharArray();
        var l = allowable.Length;
        var chars = new char[length];
        for (var i = 0; i < length; i++)
            chars[i] = allowable[rnd[i] % l];

        return new string(chars);
    }




## Cifrado asimétrico rápido de archivos
El cifrado asimétrico a menudo se considera preferible al cifrado simétrico para transferir mensajes a otras partes. Esto se debe principalmente a que anula muchos de los riesgos relacionados con el intercambio de una clave compartida y garantiza que, si bien cualquier persona con la clave pública puede cifrar un mensaje para el destinatario previsto, solo ese destinatario puede descifrarlo. Desafortunadamente, la principal desventaja de los algoritmos de cifrado asimétrico es que son significativamente más lentos que sus primos simétricos. Como tal, el cifrado asimétrico de archivos, especialmente los grandes, a menudo puede ser un proceso muy intensivo desde el punto de vista computacional.

Para proporcionar seguridad Y rendimiento, se puede adoptar un enfoque híbrido. Esto implica la generación criptográficamente aleatoria de una clave y un vector de inicialización para el cifrado *simétrico*. Luego, estos valores se cifran con un algoritmo *asimétrico* y se escriben en el archivo de salida, antes de usarse para cifrar los datos de origen *simétricamente* y agregarlos a la salida.

Este enfoque proporciona un alto grado de rendimiento y seguridad, ya que los datos se cifran mediante un algoritmo simétrico (rápido) y la clave y iv, ambos generados aleatoriamente (seguro), se cifran mediante un algoritmo asimétrico (seguro). También tiene la ventaja añadida de que la misma carga útil cifrada en diferentes ocasiones tendrá un texto cifrado muy diferente, porque las claves simétricas se generan aleatoriamente cada vez.

La siguiente clase demuestra el cifrado asimétrico de cadenas y matrices de bytes, así como el cifrado de archivos híbrido.

    public static class AsymmetricProvider
    {
        #region Key Generation
        public class KeyPair
        {
            public string PublicKey { get; set; }
            public string PrivateKey { get; set; }
        }

        public static KeyPair GenerateNewKeyPair(int keySize = 4096)
        {
            // KeySize is measured in bits. 1024 is the default, 2048 is better, 4096 is more robust but takes a fair bit longer to generate.
            using (var rsa = new RSACryptoServiceProvider(keySize))
            {
                return new KeyPair {PublicKey = rsa.ToXmlString(false), PrivateKey = rsa.ToXmlString(true)};
            }
        }

        #endregion

        #region Asymmetric Data Encryption and Decryption

        public static byte[] EncryptData(byte[] data, string publicKey)
        {
            using (var asymmetricProvider = new RSACryptoServiceProvider())
            {
                asymmetricProvider.FromXmlString(publicKey);
                return asymmetricProvider.Encrypt(data, true);
            }
        }

        public static byte[] DecryptData(byte[] data, string publicKey)
        {
            using (var asymmetricProvider = new RSACryptoServiceProvider())
            {
                asymmetricProvider.FromXmlString(publicKey);
                if (asymmetricProvider.PublicOnly)
                    throw new Exception("The key provided is a public key and does not contain the private key elements required for decryption");
                return asymmetricProvider.Decrypt(data, true);
            }
        }

        public static string EncryptString(string value, string publicKey)
        {
            return Convert.ToBase64String(EncryptData(Encoding.UTF8.GetBytes(value), publicKey));
        }

        public static string DecryptString(string value, string privateKey)
        {
            return Encoding.UTF8.GetString(EncryptData(Convert.FromBase64String(value), privateKey));
        }

        #endregion

        #region Hybrid File Encryption and Decription

        public static void EncryptFile(string inputFilePath, string outputFilePath, string publicKey)
        {
            using (var symmetricCypher = new AesManaged())
            {
                // Generate random key and IV for symmetric encryption
                var key = new byte[symmetricCypher.KeySize / 8];
                var iv = new byte[symmetricCypher.BlockSize / 8];
                using (var rng = new RNGCryptoServiceProvider())
                {
                    rng.GetBytes(key);
                    rng.GetBytes(iv);
                }

                // Encrypt the symmetric key and IV
                var buf = new byte[key.Length + iv.Length];
                Array.Copy(key, buf, key.Length);
                Array.Copy(iv, 0, buf, key.Length, iv.Length);
                buf = EncryptData(buf, publicKey);

                var bufLen = BitConverter.GetBytes(buf.Length);

                // Symmetrically encrypt the data and write it to the file, along with the encrypted key and iv
                using (var cypherKey = symmetricCypher.CreateEncryptor(key, iv))
                using (var fsIn = new FileStream(inputFilePath, FileMode.Open))
                using (var fsOut = new FileStream(outputFilePath, FileMode.Create))
                using (var cs = new CryptoStream(fsOut, cypherKey, CryptoStreamMode.Write))
                {
                    fsOut.Write(bufLen,0, bufLen.Length);
                    fsOut.Write(buf, 0, buf.Length);
                    fsIn.CopyTo(cs);
                }
            }
        }

        public static void DecryptFile(string inputFilePath, string outputFilePath, string privateKey)
        {
            using (var symmetricCypher = new AesManaged())
            using (var fsIn = new FileStream(inputFilePath, FileMode.Open))
            {
                // Determine the length of the encrypted key and IV
                var buf = new byte[sizeof(int)];
                fsIn.Read(buf, 0, buf.Length);
                var bufLen = BitConverter.ToInt32(buf, 0);

                // Read the encrypted key and IV data from the file and decrypt using the asymmetric algorithm
                buf = new byte[bufLen];
                fsIn.Read(buf, 0, buf.Length);
                buf = DecryptData(buf, privateKey);

                var key = new byte[symmetricCypher.KeySize / 8];
                var iv = new byte[symmetricCypher.BlockSize / 8];
                Array.Copy(buf, key, key.Length);
                Array.Copy(buf, key.Length, iv, 0, iv.Length);

                // Decript the file data using the symmetric algorithm
                using (var cypherKey = symmetricCypher.CreateDecryptor(key, iv))
                using (var fsOut = new FileStream(outputFilePath, FileMode.Create))
                using (var cs = new CryptoStream(fsOut, cypherKey, CryptoStreamMode.Write))
                {
                    fsIn.CopyTo(cs);
                }
            }
        }

        #endregion

        #region Key Storage

        public static void WritePublicKey(string publicKeyFilePath, string publicKey)
        {
            File.WriteAllText(publicKeyFilePath, publicKey);
        }
        public static string ReadPublicKey(string publicKeyFilePath)
        {
            return File.ReadAllText(publicKeyFilePath);
        }

        private const string SymmetricSalt = "Stack_Overflow!"; // Change me!

        public static string ReadPrivateKey(string privateKeyFilePath, string password)
        {
            var salt = Encoding.UTF8.GetBytes(SymmetricSalt);
            var cypherText = File.ReadAllBytes(privateKeyFilePath);

            using (var cypher = new AesManaged())
            {
                var pdb = new Rfc2898DeriveBytes(password, salt);
                var key = pdb.GetBytes(cypher.KeySize / 8);
                var iv = pdb.GetBytes(cypher.BlockSize / 8);

                using (var decryptor = cypher.CreateDecryptor(key, iv))
                using (var msDecrypt = new MemoryStream(cypherText))
                using (var csDecrypt = new CryptoStream(msDecrypt, decryptor, CryptoStreamMode.Read))
                using (var srDecrypt = new StreamReader(csDecrypt))
                {
                    return srDecrypt.ReadToEnd();
                }
            }
        }

        public static void WritePrivateKey(string privateKeyFilePath, string privateKey, string password)
        {
            var salt = Encoding.UTF8.GetBytes(SymmetricSalt);
            using (var cypher = new AesManaged())
            {
                var pdb = new Rfc2898DeriveBytes(password, salt);
                var key = pdb.GetBytes(cypher.KeySize / 8);
                var iv = pdb.GetBytes(cypher.BlockSize / 8);

                using (var encryptor = cypher.CreateEncryptor(key, iv))
                using (var fsEncrypt = new FileStream(privateKeyFilePath, FileMode.Create))
                using (var csEncrypt = new CryptoStream(fsEncrypt, encryptor, CryptoStreamMode.Write))
                using (var swEncrypt = new StreamWriter(csEncrypt))
                {
                    swEncrypt.Write(privateKey);
                }
            }
        }

        #endregion
    }

Ejemplo de uso:

    private static void HybridCryptoTest(string privateKeyPath, string privateKeyPassword, string inputPath)
    {
        // Setup the test
        var publicKeyPath = Path.ChangeExtension(privateKeyPath, ".public");
        var outputPath = Path.Combine(Path.ChangeExtension(inputPath, ".enc"));
        var testPath = Path.Combine(Path.ChangeExtension(inputPath, ".test"));

        if (!File.Exists(privateKeyPath))
        {
            var keys = AsymmetricProvider.GenerateNewKeyPair(2048);
            AsymmetricProvider.WritePublicKey(publicKeyPath, keys.PublicKey);
            AsymmetricProvider.WritePrivateKey(privateKeyPath, keys.PrivateKey, privateKeyPassword);
        }

        // Encrypt the file
        var publicKey = AsymmetricProvider.ReadPublicKey(publicKeyPath);
        AsymmetricProvider.EncryptFile(inputPath, outputPath, publicKey);

        // Decrypt it again to compare against the source file
        var privateKey = AsymmetricProvider.ReadPrivateKey(privateKeyPath, privateKeyPassword);
        AsymmetricProvider.DecryptFile(outputPath, testPath, privateKey);

        // Check that the two files match
        var source = File.ReadAllBytes(inputPath);
        var dest = File.ReadAllBytes(testPath);

        if (source.Length != dest.Length)
            throw new Exception("Length does not match");

        if (source.Where((t, i) => t != dest[i]).Any())
            throw new Exception("Data mismatch");
    }

