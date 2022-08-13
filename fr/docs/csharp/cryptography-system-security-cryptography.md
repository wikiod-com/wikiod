---
title: "Cryptographie (System.Security.Cryptography)"
slug: "cryptographie-systemsecuritycryptography"
draft: false
images: []
weight: 9908
type: docs
toc: true
---

## Exemples modernes de chiffrement authentifié symétrique d'une chaîne
La cryptographie est quelque chose de très difficile et après avoir passé beaucoup de temps à lire différents exemples et vu à quel point il est facile d'introduire une forme de vulnérabilité, j'ai trouvé une réponse écrite à l'origine par @jbtule que je trouve très bonne. Bonne lecture:

"La meilleure pratique générale pour le chiffrement symétrique consiste à utiliser le chiffrement authentifié avec données associées (AEAD), mais cela ne fait pas partie des bibliothèques de chiffrement .net standard. Ainsi, le premier exemple utilise [AES256][2] puis [HMAC256 ][3], une opération en deux étapes [Encrypt then MAC][4], qui nécessite plus de surcharge et plus de clés.

Le deuxième exemple utilise la pratique plus simple d'AES256-[GCM][1] en utilisant l'open source Bouncy Castle (via nuget).

Les deux exemples ont une fonction principale qui prend une chaîne de message secrète, une ou plusieurs clés et une charge utile facultative non secrète et renvoie une chaîne chiffrée authentifiée éventuellement précédée des données non secrètes. Idéalement, vous les utiliseriez avec des clés de 256 bits générées aléatoirement, voir `NewKey()`.

Les deux exemples ont également des méthodes d'assistance qui utilisent un mot de passe de chaîne pour générer les clés. Ces méthodes d'assistance sont fournies pour faciliter la comparaison avec d'autres exemples, mais elles sont * beaucoup moins sécurisées * car la force du mot de passe sera * beaucoup plus faible qu'une clé de 256 bits *.

**Mise à jour:**
Ajout de surcharges `byte[]`, et seul le [Gist](https://gist.github.com/4336842) a le formatage complet avec un retrait de 4 espaces et des documents api en raison des limites de réponse StackOverflow."

----------
**.NET Built-in Encrypt(AES)-Then-MAC(HMAC) [[Gist]](https://gist.github.com/jbtule/4336842#file-aesthenhmac-cs)**

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
**Château gonflable AES-GCM [[Gist]](https://gist.github.com/jbtule/4336842#file-aesgcm-cs)**

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

[1] : http://en.wikipedia.org/wiki/Galois/Counter_Mode
[2] : http://en.wikipedia.org/wiki/Advanced_Encryption_Standard
[3] : http://en.wikipedia.org/wiki/HMAC
[4] : http://crypto.stackexchange.com/a/205/1934

## Introduction au chiffrement symétrique et asymétrique
Vous pouvez améliorer la sécurité du transit ou du stockage des données en mettant en œuvre des techniques de cryptage. Fondamentalement, il existe deux approches lors de l'utilisation de *System.Security.Cryptography* : **symétrique** et **asymétrique.**

----------

## [**Cryptage symétrique**][1] ##

Cette méthode utilise une clé privée pour effectuer la transformation des données.

Avantages:
- Les algorithmes symétriques consomment moins de ressources et sont plus rapides que les algorithmes asymétriques.
- La quantité de données que vous pouvez chiffrer est illimitée.


Les inconvénients:
- Le chiffrement et le déchiffrement utilisent la même clé. Quelqu'un pourra déchiffrer vos données si la clé est compromise.
- Vous pourriez vous retrouver avec de nombreuses clés secrètes différentes à gérer si vous choisissez d'utiliser une clé secrète différente pour différentes données.

Sous System.Security.Cryptography, vous avez différentes classes qui effectuent un chiffrement symétrique, elles sont connues sous le nom de [chiffrements par blocs][2] :
- [AesManaged][3] (algorithme [AES][4]).
- [AesCryptoServiceProvider][5] (algorithme [AES][4] [réclamation FIPS 140-2][6]).
- [DESCryptoServiceProvider][7] (algorithme [DES][8]).
- [RC2CryptoServiceProvider][9] (algorithme [Rivest Cipher 2][10]).
- [RijndaelManaged][11] (algorithme [AES][4]). *Remarque* : RijndaelManaged n'est **pas** [FIPS-197][12] plainte.
- [TripleDES][13] (algorithme [TripleDES][14]).



----------

## [**Cryptage asymétrique**][17] ##

Cette méthode utilise une combinaison de clés publiques et privées afin d'effectuer la transformation des données.

Avantages:
- Il utilise des clés plus grandes que les algorithmes symétriques, ils sont donc moins susceptibles d'être piratés en utilisant la force brute.
- Il est plus facile de garantir qui est capable de chiffrer et de déchiffrer les données car il repose sur deux clés (publique et privée).

Les inconvénients:
- Il y a une limite à la quantité de données que vous pouvez chiffrer. La limite est différente pour chaque algorithme et est généralement proportionnelle à la taille de clé de l'algorithme. Par exemple, un objet RSACryptoServiceProvider avec une longueur de clé de 1 024 bits ne peut chiffrer qu'un message inférieur à 128 octets.
- Les algorithmes asymétriques sont très lents par rapport aux algorithmes symétriques.

Sous System.Security.Cryptography, vous avez accès à différentes classes qui effectuent un chiffrement asymétrique :
- [DSACryptoServiceProvider][18] (algorithme [Algorithme de signature numérique][19])
- [RSACryptoServiceProvider][21] (algorithme [RSA Algorithm][20])


[1] : https://en.wikipedia.org/wiki/Symmetric-key_algorithm
[2] : https://en.wikipedia.org/wiki/Block_cipher
[3] : https://msdn.microsoft.com/en-us/library/system.security.cryptography.aesmanaged
[4] : https://en.wikipedia.org/wiki/Advanced_Encryption_Standard
[5] : https://msdn.microsoft.com/en-us/library/system.security.cryptography.aescryptoserviceprovider
[6] : https://blogs.msdn.microsoft.com/winsdk/2010/05/28/behaviour-of-aescryptoserviceprovider-class-with-fips-policy-set-unset/
[7] : https://msdn.microsoft.com/en-us/library/system.security.cryptography.descryptoserviceprovider
[8] : https://en.wikipedia.org/wiki/Data_Encryption_Standard
[9] : https://msdn.microsoft.com/en-us/library/system.security.cryptography.rc2cryptoserviceprovider
[10] : https://en.wikipedia.org/wiki/RC2
[11] : https://msdn.microsoft.com/en-us/library/system.security.cryptography.rijndaelmanaged
[12] : https://blogs.msdn.microsoft.com/shawnfa/2006/10/09/the-differences-between-rijndael-and-aes/
[13] : https://msdn.microsoft.com/en-us/library/system.security.cryptography.tripledes
[14] : https://en.wikipedia.org/wiki/Triple_DES
[15] : https://msdn.microsoft.com/EN-US/library/system.security.cryptography.aesmanaged
[16] : https://msdn.microsoft.com/en-us/library/system.security.cryptography.symmetricalgorithm.iv
[17] : https://en.wikipedia.org/wiki/Public-key_cryptography
[18] : https://msdn.microsoft.com/en-us/library/system.security.cryptography.dsacryptoserviceprovider.aspx
[19] : https://en.wikipedia.org/wiki/Digital_Signature_Algorithm
[20] : https://en.wikipedia.org/wiki/RSA_(cryptosystème)
[21] : https://msdn.microsoft.com/en-us/library/system.security.cryptography.rsacryptoserviceprovider.aspx

## Hachage du mot de passe
Les mots de passe ne doivent jamais être stockés en texte brut ! Ils doivent être hachés avec un sel généré aléatoirement (pour se défendre contre les attaques de table arc-en-ciel) en utilisant un algorithme de hachage de mot de passe lent. Un nombre élevé d'itérations (> 10k) peut être utilisé pour ralentir les attaques par force brute. Un délai d'environ 100 ms est acceptable pour un utilisateur qui se connecte, mais rend difficile la rupture d'un long mot de passe. Lorsque vous choisissez un nombre d'itérations, vous devez utiliser la valeur maximale tolérable pour votre application et l'augmenter à mesure que les performances de l'ordinateur s'améliorent. Vous devrez également envisager d'arrêter les requêtes répétées qui pourraient être utilisées comme attaque DoS.

Lorsque le hachage pour la première fois d'un sel peut être généré pour vous, le hachage et le sel résultants peuvent ensuite être stockés dans un fichier.


    private void firstHash(string userName, string userPassword, int numberOfItterations)
    {
        Rfc2898DeriveBytes PBKDF2 = new Rfc2898DeriveBytes(userPassword, 8, numberOfItterations);    //Hash the password with a 8 byte salt
        byte[] hashedPassword = PBKDF2.GetBytes(20);    //Returns a 20 byte hash
        byte[] salt = PBKDF2.Salt;
        writeHashToFile(userName, hashedPassword, salt, numberOfItterations); //Store the hashed password with the salt and number of itterations to check against future password entries
    }

Vérification du mot de passe d'un utilisateur existant, lecture de son hachage et sel à partir d'un fichier et comparaison avec le hachage du mot de passe saisi

    private bool checkPassword(string userName, string userPassword, int numberOfItterations)
    {
        byte[] usersHash = getUserHashFromFile(userName);
        byte[] userSalt = getUserSaltFromFile(userName);
        Rfc2898DeriveBytes PBKDF2 = new Rfc2898DeriveBytes(userPassword, userSalt, numberOfItterations);    //Hash the password with the users salt
        byte[] hashedPassword = PBKDF2.GetBytes(20);    //Returns a 20 byte hash            
        bool passwordsMach = comparePasswords(usersHash, hashedPassword);    //Compares byte arrays
        return passwordsMach;
    }


## Cryptage de fichier symétrique simple
L'exemple de code suivant illustre un moyen simple et rapide de chiffrer et de déchiffrer des fichiers à l'aide de l'algorithme de chiffrement symétrique AES.

Le code génère de manière aléatoire les vecteurs de sel et d'initialisation chaque fois qu'un fichier est chiffré, ce qui signifie que le chiffrement du même fichier avec le même mot de passe conduira toujours à une sortie différente. Le sel et l'IV sont écrits dans le fichier de sortie afin que seul le mot de passe soit requis pour le déchiffrer.

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

## Données aléatoires sécurisées cryptographiquement
Il y a des moments où la classe Random() du framework peut ne pas être considérée comme suffisamment aléatoire, étant donné qu'elle est basée sur un générateur de nombres pseudo-aléatoires. Les classes Crypto du framework fournissent cependant quelque chose de plus robuste sous la forme de RNGCryptoServiceProvider.

Les exemples de code suivants montrent comment générer des tableaux d'octets, des chaînes et des nombres cryptographiquement sécurisés.

** Tableau d'octets aléatoires **

    public static byte[] GenerateRandomData(int length)
    {
        var rnd = new byte[length];
        using (var rng = new RNGCryptoServiceProvider())
            rng.GetBytes(rnd);
        return rnd;
    }

**Entier aléatoire** (avec distribution paire)

    public static int GenerateRandomInt(int minVal=0, int maxVal=100)
    {
        var rnd = new byte[4];
        using (var rng = new RNGCryptoServiceProvider())
            rng.GetBytes(rnd);
        var i = Math.Abs(BitConverter.ToInt32(rnd, 0));
        return Convert.ToInt32(i % (maxVal - minVal + 1) + minVal);
    }

**Chaîne aléatoire**

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




## Cryptage asymétrique rapide des fichiers
Le chiffrement asymétrique est souvent considéré comme préférable au chiffrement symétrique pour le transfert de messages à d'autres parties. Ceci est principalement dû au fait qu'il annule de nombreux risques liés à l'échange d'une clé partagée et garantit que si toute personne disposant de la clé publique peut chiffrer un message pour le destinataire prévu, seul ce destinataire peut le déchiffrer. Malheureusement, le principal inconvénient des algorithmes de chiffrement asymétriques est qu'ils sont nettement plus lents que leurs cousins ​​symétriques. En tant que tel, le chiffrement asymétrique des fichiers, en particulier des fichiers volumineux, peut souvent être un processus très gourmand en calculs.

Afin de fournir à la fois sécurité ET performances, une approche hybride peut être adoptée. Cela implique la génération cryptographiquement aléatoire d'une clé et d'un vecteur d'initialisation pour le chiffrement * symétrique *. Ces valeurs sont ensuite chiffrées à l'aide d'un algorithme *Asymétrique* et écrites dans le fichier de sortie, avant d'être utilisées pour chiffrer les données source *Symétriquement* et de les ajouter à la sortie.

Cette approche offre un haut degré de performance et de sécurité, dans la mesure où les données sont cryptées à l'aide d'un algorithme symétrique (rapide) et la clé et iv, toutes deux générées de manière aléatoire (sécurisées) sont cryptées par un algorithme asymétrique (sécurisé). Il présente également l'avantage supplémentaire que la même charge utile chiffrée à différentes occasions aura un texte chiffré très différent, car les clés symétriques sont générées de manière aléatoire à chaque fois.

La classe suivante illustre le chiffrement asymétrique des chaînes et des tableaux d'octets, ainsi que le chiffrement de fichiers hybride.

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

Exemple d'utilisation :

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

