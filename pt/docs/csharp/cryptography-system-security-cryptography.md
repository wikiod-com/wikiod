---
title: "Criptografia (Sistema.Segurança.Criptografia)"
slug: "criptografia-sistemasegurancacriptografia"
draft: false
images: []
weight: 9908
type: docs
toc: true
---

## Exemplos modernos de criptografia autenticada simétrica de uma string
Criptografia é algo muito difícil e depois de passar muito tempo lendo diferentes exemplos e vendo como é fácil introduzir alguma forma de vulnerabilidade encontrei uma resposta originalmente escrita por @jbtule que acho muito boa. Gostar de ler:

"A prática recomendada geral para criptografia simétrica é usar a criptografia autenticada com dados associados (AEAD), no entanto, isso não faz parte das bibliotecas de criptografia .net padrão. Portanto, o primeiro exemplo usa [AES256][2] e depois [HMAC256 ][3], a dois passos [Encrypt then MAC][4], que requer mais overhead e mais chaves.

O segundo exemplo usa a prática mais simples de AES256-[GCM][1] usando o Bouncy Castle de código aberto (via nuget).

Ambos os exemplos têm uma função principal que recebe string de mensagem secreta, chave(s) e uma carga útil não secreta opcional e retorna e string criptografada autenticada opcionalmente prefixada com os dados não secretos. Idealmente, você os usaria com chaves de 256 bits geradas aleatoriamente, veja `NewKey()`.

Ambos os exemplos também possuem métodos auxiliares que usam uma senha de string para gerar as chaves. Esses métodos auxiliares são fornecidos como uma conveniência para combinar com outros exemplos, no entanto, eles são *muito menos seguros* porque a força da senha será *muito mais fraca que uma chave de 256 bits*.

**Atualizar:**
Adicionadas sobrecargas de `byte[]`, e apenas o [Gist](https://gist.github.com/4336842) tem a formatação completa com recuo de 4 espaços e documentos de API devido aos limites de resposta do StackOverflow."

----------
** Criptografia integrada do .NET (AES)-Then-MAC (HMAC) [[Gist]](https://gist.github.com/jbtule/4336842#file-aesthenhmac-cs)**

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
**Castelo inflável AES-GCM [[Gist]](https://gist.github.com/jbtule/4336842#file-aesgcm-cs)**

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

## Introdução à criptografia simétrica e assimétrica
Você pode melhorar a segurança do trânsito ou armazenamento de dados implementando técnicas de criptografia. Basicamente, há duas abordagens ao usar *System.Security.Cryptography*: **simétrico** e **assimétrico.**

----------

## [**Criptografia Simétrica**][1] ##

Este método usa uma chave privada para realizar a transformação de dados.

Prós:
- Algoritmos simétricos consomem menos recursos e são mais rápidos que os assimétricos.
- A quantidade de dados que você pode criptografar é ilimitada.


Contras:
- Criptografia e descriptografia usam a mesma chave. Alguém poderá descriptografar seus dados se a chave estiver comprometida.
- Você pode acabar com muitas chaves secretas diferentes para gerenciar se optar por usar uma chave secreta diferente para dados diferentes.

Em System.Security.Cryptography você tem diferentes classes que realizam criptografia simétrica, elas são conhecidas como [block ciphers][2]:
- [AesManaged][3] (algoritmo [AES][4]).
- [AesCryptoServiceProvider][5] ([AES][4] algoritmo [reclamação FIPS 140-2][6]).
- [DESCryptoServiceProvider][7] (algoritmo [DES][8]).
- [RC2CryptoServiceProvider][9] (algoritmo [Rivest Cipher 2][10]).
- [RijndaelManaged][11] (algoritmo [AES][4]). *Observação*: RijndaelManaged **não** é uma reclamação [FIPS-197][12].
- [TripleDES][13] (algoritmo [TripleDES][14]).



----------

## [**Criptografia Assimétrica**][17] ##

Este método usa uma combinação de chaves públicas e privadas para realizar a transformação de dados.

Prós:
- Ele usa chaves maiores do que algoritmos simétricos, portanto, eles são menos suscetíveis a serem quebrados usando força bruta.
- É mais fácil garantir quem é capaz de criptografar e descriptografar os dados porque depende de duas chaves (pública e privada).

Contras:
- Há um limite na quantidade de dados que você pode criptografar. O limite é diferente para cada algoritmo e normalmente é proporcional ao tamanho da chave do algoritmo. Por exemplo, um objeto RSACryptoServiceProvider com um comprimento de chave de 1.024 bits só pode criptografar uma mensagem menor que 128 bytes.
- Algoritmos assimétricos são muito lentos em comparação com algoritmos simétricos.

Em System.Security.Cryptography você tem acesso a diferentes classes que realizam criptografia assimétrica:
- [DSACryptoServiceProvider][18] (algoritmo [Algoritmo de Assinatura Digital][19])
- [RSACryptoServiceProvider][21] (algoritmo [RSA Algorithm][20])


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
[12]: https://blogs.msdn.microsoft.com/shawnfa/2006/10/09/the-differences-between-rijndael-and-aes/
[13]: https://msdn.microsoft.com/en-us/library/system.security.cryptography.tripledes
[14]: https://en.wikipedia.org/wiki/Triple_DES
[15]: https://msdn.microsoft.com/EN-US/library/system.security.cryptography.aesmanaged
[16]: https://msdn.microsoft.com/en-us/library/system.security.cryptography.symmetricalgorithm.iv
[17]: https://en.wikipedia.org/wiki/Public-key_cryptography
[18]: https://msdn.microsoft.com/en-us/library/system.security.cryptography.dsacryptoserviceprovider.aspx
[19]: https://en.wikipedia.org/wiki/Digital_Signature_Algorithm
[20]: https://en.wikipedia.org/wiki/RSA_(cryptosystem)
[21]: https://msdn.microsoft.com/en-us/library/system.security.cryptography.rsacryptoserviceprovider.aspx

## Hash de senha
As senhas nunca devem ser armazenadas como texto simples! Eles devem ser hash com um sal gerado aleatoriamente (para se defender contra ataques de tabela de arco-íris) usando um algoritmo de hash de senha lento. Um grande número de iterações (> 10k) pode ser usado para desacelerar ataques de força bruta. Um atraso de aproximadamente 100 ms é aceitável para um usuário efetuar login, mas dificulta a quebra de uma senha longa. Ao escolher um número de iterações, você deve usar o valor máximo tolerável para seu aplicativo e aumentá-lo à medida que o desempenho do computador melhorar. Você também precisará considerar interromper solicitações repetidas que podem ser usadas como um ataque DoS.

Ao fazer o hash pela primeira vez, um sal pode ser gerado para você, o hash e o sal resultantes podem ser armazenados em um arquivo.


    private void firstHash(string userName, string userPassword, int numberOfItterations)
    {
        Rfc2898DeriveBytes PBKDF2 = new Rfc2898DeriveBytes(userPassword, 8, numberOfItterations);    //Hash the password with a 8 byte salt
        byte[] hashedPassword = PBKDF2.GetBytes(20);    //Returns a 20 byte hash
        byte[] salt = PBKDF2.Salt;
        writeHashToFile(userName, hashedPassword, salt, numberOfItterations); //Store the hashed password with the salt and number of itterations to check against future password entries
    }

Verificando uma senha de usuário existente, leia seu hash e salt de um arquivo e compare com o hash da senha inserida

    private bool checkPassword(string userName, string userPassword, int numberOfItterations)
    {
        byte[] usersHash = getUserHashFromFile(userName);
        byte[] userSalt = getUserSaltFromFile(userName);
        Rfc2898DeriveBytes PBKDF2 = new Rfc2898DeriveBytes(userPassword, userSalt, numberOfItterations);    //Hash the password with the users salt
        byte[] hashedPassword = PBKDF2.GetBytes(20);    //Returns a 20 byte hash            
        bool passwordsMach = comparePasswords(usersHash, hashedPassword);    //Compares byte arrays
        return passwordsMach;
    }


## Criptografia de arquivo simétrica simples
O exemplo de código a seguir demonstra um meio rápido e fácil de criptografar e descriptografar arquivos usando o algoritmo de criptografia simétrica AES.

O código gera aleatoriamente os Vetores Salt e de Inicialização cada vez que um arquivo é criptografado, o que significa que criptografar o mesmo arquivo com a mesma senha sempre levará a uma saída diferente. O salt e o IV são gravados no arquivo de saída para que apenas a senha seja necessária para descriptografá-lo.

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

## Dados aleatórios criptograficamente seguros
Há momentos em que a classe Random() do framework pode não ser considerada aleatória o suficiente, já que é baseada em um gerador de números pseudo-aleatórios. As classes Crypto da estrutura, no entanto, fornecem algo mais robusto na forma de RNGCryptoServiceProvider.

Os exemplos de código a seguir demonstram como gerar matrizes, strings e números de bytes criptograficamente seguros.

**Matriz de bytes aleatórios**

    public static byte[] GenerateRandomData(int length)
    {
        var rnd = new byte[length];
        using (var rng = new RNGCryptoServiceProvider())
            rng.GetBytes(rnd);
        return rnd;
    }

**Random Integer** (com distribuição uniforme)

    public static int GenerateRandomInt(int minVal=0, int maxVal=100)
    {
        var rnd = new byte[4];
        using (var rng = new RNGCryptoServiceProvider())
            rng.GetBytes(rnd);
        var i = Math.Abs(BitConverter.ToInt32(rnd, 0));
        return Convert.ToInt32(i % (maxVal - minVal + 1) + minVal);
    }

**String Aleatória**

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




## Criptografia de arquivo assimétrica rápida
A criptografia assimétrica é frequentemente considerada preferível à criptografia simétrica para transferir mensagens para outras partes. Isso ocorre principalmente porque nega muitos dos riscos relacionados à troca de uma chave compartilhada e garante que, embora qualquer pessoa com a chave pública possa criptografar uma mensagem para o destinatário pretendido, apenas esse destinatário pode descriptografá-la. Infelizmente, a principal desvantagem dos algoritmos de criptografia assimétrica é que eles são significativamente mais lentos do que seus primos simétricos. Como tal, a criptografia assimétrica de arquivos, especialmente os grandes, muitas vezes pode ser um processo computacionalmente intensivo.

Para fornecer segurança E desempenho, uma abordagem híbrida pode ser adotada. Isso envolve a geração criptograficamente aleatória de uma chave e vetor de inicialização para criptografia *Simétrica*. Esses valores são então criptografados usando um algoritmo *Assimétrico* e gravados no arquivo de saída, antes de serem usados ​​para criptografar os dados de origem *Simetricamente* e anexá-los à saída.

Essa abordagem oferece um alto grau de desempenho e segurança, pois os dados são criptografados usando um algoritmo simétrico (rápido) e a chave e iv, ambos gerados aleatoriamente (seguro) são criptografados por um algoritmo assimétrico (seguro). Ele também tem a vantagem adicional de que a mesma carga útil criptografada em diferentes ocasiões terá um texto cifrado muito diferente, porque as chaves simétricas são geradas aleatoriamente a cada vez.

A classe a seguir demonstra a criptografia assimétrica de cadeias de caracteres e matrizes de bytes, bem como a criptografia de arquivo híbrida.

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

Exemplo de uso:

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

