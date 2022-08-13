---
title: "Kriptografi (System.Security.Cryptography)"
slug: "kriptografi-systemsecuritycryptography"
draft: false
images: []
weight: 9908
type: docs
toc: true
---

## Bir dizenin Simetrik Kimliği Doğrulanmış Şifrelemesinin Modern Örnekleri
Kriptografi çok zor bir şeydir ve farklı örnekler okumak için çok zaman harcadıktan ve bir tür güvenlik açığı oluşturmanın ne kadar kolay olduğunu gördükten sonra, orijinal olarak @jbtule tarafından yazılmış bir cevap buldum ve bence çok iyi. Okumanın tadını çıkar:

"Simetrik şifreleme için genel en iyi uygulama, İlişkili Verilerle (AEAD) Doğrulanmış Şifreleme kullanmaktır, ancak bu standart .net kripto kitaplıklarının bir parçası değildir. Dolayısıyla ilk örnekte [AES256][2] ve ardından [HMAC256" kullanılır. ][3], daha fazla ek yük ve daha fazla anahtar gerektiren iki adımlı bir [Şifrele sonra MAC][4].

İkinci örnek, açık kaynak Bouncy Castle (nuget aracılığıyla) kullanarak AES256-[GCM][1]'in daha basit uygulamasını kullanır.

Her iki örnekte de gizli mesaj dizileri, anahtar(lar) ve isteğe bağlı gizli olmayan bir veri yükü alan ve isteğe bağlı olarak gizli olmayan verilerle birlikte geri dönen ve kimliği doğrulanmış şifreli dize alan bir ana işlev vardır. İdeal olarak, bunları rastgele oluşturulmuş 256 bit anahtar(lar)la kullanırsınız, bkz. `NewKey()`.

Her iki örnek de, anahtarları oluşturmak için bir dize parolası kullanan yardımcı yöntemlere sahiptir. Bu yardımcı yöntemler, diğer örneklerle eşleştirmek için kolaylık sağlamak için sağlanmıştır, ancak parolanın gücü *256 bitlik bir anahtardan* çok daha zayıf olacağından *çok daha az güvenlidir*.

**Güncelleme:**
"Bayt[]" aşırı yüklemeleri eklendi ve StackOverflow yanıt sınırları nedeniyle yalnızca [Gist](https://gist.github.com/4336842) 4 boşluk girintisi ve API belgeleriyle tam biçimlendirmeye sahip."

----------
**.NET Yerleşik Şifrele(AES)-Then-MAC(HMAC) [[Gist]](https://gist.github.com/jbtule/4336842#file-aesthenhmac-cs)**

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
**Bouncy Castle AES-GCM [[Gist]](https://Gist.github.com/jbtule/4336842#file-aesgcm-cs)**

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

## Simetrik ve Asimetrik Şifrelemeye Giriş
Şifreleme tekniklerini uygulayarak veri aktarımı veya depolama güvenliğini artırabilirsiniz. *System.Security.Cryptography*: **simetrik** ve **asimetrik** kullanılırken temel olarak iki yaklaşım vardır.

----------

## [**Simetrik Şifreleme**][1] ##

Bu yöntem, veri dönüşümünü gerçekleştirmek için özel bir anahtar kullanır.

Artıları:
- Simetrik algoritmalar daha az kaynak tüketir ve asimetrik algoritmalardan daha hızlıdır.
- Şifreleyebileceğiniz veri miktarı sınırsızdır.


Eksileri:
- Şifreleme ve şifre çözme aynı anahtarı kullanır. Anahtarın güvenliği ihlal edilirse, birisi verilerinizin şifresini çözebilir.
- Farklı veriler için farklı bir gizli anahtar kullanmayı seçerseniz, yönetmek için birçok farklı gizli anahtar elde edebilirsiniz.

System.Security.Cryptography altında simetrik şifreleme gerçekleştiren farklı sınıflarınız vardır, bunlar [blok şifreleri][2] olarak bilinir:
- [AesManaged][3] ([AES][4] algoritması).
- [AesCryptoServiceProvider][5] ([AES][4] algoritması [FIPS 140-2 şikayeti][6]).
- [DESCryptoServiceProvider][7] ([DES][8] algoritması).
- [RC2CryptoServiceProvider][9] ([Rivest Cipher 2][10] algoritması).
- [RijndaelManaged][11] ([AES][4] algoritması). *Not*: RijndaelManaged **değil** [FIPS-197][12] şikayet.
- [TripleDES][13] ([TripleDES][14] algoritması).



----------

## [**Asimetrik Şifreleme**][17] ##

Bu yöntem, veri dönüşümünü gerçekleştirmek için genel ve özel anahtarların bir kombinasyonunu kullanır.

Artıları:
- Simetrik algoritmalardan daha büyük anahtarlar kullanır, bu nedenle kaba kuvvet kullanılarak kırılmaya karşı daha az hassastırlar.
- İki anahtara (genel ve özel) dayandığından, verileri kimin şifreleyebileceğini ve şifresini çözebileceğini garanti etmek daha kolaydır.

Eksileri:
- Şifreleyebileceğiniz veri miktarında bir sınır vardır. Sınır, her algoritma için farklıdır ve tipik olarak algoritmanın anahtar boyutuyla orantılıdır. Örneğin, 1.024 bit anahtar uzunluğuna sahip bir RSACryptoServiceProvider nesnesi yalnızca 128 bayttan küçük bir iletiyi şifreleyebilir.
- Asimetrik algoritmalar, simetrik algoritmalara kıyasla çok yavaştır.

System.Security.Cryptography altında, asimetrik şifreleme gerçekleştiren farklı sınıflara erişebilirsiniz:
- [DSACryptoServiceProvider][18] ([Dijital İmza Algoritması][19] algoritması)
- [RSACryptoServiceProvider][21] ([RSA Algoritması][20] algoritması)


[1]: https://en.wikipedia.org/wiki/Smetric-key_algorithm
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
[16]: https://msdn.microsoft.com/en-us/library/system.security.cryptography.smetricalgorithm.iv
[17]: https://en.wikipedia.org/wiki/Public-key_cryptography
[18]: https://msdn.microsoft.com/en-us/library/system.security.cryptography.dsacryptoserviceprovider.aspx
[19]: https://en.wikipedia.org/wiki/Digital_Signature_Algorithm
[20]: https://en.wikipedia.org/wiki/RSA_(kriptosistem)
[21]: https://msdn.microsoft.com/en-us/library/system.security.cryptography.rsacryptoserviceprovider.aspx

## Şifre Karması
Şifreler asla düz metin olarak saklanmamalıdır! Yavaş bir parola karma algoritması kullanılarak rastgele oluşturulmuş bir tuzla (gökkuşağı tablosu saldırılarına karşı savunmak için) karma oluşturulmalıdırlar. Kaba kuvvet saldırılarını yavaşlatmak için çok sayıda yineleme (> 10k) kullanılabilir. Bir kullanıcının oturum açması için ~100ms'lik bir gecikme kabul edilebilir, ancak uzun bir parolayı kırmayı zorlaştırır. Bir dizi yineleme seçerken, uygulamanız için izin verilen maksimum değeri kullanmalı ve bilgisayar performansı arttıkça bunu artırmalısınız. Ayrıca bir DoS saldırısı olarak kullanılabilecek tekrarlanan istekleri durdurmayı da düşünmeniz gerekecektir.

Karma ilk kez sizin için bir tuz oluşturulabilir, elde edilen karma ve tuz daha sonra bir dosyada saklanabilir.


    private void firstHash(string userName, string userPassword, int numberOfItterations)
    {
        Rfc2898DeriveBytes PBKDF2 = new Rfc2898DeriveBytes(userPassword, 8, numberOfItterations);    //Hash the password with a 8 byte salt
        byte[] hashedPassword = PBKDF2.GetBytes(20);    //Returns a 20 byte hash
        byte[] salt = PBKDF2.Salt;
        writeHashToFile(userName, hashedPassword, salt, numberOfItterations); //Store the hashed password with the salt and number of itterations to check against future password entries
    }

Mevcut bir kullanıcı şifresini kontrol etme, bir dosyadan hash ve tuzlarını okuma ve girilen şifrenin hash'i ile karşılaştırma

    private bool checkPassword(string userName, string userPassword, int numberOfItterations)
    {
        byte[] usersHash = getUserHashFromFile(userName);
        byte[] userSalt = getUserSaltFromFile(userName);
        Rfc2898DeriveBytes PBKDF2 = new Rfc2898DeriveBytes(userPassword, userSalt, numberOfItterations);    //Hash the password with the users salt
        byte[] hashedPassword = PBKDF2.GetBytes(20);    //Returns a 20 byte hash            
        bool passwordsMach = comparePasswords(usersHash, hashedPassword);    //Compares byte arrays
        return passwordsMach;
    }


## Basit Simetrik Dosya Şifreleme
Aşağıdaki kod örneği, AES simetrik şifreleme algoritmasını kullanarak dosyaları şifrelemenin ve şifresini çözmenin hızlı ve kolay bir yolunu gösterir.

Kod, bir dosya her şifrelendiğinde rasgele Tuz ve Başlatma Vektörlerini oluşturur; bu, aynı dosyayı aynı parolayla şifrelemenin her zaman farklı çıktılara yol açacağı anlamına gelir. Salt ve IV çıktı dosyasına yazılır, böylece şifreyi çözmek için yalnızca parola gerekir.

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

## Rastgele Verileri Kriptografik Olarak Güvenli Hale Getirin
Çerçevenin Random() sınıfının, bir psuedo-rastgele sayı üretecine dayandığı göz önüne alındığında, yeterince rastgele olarak kabul edilemeyeceği zamanlar vardır. Ancak çerçevenin Crypto sınıfları, RNGCryptoServiceProvider biçiminde daha sağlam bir şey sağlar.

Aşağıdaki kod örnekleri, Kriptografik Olarak Güvenli bayt dizilerinin, dizelerinin ve sayıların nasıl oluşturulacağını gösterir.

**Rastgele Bayt Dizisi**

    public static byte[] GenerateRandomData(int length)
    {
        var rnd = new byte[length];
        using (var rng = new RNGCryptoServiceProvider())
            rng.GetBytes(rnd);
        return rnd;
    }

**Rastgele Tamsayı** (eşit dağılımlı)

    public static int GenerateRandomInt(int minVal=0, int maxVal=100)
    {
        var rnd = new byte[4];
        using (var rng = new RNGCryptoServiceProvider())
            rng.GetBytes(rnd);
        var i = Math.Abs(BitConverter.ToInt32(rnd, 0));
        return Convert.ToInt32(i % (maxVal - minVal + 1) + minVal);
    }

**Rastgele Dizi**

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




## Hızlı Asimetrik Dosya Şifreleme
Asimetrik şifreleme, mesajları diğer taraflara aktarmak için genellikle Simetrik şifrelemeye tercih edilir olarak kabul edilir. Bunun temel nedeni, paylaşılan bir anahtarın değiş tokuşuyla ilgili birçok riski ortadan kaldırması ve açık anahtara sahip herkesin bir mesajı hedeflenen alıcı için şifreleyebilmesine rağmen, yalnızca bu alıcının şifresini çözebilmesini sağlamasıdır. Ne yazık ki, asimetrik şifreleme algoritmalarının en büyük dezavantajı, simetrik kuzenlerinden önemli ölçüde daha yavaş olmalarıdır. Bu nedenle, özellikle büyük dosyaların asimetrik şifrelenmesi, genellikle hesaplama açısından çok yoğun bir süreç olabilir.

Hem güvenlik hem de performans sağlamak için hibrit bir yaklaşım benimsenebilir. Bu, *Simetrik* şifreleme için bir anahtarın ve başlatma vektörünün kriptografik olarak rastgele oluşturulmasını gerektirir. Bu değerler daha sonra bir *Asimetrik* algoritması kullanılarak şifrelenir ve kaynak verileri *Simetrik* olarak şifrelemek ve çıktıya eklemek için kullanılmadan önce çıktı dosyasına yazılır.

Bu yaklaşım, verilerin simetrik bir algoritma (hızlı) kullanılarak şifrelenmesi ve her ikisi de rastgele oluşturulmuş (güvenli) olan anahtar ve iv'nin asimetrik bir algoritma (güvenli) ile şifrelenmesi bakımından yüksek derecede performans ve güvenlik sağlar. Simetrik anahtarlar her seferinde rastgele oluşturulduğundan, farklı durumlarda şifrelenen aynı yükün çok farklı şifreli metne sahip olması gibi ek bir avantajı da vardır.

Aşağıdaki sınıf, dizelerin ve bayt dizilerinin asimetrik şifrelemesinin yanı sıra karma dosya şifrelemesini gösterir.

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

Kullanım örneği:

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

