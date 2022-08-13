---
title: "Şifreleme  Kriptografi"
slug: "sifreleme--kriptografi"
draft: false
images: []
weight: 9957
type: docs
toc: true
---

.NET Framework, birçok şifreleme algoritmasının uygulanmasını sağlar. Temel olarak simetrik algoritmaları, asimetrik algoritmaları ve karmaları içerirler.

## RijndaelYönetilen
Gerekli Ad Alanı: `System.Security.Cryptography`
    
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

**Kullanım**

    var textToEncrypt = "hello World";
    
     var encrypted = new Encryption().Encrypt(textToEncrypt); //-> zBmW+FUxOvdbpOGm9Ss/vQ==
    
     var decrypted = new Encryption().Decrypt(encrypted); //-> hello World

**Not:**

- Rijndael, standart simetrik şifreleme algoritması AES'nin öncüsüdür.

## AES kullanarak verileri şifreleyin ve şifresini çözün (C# dilinde)
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

Bu örnek [MSDN](https://msdn.microsoft.com/en-us/library/system.security.cryptography.aes(v=vs.110).aspx) adresinden alınmıştır.

Standart **AES** şifrelemesini kullanarak bir dizenin nasıl şifreleneceğini ve daha sonra şifresinin nasıl çözüleceğini gösteren bir konsol demo uygulamasıdır.

(**[AES = Gelişmiş Şifreleme Standardı](https://en.wikipedia.org/wiki/Advanced_Encryption_Standard),** ABD Ulusal Standartlar ve Teknoloji Enstitüsü (NIST) tarafından elektronik verilerin şifrelenmesine yönelik bir belirtim Simetrik şifreleme için hala fiili standart olan 2001)

**Notlar:**

- Gerçek bir şifreleme senaryosunda, uygun bir şifreleme modu seçmeniz gerekir ("CipherMode" numaralandırmasından bir değer seçilerek "Mode" özelliğine atanabilir). **Asla** `CipherMode.ECB`yi (elektronik kod kitabı modu) kullanmayın, çünkü bu zayıf bir şifre akışı sağlar

- İyi (zayıf olmayan) bir "Anahtar" oluşturmak için, ya bir kriptografik rastgele oluşturucu kullanın ya da yukarıdaki örneği kullanın (**Paroladan Anahtar Oluşturun**). Önerilen **KeySize** 256 bit'tir. Desteklenen anahtar boyutları, 'LegalKeySizes' özelliği aracılığıyla kullanılabilir.

- 'IV' başlatma vektörünü başlatmak için yukarıdaki örnekte gösterildiği gibi bir TUZ kullanabilirsiniz (**Rastgele TUZ**)

- Desteklenen blok boyutları 'SupportedBlockSizes' özelliği aracılığıyla kullanılabilir, blok boyutu 'BlockSize' özelliği aracılığıyla atanabilir

**Kullanım:** Main() yöntemine bakın.


## Paroladan Anahtar Oluştur / Rastgele SALT (C# ile)
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

Bu örnek [MSDN](https://msdn.microsoft.com/en-us/library/system.security.cryptography.passwordderivebytes(v=vs.110).aspx) adresinden alınmıştır.

Bu bir konsol demosudur ve kullanıcı tanımlı bir parolaya dayalı olarak nasıl güvenli bir anahtarın oluşturulacağını ve kriptografik rasgele oluşturucuya dayalı olarak rasgele bir SALT'ın nasıl oluşturulacağını gösterir.

**Notlar:**

- Yerleşik `PasswordDeriveBytes` işlevi, şifreden bir anahtar oluşturmak için standart PBKDF1 algoritmasını kullanır. Varsayılan olarak, kaba kuvvet saldırılarını yavaşlatacak anahtarı oluşturmak için 100 yineleme kullanır. Rastgele oluşturulan SALT, anahtarı daha da güçlendirir.

- "CryptDeriveKey" işlevi, "PasswordDeriveBytes" tarafından oluşturulan anahtarı, belirtilen karma algoritmayı (burada "SHA1") kullanarak belirtilen şifreleme algoritmasıyla (burada "TripleDES") uyumlu bir anahtara dönüştürür. Bu örnekteki anahtar boyutu 192 bayttır ve başlatma vektörü IV, üçlü DES şifreleme sağlayıcısından alınmıştır.

- Genellikle, bu mekanizma, büyük miktarda veriyi şifreleyen bir parola ile rastgele oluşturulmuş daha güçlü bir anahtarı korumak için kullanılır. Aynı verilere erişim sağlamak için farklı kullanıcıların birden fazla parolasını sağlamak için de kullanabilirsiniz (farklı bir rastgele anahtarla korunur).

- Ne yazık ki, "CryptDeriveKey" şu anda AES'yi desteklemiyor. [buraya] bakın.(https://social.msdn.microsoft.com/Forums/vstudio/en-US/61d85001-2eae-4419-b4bf-ce98d46f4d21/passwordderivebytescryptderivekey-derives-an-aes-key-but-gets- nesne-tanımlayıcı-oid-is-unknown?forum=netfxbcl) <br/> **NOT:** Geçici bir çözüm olarak, AES ile korunacak verilerin şifrelenmesi için rastgele bir AES anahtarı oluşturabilir ve AES anahtarını depolayabilirsiniz. 'CryptDeriveKey' tarafından oluşturulan anahtarı kullanan bir TripleDES-Container'da. Ancak bu, güvenliği TripleDES ile sınırlar, AES'nin daha büyük anahtar boyutlarından yararlanmaz ve TripleDES'e bağımlılık yaratır.

**Kullanım:** Main() yöntemine bakın.

## Kriptografi (AES) Kullanarak Şifreleme ve Şifre Çözme
Şifre Çözme Kodu

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
Şifreleme Kodu

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

kullanım

   

    var textToEncrypt = "TestEncrypt";
    
    var encrypted = Encrypt(textToEncrypt);
    
    var decrypted = Decrypt(encrypted);

