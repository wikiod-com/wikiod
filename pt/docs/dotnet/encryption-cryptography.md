---
title: "Criptografia  Criptografia"
slug: "criptografia--criptografia"
draft: false
images: []
weight: 9957
type: docs
toc: true
---

O .NET Framework fornece implementação de muitos algoritmos criptográficos. Eles incluem algoritmos basicamente simétricos, algoritmos assimétricos e hashes.

## RijndaelGerenciado
Namespace necessário: `System.Security.Cryptography`
    
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

**Observação:**

- Rijndael é o predecessor do algoritmo criptográfico simétrico padrão AES.

## Criptografe e descriptografe dados usando AES (em C#)
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

Este exemplo é do [MSDN](https://msdn.microsoft.com/en-us/library/system.security.cryptography.aes(v=vs.110).aspx).

É um aplicativo de demonstração de console, mostrando como criptografar uma string usando a criptografia padrão **AES** e como descriptografá-la posteriormente.

(**[AES = Advanced Encryption Standard](https://en.wikipedia.org/wiki/Advanced_Encryption_Standard),** uma especificação para a criptografia de dados eletrônicos estabelecida pelo U.S. National Institute of Standards and Technology (NIST) em 2001, que ainda é o padrão de fato para criptografia simétrica)

**Notas:**

- Em um cenário de criptografia real, você precisa escolher um modo de codificação adequado (pode ser atribuído à propriedade `Mode` selecionando um valor da enumeração `CipherMode`). **Nunca** use o `CipherMode.ECB` (modo de livro de código eletrônico), pois isso produz um fluxo de criptografia fraco

- Para criar uma boa (e não uma fraca) `Key`, use um gerador aleatório criptográfico ou use o exemplo acima (**Create a Key from a Password**). O **KeySize** recomendado é de 256 bits. Os tamanhos de chave suportados estão disponíveis por meio da propriedade `LegalKeySizes`.

- Para inicializar o vetor de inicialização `IV`, você pode usar um SALT como mostrado no exemplo acima (**Random SALT**)

- Os tamanhos de bloco suportados estão disponíveis através da propriedade `SupportedBlockSizes`, o tamanho do bloco pode ser atribuído através da propriedade `BlockSize`

**Uso:** veja o método Main().


## Crie uma chave a partir de uma senha / SALT aleatório (em C#)
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

Este exemplo foi retirado de [MSDN.](https://msdn.microsoft.com/en-us/library/system.security.cryptography.passwordderivebytes(v=vs.110).aspx)

É uma demonstração de console e mostra como criar uma chave segura com base em uma senha definida pelo usuário e como criar um SALT aleatório com base no gerador aleatório criptográfico.

**Notas:**

- A função interna `PasswordDeriveBytes` usa o algoritmo padrão PBKDF1 para gerar uma chave a partir da senha. Por padrão, ele usa 100 iterações para gerar a chave para desacelerar os ataques de força bruta. O SALT gerado aleatoriamente fortalece ainda mais a chave.

- A função `CryptDeriveKey` converte a chave gerada por `PasswordDeriveBytes` em uma chave compatível com o algoritmo de criptografia especificado (aqui "TripleDES") usando o algoritmo de hash especificado (aqui "SHA1"). O tamanho da chave neste exemplo é de 192 bytes e o vetor de inicialização IV é obtido do provedor de criptografia DES triplo

- Normalmente, esse mecanismo é usado para proteger uma chave gerada aleatoriamente mais forte por uma senha, que criptografa uma grande quantidade de dados. Você também pode usá-lo para fornecer várias senhas de usuários diferentes para dar acesso aos mesmos dados (sendo protegidos por uma chave aleatória diferente).

- Infelizmente, `CryptDeriveKey` atualmente não suporta AES. Consulte [aqui.](https://social.msdn.microsoft.com/Forums/vstudio/en-US/61d85001-2eae-4419-b4bf-ce98d46f4d21/passwordderivebytescryptderivekey-derives-an-aes-key-but-gets- object-identifier-oid-is-unknown?forum=netfxbcl) <br/> **OBSERVAÇÃO:** como solução alternativa, você pode criar uma chave AES aleatória para criptografia dos dados a serem protegidos com AES e armazenar a chave AES em um TripleDES-Container que usa a chave gerada por `CryptDeriveKey`. Mas isso limita a segurança ao TripleDES, não aproveita os tamanhos de chave maiores do AES e cria uma dependência do TripleDES.

**Uso:** veja o método Main().

## Criptografia e descriptografia usando criptografia (AES)
Código de descriptografia

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
Código de criptografia

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

