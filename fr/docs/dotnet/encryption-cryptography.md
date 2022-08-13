---
title: "Cryptage  Cryptographie"
slug: "cryptage--cryptographie"
draft: false
images: []
weight: 9957
type: docs
toc: true
---

.NET Framework fournit l'implémentation de nombreux algorithmes cryptographiques. Ils comprennent essentiellement des algorithmes symétriques, des algorithmes asymétriques et des hachages.

## RijndaelGéré
Espace de noms requis : `System.Security.Cryptography`
    
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

**Usage**

    var textToEncrypt = "hello World";
    
     var encrypted = new Encryption().Encrypt(textToEncrypt); //-> zBmW+FUxOvdbpOGm9Ss/vQ==
    
     var decrypted = new Encryption().Decrypt(encrypted); //-> hello World

**Noter:**

- Rijndael est le prédécesseur de l'algorithme cryptographique symétrique standard AES.

## Chiffrer et déchiffrer les données à l'aide d'AES (en C#)
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

Cet exemple provient de [MSDN](https://msdn.microsoft.com/en-us/library/system.security.cryptography.aes(v=vs.110).aspx).

Il s'agit d'une application de démonstration de console, montrant comment chiffrer une chaîne en utilisant le chiffrement **AES** standard, et comment la déchiffrer ensuite.

(**[AES = Advanced Encryption Standard](https://en.wikipedia.org/wiki/Advanced_Encryption_Standard),** une spécification pour le chiffrement des données électroniques établie par le U.S. National Institute of Standards and Technology (NIST) en 2001 qui est toujours la norme de facto pour le chiffrement symétrique)

**Remarques:**

- Dans un scénario de chiffrement réel, vous devez choisir un mode de chiffrement approprié (peut être affecté à la propriété `Mode` en sélectionnant une valeur dans l'énumération `CipherMode`). **Ne jamais** utiliser le `CipherMode.ECB` (mode livre de codes électronique), car cela produit un flux de chiffrement faible

- Pour créer une bonne (et non une faible) `Clé`, utilisez soit un générateur aléatoire cryptographique, soit l'exemple ci-dessus (**Créer une clé à partir d'un mot de passe**). La **KeySize** recommandée est de 256 bits. Les tailles de clé prises en charge sont disponibles via la propriété `LegalKeySizes`.

- Pour initialiser le vecteur d'initialisation `IV`, vous pouvez utiliser un SALT comme indiqué dans l'exemple ci-dessus (**Random SALT**)

- Les tailles de bloc prises en charge sont disponibles via la propriété `SupportedBlockSizes`, la taille de bloc peut être attribuée via la propriété `BlockSize`

**Utilisation :** voir la méthode Main().


## Créer une clé à partir d'un mot de passe / SALT aléatoire (en C#)
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

Cet exemple est tiré de [MSDN.](https://msdn.microsoft.com/en-us/library/system.security.cryptography.passwordderivebytes(v=vs.110).aspx)

Il s'agit d'une démo de console, et elle montre comment créer une clé sécurisée basée sur un mot de passe défini par l'utilisateur, et comment créer un SALT aléatoire basé sur le générateur aléatoire cryptographique.

**Remarques:**

- La fonction intégrée "PasswordDeriveBytes" utilise l'algorithme standard PBKDF1 pour générer une clé à partir du mot de passe. Par défaut, il utilise 100 itérations pour générer la clé permettant de ralentir les attaques par force brute. Le SALT généré aléatoirement renforce encore la clé.

- La fonction `CryptDeriveKey` convertit la clé générée par `PasswordDeriveBytes` en une clé compatible avec l'algorithme de chiffrement spécifié (ici "TripleDES") en utilisant l'algorithme de hachage spécifié (ici "SHA1"). La taille de clé dans cet exemple est de 192 octets et le vecteur d'initialisation IV est tiré du fournisseur de chiffrement triple-DES

- Habituellement, ce mécanisme est utilisé pour protéger une clé générée aléatoirement plus forte par un mot de passe, qui crypte une grande quantité de données. Vous pouvez également l'utiliser pour fournir plusieurs mots de passe d'utilisateurs différents pour donner accès aux mêmes données (étant protégés par une clé aléatoire différente).

- Malheureusement, `CryptDeriveKey` ne prend actuellement pas en charge AES. Voir [ici.](https://social.msdn.microsoft.com/Forums/vstudio/en-US/61d85001-2eae-4419-b4bf-ce98d46f4d21/passwordderivebytescryptderivekey-derives-an-aes-key-but-gets- object-identifier-oid-is-unknown?forum=netfxbcl) <br/> **REMARQUE :** Pour contourner le problème, vous pouvez créer une clé AES aléatoire pour le chiffrement des données à protéger avec AES et stocker la clé AES dans un TripleDES-Container qui utilise la clé générée par `CryptDeriveKey`. Mais cela limite la sécurité à TripleDES, ne tire pas parti des tailles de clé plus importantes d'AES et crée une dépendance à TripleDES.

**Utilisation :** Voir la méthode Main().

## Chiffrement et déchiffrement à l'aide de la cryptographie (AES)
Code de déchiffrement

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
Code de cryptage

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

Usage

   

    var textToEncrypt = "TestEncrypt";
    
    var encrypted = Encrypt(textToEncrypt);
    
    var decrypted = Decrypt(encrypted);

