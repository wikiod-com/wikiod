---
title: "Getting started with cryptography"
slug: "getting-started-with-cryptography"
draft: false
images: []
weight: 1
type: docs
toc: true
---

## Introduction
Cryptography is the science of using mathematical constructs (codes) to make communication secure. The field of cryptography is a subset of the field of Information Security.

There are many cryptographic operations possible; some best known examples are:

 - **Encryption :** transforming a plaintext message into a ciphertext message so that the the message remains *confidential*
 - **Decryption :** transforming a ciphertext message back into a plaintext message
 - **Secure hashing :** performing irreversible (one-way) compression to create a statically sized, computationally distinct representation for a specific message.

Cryptography is based on math, and arithmetic is frequently used in algorithms related to cryptography. There is a small subset of primitives, schemes and protocols that are used by developers. Developers usually do not implement the algorithms themselves but use the schemes and protocols provided by cryptographic API's and runtimes.

A **primitive** could be a block cipher such as AES. A primitive is any algorithm that is used as building block for a cryptographic scheme. A **scheme** is for instance a block cipher *mode of operation* such as CBC or GCM. One or more cryptographic schemes can make up a cryptographic protocol. A **protocol** such as TLS uses many cryptographic schemes, but also message encoding / decoding techniques, message ordering, conditions for use etc. Low level cryptographic API's just provide direct access to primitives while high level API's may offer access to full protocol implementations.

Messages have been encrypted and decrypted by hand since written word was invented. Mechanical devices have been used at least since ancient Greek society. This kind of cryptography is referred to as *classic cryptography*. Many introductions to cryptography start with classic cryptography as it is relatively easy to analyze. Classic algorithms however do not adhere to the security required from modern constructs and are often easy to break. Examples of classic schemes are the Caesar and Vigenère. The most well known mechanical device is undoubtedly the Enigma coding machine.

Modern cryptography is based on science - mainly math and number/group theory. It involves much more intricate algorithms and key sizes. These can only be handled efficiently by computing devices. For this reason modern cryptography mainly uses byte oriented input and output. This means that messages need to be converted to binary and back before they can be transformed by any implementation of a cryptographic algorithm. This means that (textual) messages need to be transformed using character-encoding before being encrypted.

Forms of *character-encoding* of textual messages is UTF-8. Structured messages may be encoded using ASN.1 / DER or *canonical* XML representations - or any number of proprietary techniques. Sometimes the binary output needs to be transformed back into text as well. In this case an *encoding* scheme such as base 64 or hexadecimals can be used to represent the binary data within text.

Cryptography is notoriously hard to get right. Developers should only use constructs that they fully understand. If possible, a developer should use a higher level protocol such as TLS to create transport security. There is no practical chance of creating a secure algorithm, scheme or protocol without formal education or extensive experience. Copy/pasting examples from the internet is not likely lead to secure solutions and may even result in data loss.


## Integrity Validated - Symmetric Key - Encryption and Decryption example using Java
<p> Encryption is used to transform data in its orignal format (Eg: The contents of a letter, Credentials part of authorizing a financial transaction) to something that cannot be easily reconstructed by anyone who is not intended to be part of the conversation.  </p>
<p> Basically encryption is used to prevent eavesdropping between any two entities (individuals or a group). </p> 

<p> In case of symmetric encryption, both the sender and receiver (Eg: Alice, Bob) must use the same encryption algorithm (generally a standardised one) and the same encryption key (known only to the two of them). </p>

<p> http://docs.oracle.com/javase/1.5.0/docs/guide/security/jce/JCERefGuide.html#Examples </p>

<p> Related Links </p>
<ul>
    <li>https://en.wikipedia.org/wiki/History_of_cryptography</li>
    <li>https://en.wikipedia.org/wiki/Cryptography</li>
</ul>

    package com.example.so.documentation.cryptography;
    
    import java.nio.charset.Charset;
    import java.security.InvalidAlgorithmParameterException;
    import java.security.InvalidKeyException;
    import java.security.NoSuchAlgorithmException;
    import java.security.spec.AlgorithmParameterSpec;
    import java.util.StringTokenizer;
    
    import javax.crypto.BadPaddingException;
    import javax.crypto.Cipher;
    import javax.crypto.IllegalBlockSizeException;
    import javax.crypto.KeyGenerator;
    import javax.crypto.Mac;
    import javax.crypto.NoSuchPaddingException;
    import javax.crypto.SecretKey;
    import javax.crypto.spec.IvParameterSpec;
    import javax.crypto.spec.SecretKeySpec;
    import javax.xml.bind.DatatypeConverter;
    
    
    /**
     * 
     * <p> Encryption is used to transform data in its orignal format (Eg: The contents of a letter, Credentials part of authorizing a financial transaction) to something that 
     * cannot be easily reconstructed by anyone who is not intended to be part of the conversation.  </p>
     * <p> Basically encryption is used to prevent eavesdropping between any two entities 
     * (individuals or a group). </p> 
     * 
     * <p> In case of symmetric encryption, both the sender and receiver (Eg: Alice, Bob) must use the same encryption algorithm (generally a standardised one) 
     * and the same encryption key (known only to the two of them). </p>
     * 
     * <p> http://docs.oracle.com/javase/1.5.0/docs/guide/security/jce/JCERefGuide.html#Examples </p>
     * 
     * <p> Related Links </p>
     * <ul>
     *     <li>https://en.wikipedia.org/wiki/History_of_cryptography</li>
     *     <li>https://en.wikipedia.org/wiki/Cryptography</li>
     * </ul>
     * 
     * <pre>
     *         ChangeLog : 2016-09-24
     *         1. The modified encrypted text is now reflected correctly in the log and also updated same in javadoc comment.
     * </pre>
     * @author Ravindra HV (with inputs w.r.t integrity check from ArtjomB[http://stackoverflow.com/users/1816580/artjom-b])
     * @since (30 July 2016)
     * @version 0.3
     *
     */
    public class IntegrityValidatedSymmetricCipherExample {
        
        /**
         * <p>https://en.wikipedia.org/wiki/Advanced_Encryption_Standard</p>
         */
        private static final String SYMMETRIC_ENCRYPTION_ALGORITHM_NAME = "AES"; // The current standard encryption algorithm (as of writing)
        
        /**
         * <p>Higher the number, the better</p>
         * <p>Encryption is performed on chunks of data defined by the key size</p>
         * <p>Higher key sizes may require modification to the JDK (Unlimited Strength Cryptography)</p>
         *         
         */
        private static final int SYMMETRIC_ENCRYPTION_KEY_SIZE = 128; // lengths can be 128, 192 and 256
        
        /**
         * <p> 
         *         A transformation defines in what manner the encryption should be performed.
         * </p>
         * <p>
                Eg: Whether there is any link between two chunks of encrypted data (CBC) or what should happen 
         *         if there is a mismatch between the key-size and the data length.       * 
         * </p>
         * 
         * <p> https://en.wikipedia.org/wiki/Block_cipher_mode_of_operation </p>
         */
        private static final String SYMMETRIC_ENCRYPTION_TRANSFORMATION = "AES/CBC/PKCS5Padding";
        private static final Charset CHARSET_INSTANCE_UTF8 = Charset.forName("UTF-8");
        
        
        private static final int AES_IV_KEY_SIZE = 128; // for AES, iv key size is fixed at 128 independent of key-size
        
        private static final String MAC_ALGORITHM_NAME__HMAC_SHA256 = "HmacSHA256";
        private static final String HASH_FIELDS_SEPARATOR = "|" ;
        
        
        
    
        /**
         * @param args
         * <p>Sample output.</p>
         * <pre>
    Encrypted, Base64 encoded text :W1DePjeYMlI6xmyq9jr+cw==|55F80F4C2987CC143C69563025FACE22|GLR3T8GdcocpsTM1qSXp5jLsNx6QRK880BtgnV1jFg0=
    Decrypted text :helloworld
    Encrypted, Base64 encoded text - v2:1XX/A9BO1Cp8mK+SHh9iHA==|B8294AC9967BB57D714ACCB3EE5710BD|TnjdaWbvp+H6yCbAAQFMkWNixeW8VwmW48YlKA/AAyw=
    Decrypted text  - v2:helloworld
    Encrypted, Base64 encoded text - v3 (original):EU4+rAZ2vOKtoSDiDPcO+A==|AEEB8DD341D8D9CD2EDFA05A4595EBD2|7anESSSJf1dHobS5tDdQ1mCNkFcIgCvtNC/p79xJi5U=
    Encrypted, Base64 encoded text - v3 (modified):FU4+rAZ2vOKtoSDiDPcO+A==|AEEB8DD341D8D9CD2EDFA05A4595EBD2|7anESSSJf1dHobS5tDdQ1mCNkFcIgCvtNC/p79xJi5U=
    Error : Integrity check failed
    Exception in thread "main" java.lang.RuntimeException: Error : Integrity check failed
        at com.example.so.documentation.cryptography.IntegrityValidatedSymmetricCipherExampleThree.decrypt(IntegrityValidatedSymmetricCipherExampleThree.java:165)
        at com.example.so.documentation.cryptography.IntegrityValidatedSymmetricCipherExampleThree.main(IntegrityValidatedSymmetricCipherExampleThree.java:126)
         * </pre>
         */
        public static void main(String[] args) {
            
            /*
             * EncryptionKey : Shared secret between receiver and sender (who generates the password and how its shared depends on the purpose)
             * This program generates a new one every time its run ! 
             * Normally it would be generated once and then be stored somewhere (Eg: In a JCEKS keystore file).
             */
            byte[] generatedSharedSecret = secretKeyGeneratorUtility();
            byte[] generatedSharedHMACKey = secretKeyGeneratorUtility();
            String plainText = "helloworld";
            
            String encryptedText = encrypt(plainText, generatedSharedSecret, generatedSharedHMACKey);
            System.out.println("Encrypted, Base64 encoded text :"+encryptedText);
            String decryptedText = decrypt(encryptedText, generatedSharedSecret, generatedSharedHMACKey);
            System.out.println("Decrypted text :"+decryptedText);
            
            String encryptedTextTwo = encrypt(plainText, generatedSharedSecret, generatedSharedHMACKey);
            System.out.println("Encrypted, Base64 encoded text - v2:"+encryptedTextTwo);
            String decryptedTextTwo = decrypt(encryptedTextTwo, generatedSharedSecret, generatedSharedHMACKey);
            System.out.println("Decrypted text  - v2:"+decryptedTextTwo);
            
            String encryptedTextThree = encrypt(plainText, generatedSharedSecret, generatedSharedHMACKey);
            System.out.println("Encrypted, Base64 encoded text - v3 (original):"+encryptedTextThree);
            char[] encryptedTextThreeChars = encryptedTextThree.toCharArray();
            encryptedTextThreeChars[0] = (char) ((encryptedTextThreeChars[0])+1);
            String encryptedTextThreeModified = new String(encryptedTextThreeChars);
            System.out.println("Encrypted, Base64 encoded text - v3 (modified):"+encryptedTextThreeModified);
            
            String decryptedTextThree = decrypt(encryptedTextThreeModified, generatedSharedSecret, generatedSharedHMACKey);
            System.out.println("Decrypted text  - v3:"+decryptedTextThree);
    
        }
        
        
        public static String encrypt(String plainText, byte[] key, byte[] hmacKey) {
            
            byte[] plainDataBytes = plainText.getBytes(CHARSET_INSTANCE_UTF8);
            byte[] iv = initializationVectorGeneratorUtility();
            byte[] encryptedDataBytes = encrypt(plainDataBytes, key, iv);
            
            String initializationVectorHex = DatatypeConverter.printHexBinary(iv);
            String encryptedBase64EncodedString = DatatypeConverter.printBase64Binary(encryptedDataBytes); // Generally the encrypted data is encoded in Base64 or hexadecimal encoding for ease of handling.
            String hashInputString = encryptedBase64EncodedString + HASH_FIELDS_SEPARATOR + initializationVectorHex + HASH_FIELDS_SEPARATOR;
            String hashedOutputString =  DatatypeConverter.printBase64Binary(messageHashWithKey(hmacKey, hashInputString.getBytes(CHARSET_INSTANCE_UTF8)));
            String encryptionResult = hashInputString + hashedOutputString;  
            return encryptionResult;
        }
        
        public static byte[] encrypt(byte[] plainDataBytes, byte[] key, byte[] iv) {
            byte[] encryptedDataBytes = encryptOrDecrypt(plainDataBytes, key, iv, true);
            return encryptedDataBytes;
        }
    
        
        public static String decrypt(String cipherInput, byte[] key, byte[] hmacKey) {
            StringTokenizer stringTokenizer = new StringTokenizer(cipherInput, HASH_FIELDS_SEPARATOR);
            
            String encryptedString = stringTokenizer.nextToken();
            String initializationVectorHex = stringTokenizer.nextToken();
            String hashedString = stringTokenizer.nextToken();
    
            String hashInputString = encryptedString + HASH_FIELDS_SEPARATOR + initializationVectorHex + HASH_FIELDS_SEPARATOR;
            String hashedOutputString =  DatatypeConverter.printBase64Binary(messageHashWithKey(hmacKey, hashInputString.getBytes(CHARSET_INSTANCE_UTF8)));
    
            if( hashedString.equals(hashedOutputString) == false ) {
                String message = "Error : Integrity check failed";
                System.out.println(message);
                throw new RuntimeException(message);
            }
            
            byte[] encryptedDataBytes = DatatypeConverter.parseBase64Binary(encryptedString); // The Base64 encoding must be reversed so as to reconstruct the raw bytes.
            byte[] iv = DatatypeConverter.parseHexBinary(initializationVectorHex);
            byte[] plainDataBytes = decrypt(encryptedDataBytes, key, iv);
            String plainText = new String(plainDataBytes, CHARSET_INSTANCE_UTF8);
            return plainText;
        }
    
        public static byte[] decrypt(byte[] encryptedDataBytes, byte[] key, byte[] iv) {
            byte[] decryptedDataBytes = encryptOrDecrypt(encryptedDataBytes, key, iv, false);
            return decryptedDataBytes;
        }
        
    
        public static byte[] encryptOrDecrypt(byte[] inputDataBytes, byte[] key, byte[] iv, boolean encrypt) {
            byte[] resultDataBytes = null;
            
            // Exceptions, if any, are just logged to console for this example.
            try {
                Cipher cipher = Cipher.getInstance(SYMMETRIC_ENCRYPTION_TRANSFORMATION);
                SecretKey secretKey = new SecretKeySpec(key, SYMMETRIC_ENCRYPTION_ALGORITHM_NAME);
                AlgorithmParameterSpec algorithmParameterSpec = new IvParameterSpec(iv);
                if(encrypt) {
                    cipher.init(Cipher.ENCRYPT_MODE, secretKey, algorithmParameterSpec);    
                }
                else {
                    cipher.init(Cipher.DECRYPT_MODE, secretKey, algorithmParameterSpec);
                }
                
                resultDataBytes = cipher.doFinal(inputDataBytes); // In relative terms, invoking do-final in one go is fine as long as the input size is small.
            } catch (NoSuchAlgorithmException e) {
                e.printStackTrace();
            } catch (NoSuchPaddingException e) {
                e.printStackTrace();
            } catch (InvalidKeyException e) {
                e.printStackTrace();
            } catch (IllegalBlockSizeException e) {
                e.printStackTrace();
            } catch (BadPaddingException e) {
                e.printStackTrace();
            } catch (InvalidAlgorithmParameterException e) {
                e.printStackTrace();
            }
            
            return resultDataBytes;
        }
    
        
        private static byte[] secretKeyGeneratorUtility() {
            byte[] keyBytes = null;
            
            try {
                KeyGenerator keyGenerator = KeyGenerator.getInstance(SYMMETRIC_ENCRYPTION_ALGORITHM_NAME);
                keyGenerator.init(SYMMETRIC_ENCRYPTION_KEY_SIZE);
                SecretKey secretKey = keyGenerator.generateKey();
                keyBytes = secretKey.getEncoded();
            } catch (NoSuchAlgorithmException e) {
                e.printStackTrace();
            }
            
            return keyBytes;
            
        }
        
        
        /**
         * <p> InitialVector : Helps in avoiding generating the same encrypted result, even when the same encryption - algorithm and key are used. </p>
         * <p> Since this is also required to be known to both sender and receiver, its either based on some convention or is part of the cipher-text transmitted.</p>  
         * <p> https://en.wikipedia.org/wiki/Initialization_vector </p>
         * @return
         */
        private static byte[] initializationVectorGeneratorUtility() {
            byte[] initialVectorResult = null;
            
            try {
                KeyGenerator keyGenerator = KeyGenerator.getInstance(SYMMETRIC_ENCRYPTION_ALGORITHM_NAME);
                keyGenerator.init(AES_IV_KEY_SIZE);
                SecretKey secretKey = keyGenerator.generateKey();
                initialVectorResult = secretKey.getEncoded();
            } catch (NoSuchAlgorithmException e) {
                e.printStackTrace();
            }
            
            return initialVectorResult;
        }
        
        
        private static byte[] messageHashWithKey(byte[] key, byte[] data) { // byte[] iv, 
            byte[] hmac = null;
            
            try {
                Mac mac = Mac.getInstance(MAC_ALGORITHM_NAME__HMAC_SHA256);
                SecretKeySpec secretKeySpec = new SecretKeySpec(key, MAC_ALGORITHM_NAME__HMAC_SHA256);
                //AlgorithmParameterSpec algorithmParameterSpec = new IvParameterSpec(iv);
                mac.init(secretKeySpec); // algorithmParameterSpec
                hmac = mac.doFinal(data);
            } catch (NoSuchAlgorithmException e) {
                e.printStackTrace();
            } catch (InvalidKeyException e) {
                e.printStackTrace();
            } /*catch (InvalidAlgorithmParameterException e) {
                e.printStackTrace();
            }*/
            
            return hmac;
            
        }
    
    }



