---
title: "Caesar cipher"
slug: "caesar-cipher"
draft: false
images: []
weight: 9976
type: docs
toc: true
---

It is the simple shift monoalphabetic classical cipher where each letter is replaced by a letter 3 position (actual Caesar cipher) ahead using the circular alphabetic ordering i.e. letter after Z is A. So when we encode HELLO WORLD, the cipher text becomes KHOORZRUOG. 


## Introduction
The Caesar cipher is a classic encryption method. It works by shifting the characters by a certain amount. For example, if we choose a shift of 3, A will become D and E will become H.

The following text has been encrypted using a 23 shift.

    THE QUICK BROWN FOX JUMPS OVER THE LAZY DOG
    QEB NRFZH YOLTK CLU GRJMP LSBO QEB IXWV ALD

## A Java implementation for Caesar Cipher
Implementation of the Caesar cipher. 

 - This implementation performs the shift operation only on upper and lower case alphabets and retains the other characters (such as space as-is).
 - The Caesar cipher is not secure as per current standards.
 - Below example is for illustrative purposes only !
 - Reference: [https://en.wikipedia.org/wiki/Caesar_cipher](https://en.wikipedia.org/wiki/Caesar_cipher)

---

<!-- language: lang-java -->

    package com.example.so.cipher;
    
    /**
     * Implementation of the Caesar cipher.
     * <p>
     * <ul>
     * <li>This implementation performs the shift operation only on upper and lower
     * case alphabets and retains the other characters (such as space as-is).</li>
     * <li>The Caesar cipher is not secure as per current standards.</li>
     * <li>Below example is for illustrative purposes only !</li>
     * <li>Reference: https://en.wikipedia.org/wiki/Caesar_cipher</li>
     * </ul>
     * </p>
     * 
     * @author Ravindra HV
     * @author Maarten Bodewes (beautification only)
     * @since 2016-11-21
     * @version 0.3
     *
     */
    public class CaesarCipher {
    
        public static final char START_LOWER_CASE_ALPHABET = 'a'; // ASCII-97
        public static final char END_LOWER_CASE_ALPHABET = 'z';   // ASCII-122
    
        public static final char START_UPPER_CASE_ALPHABET = 'A'; // ASCII-65
        public static final char END_UPPER_CASE_ALPHABET = 'Z';   // ASCII-90
        
        public static final int ALPHABET_SIZE = 'Z' - 'A' + 1;    // 26 of course
    
        /**
         * Performs a single encrypt followed by a single decrypt of the Caesar
         * cipher, prints out the intermediate values and finally validates
         * that the decrypted plaintext is identical to the original plaintext.
         * 
         * <p>
         * This method outputs the following:
         * 
         * <pre>
         * Plaintext  : The quick brown fox jumps over the lazy dog
         * Ciphertext : Qeb nrfzh yoltk clu grjmp lsbo qeb ixwv ald
         * Decrypted  : The quick brown fox jumps over the lazy dog
         * Successful decryption: true
         * </pre>
         * </p>
         * 
         * @param args (ignored)
         */
        public static void main(String[] args) {
    
            int shift = 23;
            String plainText = "The quick brown fox jumps over the lazy dog";
            
            System.out.println("Plaintext  : " + plainText);
     
            String ciphertext = caesarCipherEncrypt(plainText, shift);
            System.out.println("Ciphertext : " + ciphertext);
            
            String decrypted = caesarCipherDecrypt(ciphertext, shift);
            System.out.println("Decrypted  : " + decrypted);
            System.out.println("Successful decryption: "
                    + decrypted.equals(plainText));
        }
    
        public static String caesarCipherEncrypt(String plaintext, int shift) {
            return caesarCipher(plaintext, shift, true);
        }
    
        public static String caesarCipherDecrypt(String ciphertext, int shift) {
            return caesarCipher(ciphertext, shift, false);
        }
    
        private static String caesarCipher(
                String input, int shift, boolean encrypt) {
    
            // create an output buffer of the same size as the input
            StringBuilder output = new StringBuilder(input.length());
    
            for (int i = 0; i < input.length(); i++) {
                
                // get the next character
                char inputChar = input.charAt(i);
                
                // calculate the shift depending on whether to encrypt or decrypt
                int calculatedShift = (encrypt) ? shift : (ALPHABET_SIZE - shift);
    
                char startOfAlphabet;
                if ((inputChar >= START_LOWER_CASE_ALPHABET)
                        && (inputChar <= END_LOWER_CASE_ALPHABET)) {
                    
                    // process lower case
                    startOfAlphabet = START_LOWER_CASE_ALPHABET;
                } else if ((inputChar >= START_UPPER_CASE_ALPHABET)
                        && (inputChar <= END_UPPER_CASE_ALPHABET)) {
                    
                    // process upper case
                    startOfAlphabet = START_UPPER_CASE_ALPHABET;
                } else {
                    
                    // retain all other characters
                    output.append(inputChar);
                    
                    // and continue with the next character
                    continue;
                }                
                    
                // index the input character in the alphabet with 0 as base
                int inputCharIndex =
                        inputChar - startOfAlphabet;
    
                // cipher / decipher operation (rotation uses remainder operation)
                int outputCharIndex =
                        (inputCharIndex + calculatedShift) % ALPHABET_SIZE;
    
                // convert the new index in the alphabet to an output character
                char outputChar =
                        (char) (outputCharIndex + startOfAlphabet);
    
                // add character to temporary-storage
                output.append(outputChar);
            }
    
            return output.toString();
        }
    }

---

Program Output:

<!-- language: lang-none -->

    Plaintext  : The quick brown fox jumps over the lazy dog
    Ciphertext : Qeb nrfzh yoltk clu grjmp lsbo qeb ixwv ald
    Decrypted  : The quick brown fox jumps over the lazy dog
    Successful decryption: true







## ROT13
ROT13 is a special case of Caesar cipher, with a 13 shift. Only letters are changed, and white-space and special characters are left as they are.

What is interesting is that ROT13 is a reciprocal cipher : applying ROT13 twice will give you the initial input. Indeed, 2 * 13 = 26, the number of letters in the alphabet.

---

As ROT13 doesn't have a key as input parameter it is often seen more as a *encoding algorithm* or, more specifically, an *obfuscation algorithm* rather than a cipher.

ROT13 just makes it hard to read messages directly and is therefore often used for offensive messages or puns of jokes. It doesn't provide any computational security.

## Python implementation
The following code example implements the Caesar cipher and shows the properties of the cipher.

It handles both uppercase and lowercase alpha-numerical characters, leaving all other characters as they were.

The following properties of the Caesar cipher are shown:

 - weak keys;
 - low key space;
 - the fact that each key has a reciprocal (inverse) key;
 - the relation with ROT13;

it also shows the following - more generic - cryptographic notions:

 - weak keys;
 - the difference between obfuscation (without a key) and encryption;
 - brute forcing a key;
 - the missing integrity of the ciphertext.

---

<!-- language: lang-python -->

    def caesarEncrypt(plaintext, shift):
        return caesarCipher(True, plaintext, shift)
    
    def caesarDecrypt(ciphertext, shift):
        return caesarCipher(False, ciphertext, shift)
    
    def caesarCipher(encrypt, text, shift):
        if not shift in range(0, 25):
            raise Exception('Key value out of range')
    
        output = ""
        for c in text:
            # only encrypt alphanumerical characters
            if c.isalpha():
                # we want to shift both upper- and lowercase characters
                ci = ord('A') if c.isupper() else ord('a')
                
                # if not encrypting, we're decrypting
                if encrypt:
                    output += caesarEncryptCharacter(c, ci, shift)
                else:
                    output += caesarDecryptCharacter(c, ci, shift)
            else:
                # leave other characters such as digits and spaces
                output += c
        return output
    
    def caesarEncryptCharacter(plaintextCharacter, positionOfAlphabet, shift):
        # convert character to the (zero-based) index in the alphabet
        n = ord(plaintextCharacter) - positionOfAlphabet
        # perform the >positive< modular shift operation on the index
        # this always returns a value within the range [0, 25]
        # (note that 26 is the size of the western alphabet)
        x = (n + shift) % 26 # <- the magic happens here
        # convert the index back into a character
        ctc = chr(x + positionOfAlphabet)
        # return the result
        return ctc
    
    def caesarDecryptCharacter(plaintextCharacter, positionOfAlphabet, shift):
        # convert character to the (zero-based) index in the alphabet 
        n = ord(plaintextCharacter) - positionOfAlphabet
        # perform the >negative< modular shift operation on the index
        x = (n - shift) % 26
        # convert the index back into a character
        ctc = chr(x + positionOfAlphabet)
        # return the result
        return ctc
    
    def encryptDecrypt(): 
        print '--- Run normal encryption / decryption'
        plaintext = 'Hello world!'
        key = 3 # the original value for the Caesar cipher
        ciphertext = caesarEncrypt(plaintext, key)
        print ciphertext
        decryptedPlaintext = caesarDecrypt(ciphertext, key)
        print decryptedPlaintext
    encryptDecrypt()
    
    print '=== Now lets show some cryptographic properties of the Caesar cipher'
    
    def withWeakKey():
        print '--- Encrypting plaintext with a weak key is not a good idea'
        plaintext = 'Hello world!'
        # This is the weakest key of all, it does nothing
        weakKey = 0
        ciphertext = caesarEncrypt(plaintext, weakKey)
        print ciphertext # just prints out the plaintext
    withWeakKey();
        
    def withoutDecrypt():
        print '--- Do we actually need caesarDecrypt at all?'
        plaintext = 'Hello world!'
        key = 3 # the original value for the Caesar cipher
        ciphertext = caesarEncrypt(plaintext, key)
        print ciphertext
        decryptionKey = 26 - key; # reciprocal value
        decryptedPlaintext = caesarEncrypt(ciphertext, decryptionKey)
        print decryptedPlaintext # performed decryption
    withoutDecrypt()
        
    
    def punnify():
        print '--- ROT 13 is the Caesar cipher with a given, reciprocal, weak key: 13'
        # The key is weak because double encryption will return the plaintext
        def rot13(pun):
            return caesarEncrypt(pun, 13)
    
        print 'Q: How many marketing people does it take to change a light bulb?' 
        obfuscated = 'N: V jvyy unir gb trg onpx gb lbh ba gung.'
        print obfuscated
        deobfuscated = rot13(obfuscated)
        print deobfuscated
        # We should not leak the pun, right? Lets obfuscate afterwards!
        obfuscatedAgain = rot13(deobfuscated)
        print obfuscatedAgain
    punnify()
    
    def bruteForceAndLength():
        print '--- Brute forcing is very easy as there are only 25 keys in the range [1..25]'
        # Note that AES-128 has 340,282,366,920,938,463,463,374,607,431,768,211,456 keys
        # and is therefore impossible to bruteforce (if the key is correctly generated)
        key = 10;
        plaintextToFind = 'Hello Maarten!'
        ciphertextToBruteForce = caesarEncrypt(plaintextToFind, key)
        for candidateKey in range(1, 25):
            bruteForcedPlaintext = caesarDecrypt(ciphertextToBruteForce, candidateKey)
            # lets assume the adversary knows 'Hello', but not the name
            if bruteForcedPlaintext.startswith('Hello'):
                print 'key value: ' + str(candidateKey) + ' gives : ' + bruteForcedPlaintext
    
        print '--- Length of plaintext usually not hidden'
        # Side channel attacks on ciphertext lengths are commonplace! Beware!
        if len(ciphertextToBruteForce) != len('Hello Stefan!'):
            print 'The name is not Stefan (but could be Stephan)'
    bruteForceAndLength()
    
    def manInTheMiddle():
        print '--- Ciphers are vulnerable to man-in-the-middle attacks'
        # Hint: do not directly use a cipher for transport security
        moneyTransfer = 'Give Maarten one euro'
        key = 1
        print moneyTransfer
        encryptedMoneyTransfer = caesarEncrypt(moneyTransfer, key)
        print encryptedMoneyTransfer
        # Man in the middle replaces third word with educated guess
        # (or tries different ciphertexts until success)
        encryptedMoneyTransferWords  = encryptedMoneyTransfer.split(' ');
        encryptedMoneyTransferWords[2] = 'ufo' # unidentified financial object
        modifiedEncryptedMoneyTransfer = ' '.join(encryptedMoneyTransferWords)
        print modifiedEncryptedMoneyTransfer
        decryptedMoneyTransfer = caesarDecrypt(modifiedEncryptedMoneyTransfer, key)
        print decryptedMoneyTransfer
    manInTheMiddle()



## Python implementation
The ASCII way
===================

This shifts the characters but doesn't care if the new character is not a letter. This is good if you want to use punctuation or special characters, but it won't necessarily give you letters only as an output. For example, "z" 3-shifts to "}".

<!-- language: lang-python -->

    def ceasar(text, shift):
        output = ""
        for c in text:
            output += chr(ord(c) + shift)
        return output

