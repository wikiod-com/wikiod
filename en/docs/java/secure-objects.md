---
title: "Secure objects"
slug: "secure-objects"
draft: false
images: []
weight: 9956
type: docs
toc: true
---

## Syntax
 - SealedObject sealedObject = new SealedObject(obj, cipher);
 - SignedObject signedObject = new SignedObject(obj, signingKey,
           signingEngine);



## SealedObject (javax.crypto.SealedObject)
This class enables a programmer to create an object and protect its confidentiality with a cryptographic algorithm. 

Given any Serializable object, one can create a **SealedObject** that encapsulates the original object, in serialized format (i.e., a "deep copy"), and seals (encrypts) its serialized contents, using a cryptographic algorithm such as AES, DES, to protect its confidentiality. The encrypted content can later be decrypted (with the corresponding algorithm using the correct decryption key) and de-serialized, yielding the original object. 

[![enter image description here][1]][1]



    Serializable obj = new String("John");
    // Generate key
    KeyGenerator kgen = KeyGenerator.getInstance("AES");
    kgen.init(128);
    SecretKey aesKey = kgen.generateKey();
    Cipher cipher = Cipher.getInstance("AES");
    cipher.init(Cipher.ENCRYPT_MODE, aesKey);
    SealedObject sealedObject = new SealedObject(obj, cipher);
    System.out.println("sealedObject-" + sealedObject);
    System.out.println("sealedObject Data-" + sealedObject.getObject(aesKey));


  [1]: http://i.stack.imgur.com/Pz2NR.png

## SignedObject (java.security.SignedObject)
SignedObject is a class for the purpose of creating authentic runtime objects whose integrity cannot be compromised without being detected. 

More specifically, a SignedObject contains another Serializable object, the (to-be-)signed object and its signature. 

[![enter image description here][1]][1]


    

    //Create a key
    KeyPairGenerator keyGen = KeyPairGenerator.getInstance("DSA", "SUN");
    SecureRandom random = SecureRandom.getInstance("SHA1PRNG", "SUN");
    keyGen.initialize(1024, random);
    // create a private key
    PrivateKey signingKey = keyGen.generateKeyPair().getPrivate();
    // create a Signature
    Signature signingEngine = Signature.getInstance("DSA");
    signingEngine.initSign(signingKey);
    // create a simple object 
    Serializable obj = new String("John");
    // sign our object
    SignedObject signedObject = new SignedObject(obj, signingKey, signingEngine);

    System.out.println("signedObject-" + signedObject);
    System.out.println("signedObject Data-" + signedObject.getObject());


  [1]: http://i.stack.imgur.com/nSE3Z.png

