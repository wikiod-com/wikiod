---
title: "Caesar cipher"
slug: "caesar-cipher"
draft: false
images: []
weight: 9992
type: docs
toc: true
---

A Caesar cipher is one of the simplest and most widely known encryption techniques. The names comes from Julius Caesar, who, according to Suetonius, used it with a shift of three to protect messages of military significance

## Encryption
Encyption happens by replacing each letter of the alfabet with an other letter. The keys can be showed with this circle. Here is a shift of 8 chars used.

[![Encryption circle of Caesar cipher][1]][1]

Here will the A replaced by T, B by U, C by V etc. So will next string encrypted:

Original         | Encrypted       |
---------------- | --------------- |
HELLO WORLD      | AXEEH PHKEW     |

  [1]: https://i.stack.imgur.com/weg8k.png

## Decryption
Decription happens by the same cicle showed in the encryption example. Example:

Encrypted    | Original
------------ | ------------
AXEEH PHKEW  | HELLO WORLD 

## Hacking
Ceasar ciphers are easy to hack. If you know that an encrypted A is equal to H and a P is equal to I, everything with the same encryption key could be encrypted.

