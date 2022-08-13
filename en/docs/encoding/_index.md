---
title : encoding Tutorial
slug : encoding-tutorial
weight : 9987
draft : false
images : []
type : docs
---

**What is an encoding and how it works?** 

A computer can't store letters or anything else - it stores bits. Bit can be either 0 or 1 ("yes"/"no", "true"/"false" - these formats are called binary therefore). To use these bits some rules are required, to convert the bits into some content. There rules are called *encodings*, where sequences of 1/0 bits stand for certain characters. A sequence of 8 bits is called *byte*.

Encodings work like tables, where each character is related to a specific byte. To encode something in ASCII encoding, one should follow the entries from right to left, searching for bits related to characters. To decode a string of bits into characters, one substitutes bits for letters from left to right.

Bytes can be represented in different formats: for example `10011111` in binary is `237` in octal, `159` in decimal and `9F` in hexadecimal formats. 

**What is the difference between different encodings?** 

First character encoding like ASCII from the pre-8-bit era used only 7 bits from 8. ASCII was used to encode English language with all the 26 letters in lower und upper case form, numbers and plenty of punctuation signs. ASCII could not cover other European languages with all the `ö-ß-é-å` letters - so encodings were developed that used the 8-th bit of a byte to cover another 128 characters. 

But one byte is not enough to represent languages with more than 256 characters - for example Chinese. Using two bytes (16 bits) enables encoding of 65,536 distinct values. Such encodings as BIG-5 separate a string of bits into blocks of 16 bits (2 bytes) to encode characters. Multi-byte encodings have the advantage to be space-efficient, but the downside that operations such as finding substrings, comparisons, etc. all have to decode the characters to unicode code points before such operations can be performed (there are some shortcuts, though).

Another type of encoding are such with variable number of bytes per character - such as UTF standards. These standards have some unit size, which for [UTF-8][1] is 8 bits, for UTF-16 is 16 bits, and for UTF-32 is 32 bits. And then the standard defines some of the bits as flags: if they're set, then the next unit in a sequence of units is to be considered part of the same character. If they're not set, this unit represents only one character fully (for example English occupies only one byte, and thats why ASCII encoding maps fully to UTF-8).


**What is the Unicode?**

[Unicode][2] if a huge character set (saying in a more understandable way - a table) with 1,114,112 code points, each of them stands for specific letter, symbol or another character. Using Unicode, you can write a document which contains theoretically any language used by people. 

*Unicode is not an encoding - it is a set of code points.* And there are several ways to encode Unicode code points into bits - such as UTF-8, -16 and -32.


  [1]: https://www.wikiod.com/unicode/utf-8-as-an-encoding-way-of-unicode
  [2]: https://www.wikiod.com/unicode

