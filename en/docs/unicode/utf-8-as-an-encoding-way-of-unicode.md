---
title: "UTF-8 as an encoding way of Unicode"
slug: "utf-8-as-an-encoding-way-of-unicode"
draft: false
images: []
weight: 9990
type: docs
toc: true
---

**What is UTF-8**?

UTF-8 is an encoding, which is variable-length and uses 8-bit code units - that's why UTF-**8**. In the internet UTF-8 is dominant encoding (before 2008 ASCII was, ehich also can handle any Unicode code point.). 

**Is UTF-8 the same as Unicode?** 

"Unicode" isn't an encoding - it is  a coded character set - i.e. a set of characters and a mapping between the characters and integer code points representing them. But a lot of documentation uses it to refer to *encodings*. On Windows, for example, the term Unicode is used to refer to UTF-16.

UTF-8 is only one of the ways to encode Unicode and as an encoding it converts the sequences of bytes to sequences of characters and vice versa. UTF-16 and -32 are other Unicode transformation formats.


 
**BOM of UTF-8**

All three mayhave a specific Byte Order Marks, which being a magic number signals several important things to a program (for example, Notepad++) - for example, the fact, that the imported text stream is Unicode; also it helps to detect the art of Unicode used for this stream. 
However the Unicode consortium recommends storing UTF-8 without any signature. Some software, for example gcc compiler complains if a file contains the UTF-8 signature. A lot of Windows programs on the other hand use the signature. And trying to detect the encoding of a stream of bytes don't always work.

**How to check if your project has UTF-8 encoding or not**

UTF-8 is yet not universal, and software engineers and data scientists often face problem of encoding of text streams. Sometimes UTF-8 is supposed to be used in the project, however another ecndoing is being used. There are several tools to detect the encoding of the file:
 
 

 - Some CMD tools, like Linux command-line tool '[file][1]' or         
   `powershell`;
 - Python package "chardet"
 - Notepad++ as maybe the most popular tool for manual check.




  [1]: http://gnuwin32.sourceforge.net/packages/file.htm
 


## How to change the default encoding of the server to UTF-8
Sometimes users from other regions than English-speaking have problems with encoding while for example programming a php project. It can be, that the server has another encoding then UTF-8, and if someone want to create a php project in UTF-8 on this server, his text might be shown incorrect.

Example: it can be that on your server default encoding is Windows-1251 - then you should delete the `AddDefaultCharset windows-1251` from the **.htaccess** server file and write `AddDefaultCharset utf-8`.

To check, which encoding does your server have, don't set the `<META charset>` tag and activate `"automatic encoding detection"` in your browser. 

## Save an Excel file in UTF-8
Excel -> Save as -> Save as type -> "Comma separated value (*.csv)" AND Tools (left to Save button) -> Web options -> Encoding -> Save this document as -> Unicode (UTF-8)

[![enter image description here][1]][1]

  [1]: http://i.stack.imgur.com/0W1GK.png

## How to convert a byte array of UTF-8 data to a Unicode string in Python
    def make_unicode(data):
        if type(data) != unicode:
            data =  data.decode('utf-8')
            return data
        else:
            return data

