---
title: "Getting started with java.util.scanner"
slug: "getting-started-with-javautilscanner"
draft: false
images: []
weight: 1
type: docs
toc: true
---

## Installation or Setup
The java.util.Scanner class is a simple text scanner which can parse primitive types and strings using regular expressions. A Scanner breaks its input into tokens using a delimiter, which by default matches whitespace.

java.util.Scanner is part of the Java API, and is therefore included by default with each Java installation.

To use Scanner in your code, you first need to specify where it is in Java's library: Scanner is in the package `java.util`.

The easy way is to add this line at the top of your file:

    import java.util.Scanner;

When the code compiles, "Scanner" will refer to that class.
If you want to use another class also named Scanner, you can specify each usage individually, although this is can get cumbersome:

    import java.util.Scanner;

    //Code not shown

    public int exampleMethod()
    {
        Scanner keyboardInput = new Scanner(System.in);        //Scans character input.
        int orcasPresent = myPackage.Scanner.scanForOrcas();   //<<Demonstration Here>>
        return keyboardInput.nextInt() * orcasPresent;
    }

(In this case, `scanForOrcas()` is a static method of your custom class, `myPackage.Scanner`. `myPackage.Scanner` must be in the folder myPackage and contain the line `package myPackage;`)

