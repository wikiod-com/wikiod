---
title: "Java Native Access"
slug: "java-native-access"
draft: false
images: []
weight: 9972
type: docs
toc: true
---

## Introduction to JNA
What is JNA?
============
Java Native Access (JNA) is a community-developed library providing Java programs an easy access to native shared libraries (`.dll` files on windows, `.so` files on Unix ...)


How can I use it?
============

- Firstly, download the [latest release of JNA][1] and reference its jna.jar in your project's CLASSPATH.

- Secondly, copy, compile and run the Java code below

>*For the purpose of this introduction, we suppose the native platform in use is Windows. If you're running on another platform simply replace the string `"msvcrt"` with the string `"c"` in the code below.*

The small Java program below will print a message on the console by calling the [C][2] `printf` function.

**CRuntimeLibrary.java**

    package jna.introduction;

    import com.sun.jna.Library;
    import com.sun.jna.Native;

    // We declare the printf function we need and the library containing it (msvcrt)... 
    public interface CRuntimeLibrary extends Library {
    
       CRuntimeLibrary INSTANCE =
           (CRuntimeLibrary) Native.loadLibrary("msvcrt", CRuntimeLibrary.class);
 
       void printf(String format, Object... args);
    }

**MyFirstJNAProgram.java**

    package jna.introduction;
 
    // Now we call the printf function...
    public class MyFirstJNAProgram {
        public static void main(String args[]) {
             CRuntimeLibrary.INSTANCE.printf("Hello World from JNA !");
        }
    }

Where to go now?
================
Jump into another topic here or jump to the [official site][3].


  [1]: https://mvnrepository.com/artifact/net.java.dev.jna/jna
  [2]: https://www.wikiod.com/c
  [3]: https://github.com/java-native-access/jna

