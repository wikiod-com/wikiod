---
title: "Getting started with jni"
slug: "getting-started-with-jni"
draft: false
images: []
weight: 1
type: docs
toc: true
---

## Getting started with JNI
JNI means Java Native Interface. It is a mechanism how to call a native code from Java application running under JVM control and vice versa. Native code means code compiled for the target platform. Native code code is typically written in C or C++ but it may be written in any language that has compiler for the target platform.

JNI is useful when

* A java application need to access a platform specific resources and there is no java library with the required functionality. The resource may be a specific hardware, sensors or whatever.
* A java application has to do a performance critical task and native code may be faster or with less footprint than java bytecode. Nevertheless do be too self-confident JVM is able to do a lot of optimization and a naive implementation in C/C++ will be probably slower.
* An application in C/C++ (or another language) wants to use a feature provided in a java library.

To start with JNI you need

* JDK or something that is able to compile java to bytecode.
* Compiler for compiling the native code.

The following hello world example is a simple java application that calls a C function. The example can be compiled by javac from JDK and gcc C compiler.

Java code:

    public class JNIExample {

        public static void main(String[] args) {
           // The loadLibrary search for the native library (libnative.so in this case)
           System.loadLibrary("native");
           String s = "Hello JNI";
           JNIExample example = new JNIExample();
           example.doPrint(s);
       }

       // The method with native code (written in C) must be declared with native prefix
       public native void doPrint(String message);
    
    }

C code:

    #include <jni.h>
    #include <stdio.h>

    /* the function that is called from java must be declered with decorators
     * JNIEXPORT and JNICALL.
     * The function name is constructed as Java_ClassName_MethodName
     * Function parameters correspond parameters in java but there are 2 extra parameters
     * JNIEnv is a pointer to java envoronmet and jobject is a reference to caller object.
     * Caller object is the instance of the JNIExample in this case.
     */
    JNIEXPORT void JNICALL Java_JNIExample_doPrint(JNIEnv *e, jobject obj, jstring message) {
        const char *c_message;
        /* It is necessary to convert java objects like string to something C native */
        c_message = (*e)->GetStringUTFChars(e, message, NULL);
        printf("%s\n", c_message);
        /* in the end it is necessary to free resources allocated by Get above */
        (*e)->ReleaseStringUTFChars(e, message, c_message);
    }



