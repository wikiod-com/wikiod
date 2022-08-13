---
title: "StringBuffer"
slug: "stringbuffer"
draft: false
images: []
weight: 9990
type: docs
toc: true
---

**Introduction to Java StringBuffer class.**

## String Buffer class
**Key Points :-**

 - used to created mutable (modifiable) string.
 
 - **Mutable** :- Which can be changed.
 - is thread-safe i.e. multiple threads cannot access it simultaneously.

**Methods :-**

 - public synchronized StringBuffer append(String s)

 - public synchronized StringBuffer insert(int offset, String s)

 - public synchronized StringBuffer replace(int startIndex, int endIndex, String str)

 - public synchronized StringBuffer delete(int startIndex, int endIndex)

 - public synchronized StringBuffer reverse()

 - public int capacity()

 - public void ensureCapacity(int minimumCapacity)

 - public char charAt(int index)

 - public int length()

 - public String substring(int beginIndex)

 - public String substring(int beginIndex, int endIndex)

**Example Showing diffrence between String and String Buffer implementation :-**

    class Test {
     public static void main(String args[])
     {
      String str = "study";
      str.concat("tonight");
      System.out.println(str);      // Output: study
    
      StringBuffer strB = new StringBuffer("study");
      strB.append("tonight");
      System.out.println(strB);    // Output: studytonight
     }
    }

