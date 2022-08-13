---
title : Java Language Tutorial
slug : java-language-tutorial
weight : 4288
draft : false
images : []
type : docs
---

The Java programming language is...

- **General-purpose**: It is designed to be used for writing software in a wide variety of application domains, and lacks specialized features for any specific domain.

- **Class-based**: Its object structure is defined in classes. Class instances always have those fields and methods specified in their class definitions (see [Classes and Objects][1]). This is in contrast to non-class-based languages such as JavaScript.

- **Statically-typed**: the compiler checks at compile time that variable types are respected. For example, if a method expects an argument of type `String`, that argument must in fact be a string when the method is called.

- **Object-oriented**: most things in a Java program are class instances, i.e. bundles of state (fields) and behavior (methods which operate on data and form the object's *interface* to the outside world).

- **Portable**: It can be compiled on any platform with `javac` and the resultant class files can run on any platform that has a JVM.

Java is intended to let application developers "write once, run anywhere" (WORA), meaning that compiled Java code can run on all platforms that support Java without the need for recompilation.

Java code is compiled to bytecode (the `.class` files) which in turn get interpreted by the Java Virtual Machine (JVM). In theory, bytecode created by one Java compiler should run the same way on any JVM, even on a different kind of computer. The JVM might (and in real-world programs will) choose to compile into native machine commands the parts of the bytecode that are executed often. This is called "Just-in-time (JIT) compilation".

# Java Editions and Versions

There are three "editions" of Java defined by Sun / Oracle:

  - *Java Standard Edition (SE)* is the edition that is designed for general use.
  - *Java Enterprise Edition (EE)* adds a range of facilities for building "enterprise grade" services in Java.  Java EE is covered [separately][2].
  - *Java Micro Edition (ME)* is based on a subset of *Java SE* and is intended for use on small devices with limited resources.

There is a separate topic on [Java SE / EE / ME editions][3].

Each edition has multiple versions.  The Java SE versions are listed below.

# Installing Java

There is a separate topic on [Installing Java (Standard Edition)][4].

# Compiling and running Java programs

There are separate topics on:

  - [Compiling Java source code][5]
  - [Java deployment][6] including creating JAR files
  - [Running Java applications][7]
  - [The Classpath][8]
  

# What's next?

Here are links to subjects to continue learning and understanding the Java programming language. These subjects are the basics of the Java programming to get you started.

  - [Primitive Data Types in Java][9]
  - [Operators in Java][10]
  - [Strings in Java][11]
  - [Basic Control Structures in Java][12]
  - [Classes and Objects in Java][1]
  - [Arrays in Java][13]
  - [Java code standards][14]

# Testing

While Java does not have any support for testing in the standard library, there are 3rd-party libraries that are designed to support testing.  The two most popular unit testing libraries are:

- [JUnit](https://www.wikiod.com/junit) ([Official Site](http://junit.org/junit4/))
- [TestNG](https://www.wikiod.com/docs/testng) ([Official Site](http://testng.org/doc/index.html))

# Other

- Design patterns for Java are covered in [Design Patterns][15].
- Programming for Android is covered in [Android][16].
- Java Enterprise Edition technologies are covered in [Java EE][2].
- The Oracle JavaFX technologies are covered in [JavaFX][17].

<sup>**1.** In **Versions** section the *end-of-life (free)* date is when Oracle will stop posting further updates of Java SE to its public download sites. Customers who need continued access to critical bug fixes and security fixes as well as general maintenance for Java SE can get long term support through [Oracle Java SE Support][java-se-support]. </sup>

  [java-se-support]: http://www.oracle.com/us/technologies/java/java-se-support-393643.html?ssSourceSiteId=otnen

  [1]: https://www.wikiod.com/java/classes-and-objects
  [2]: https://www.wikiod.com/java-ee
  [3]: https://www.wikiod.com/java/java-editions-versions-releases-and-distributions
  [4]: https://www.wikiod.com/java/installing-java-standard-edition
  [5]: https://www.wikiod.com/java/java-compiler---javac
  [6]: https://www.wikiod.com/java/java-deployment
  [7]: https://www.wikiod.com/java/the-java-command---java-and-javaw
  [8]: https://www.wikiod.com/java/the-classpath
  [9]: https://www.wikiod.com/java/primitive-data-types
  [10]: https://www.wikiod.com/java/operators
  [11]: https://www.wikiod.com/java/strings
  [12]: https://www.wikiod.com/java/basic-control-structures
  [13]: https://www.wikiod.com/java/arrays
  [14]: https://www.wikiod.com/java/oracle-official-code-standard
  [15]: https://www.wikiod.com/design-patterns
  [16]: https://www.wikiod.com/android
  [17]: https://www.wikiod.com/javafx

