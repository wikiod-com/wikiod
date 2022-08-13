---
title: "Java SE 8 Features"
slug: "java-se-8-features"
draft: false
images: []
weight: 9958
type: docs
toc: true
---

In this topic you'll find a summary of the new features added to the Java programming language in Java SE 8. There are many other new features in other fields such as JDBC and Java Virtual Machine (JVM) that are not going to be covered in this topic.

Reference: [Enhancements in Java SE 8][1]


  [1]: http://docs.oracle.com/javase/8/docs/technotes/guides/language/enhancements.html#javase8 "Official list of Enhancements in Java SE 8"

## New Java SE 8 programming language features
 - [Lambda Expressions][1], a new language feature, has been introduced in this release. They enable you to treat functionality as a method argument, or code as data. Lambda expressions let you express instances of single-method interfaces (referred to as functional interfaces) more compactly.  
   - [Method references][2] provide easy-to-read lambda expressions for methods that already have a name. 
   - [Default methods][3] enable new functionality to be added to the interfaces of libraries and ensure binary compatibility with code written for older versions of those interfaces. 
   - [New and Enhanced APIs That Take Advantage of Lambda Expressions and Streams][4] in Java SE 8 describe new and enhanced classes that take advantage of lambda expressions and streams. 
- Improved Type Inference - The Java compiler takes advantage of target typing to infer the type parameters of a generic method invocation. The target type of an expression is the data type that the Java compiler expects depending on where the expression appears. For example, you can use an assignment statement's target type for type inference in Java SE 7. However, in Java SE 8, you can use the target type for type inference in more contexts.
   - [Target Typing][5] in [Lambda Expressions][1]
   - [Type Inference][6]
- [Repeating Annotations][7] provide the ability to apply the same annotation type more than once to the same declaration or type use.
- [Type Annotations][8] provide the ability to apply an annotation anywhere a type is used, not just on a declaration. Used with a
   pluggable type system, this feature enables improved type checking of
   your code.
- [Method parameter reflection][9] - You can obtain the names of the formal parameters of any method or constructor with the method `java.lang.reflect.Executable.getParameters`. (The classes Method and Constructor extend the class Executable and therefore inherit the method `Executable.getParameters`) However, `.class` files do not store formal parameter names by default. To store formal parameter names in a particular `.class` file, and thus enable the Reflection API to retrieve formal parameter names, compile the source file with the `-parameters` option of the javac compiler.
- Date-time-api - Added new time api in `java.time`. If used this, you don't need to designate timezone.


  [1]: http://docs.oracle.com/javase/tutorial/java/javaOO/lambdaexpressions.html
  [2]: http://docs.oracle.com/javase/tutorial/java/javaOO/methodreferences.html
  [3]: http://docs.oracle.com/javase/tutorial/java/IandI/defaultmethods.html
  [4]: http://docs.oracle.com/javase/8/docs/technotes/guides/language/lambda_api_jdk8.html
  [5]: http://docs.oracle.com/javase/tutorial/java/javaOO/lambdaexpressions.html#target-typing
  [6]: http://docs.oracle.com/javase/tutorial/java/generics/genTypeInference.html
  [7]: http://docs.oracle.com/javase/tutorial/java/annotations/repeating.html
  [8]: http://docs.oracle.com/javase/tutorial/java/annotations/type_annotations.html
  [9]: http://docs.oracle.com/javase/tutorial/reflect/member/methodparameterreflection.html

