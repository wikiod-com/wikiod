---
title : Kotlin Tutorial
slug : kotlin-tutorial
weight : 8255
draft : false
images : []
type : docs
---

[Kotlin](http://kotlinlang.org) is a statically-typed object-oriented programming language developed by JetBrains primarily targeting the JVM. Kotlin is developed with the goals of being quick to compile, backwards-compatible, very type safe, and 100% interoperable with Java. Kotlin is also developed with the goal of providing many of the features wanted by Java developers. Kotlin's standard compiler allows it to be compiled both into Java bytecode for the JVM and into JavaScript.

## Compiling Kotlin ##

Kotlin has a standard IDE plugin for Eclipse and IntelliJ. Kotlin can also be compiled [using Maven](http://kotlinlang.org/docs/reference/using-maven.html), [using Ant](http://kotlinlang.org/docs/reference/using-ant.html), and [using Gradle](http://kotlinlang.org/docs/reference/using-gradle.html), or through the [command line](https://kotlinlang.org/docs/tutorials/command-line.html).

It is worth noting in `$ kotlinc Main.kt` will output a java class file, in this case `MainKt.class` (Note the Kt appended to the class name). However if one was to run the class file using `$ java MainKt` java will throw the following exception:

```
Exception in thread "main" java.lang.NoClassDefFoundError: kotlin/jvm/internal/Intrinsics
    at MainKt.main(Main.kt)
Caused by: java.lang.ClassNotFoundException: kotlin.jvm.internal.Intrinsics
    at java.net.URLClassLoader.findClass(URLClassLoader.java:381)
    at java.lang.ClassLoader.loadClass(ClassLoader.java:424)
    at sun.misc.Launcher$AppClassLoader.loadClass(Launcher.java:335)
    at java.lang.ClassLoader.loadClass(ClassLoader.java:357)
    ... 1 more

```

In order to run the resulting class file using Java, one must include the Kotlin runt-time jar file to the current class path.

`java -cp .:/path/to/kotlin/runtime/jar/kotlin-runtime.jar MainKt ` 



