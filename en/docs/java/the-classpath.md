---
title: "The Classpath"
slug: "the-classpath"
draft: false
images: []
weight: 9519
type: docs
toc: true
---

The classpath lists places where the Java runtime should look for classes and resources. The classpath is also used by the Java compiler to find previously compiled and external dependencies.

**Java class loading**

The JVM (Java Virtual Machine) will load classes as and when the classes are required (this is called lazy-loading).  Locations of the classes to be used are specified in three places:-

 1. Those required by the Java Platform are loaded first, such as those in the Java Class Library and it's dependencies.
 2. Extension classes are loaded next (i.e. those in `jre/lib/ext/`)
 3. User-defined classes via the classpath are then loaded

Classes are loaded using classes that are subtypes of `java.lang.ClassLoader`.  This described in a more detail in this Topic: https://www.wikiod.com/java/classloaders

**Classpath**

The classpath is a parameter used by the JVM or compiler which specifies the locations of user-defined classes and packages.
This can be set in the command line as with most of these examples or through an environmental variable (`CLASSPATH`)


## Different ways to specify the classpath
There are three ways to set the classpath.

1. It can be set using the `CLASSPATH` environment variable :

        set CLASSPATH=...         # Windows and csh
        export CLASSPATH=...      # Unix ksh/bash

2. It can be set on the command line as follows

        java -classpath ...
        javac -classpath ...

   Note that the `-classpath` (or `-cp`) option takes precedence over the `CLASSPATH` environment variable.

3. The classpath for an executable JAR file is specified using the `Class-Path` element in `MANIFEST.MF`:

        Class-Path: jar1-name jar2-name directory-name/jar3-name

   Note that this only applies when the JAR file is executed like this:

        java -jar some.jar ...

   In this mode of execution, the `-classpath` option and the CLASSPATH environment variable will be ignored, even if the JAR file has no `Class-Path` element. 

If no classpath is specified, then the default classpath is the selected JAR file when using `java -jar`, or the current directory otherwise.

Related:

* https://docs.oracle.com/javase/tutorial/deployment/jar/downman.html
* http://docs.oracle.com/javase/7/docs/technotes/tools/windows/classpath.html

## Adding all JARs in a directory to the classpath
If you want to add all the JARs in directory to the classpath, you can do this concisely using classpath wildcard syntax; for example:

     someFolder/*

This tells the JVM to add all JAR and ZIP files in the `someFolder` directory to the classpath.  This syntax can be used in a `-cp` argument, a `CLASSPATH` environment variable, or a `Class-Path` attribute in an executable JAR file's manifest file.See [*Setting the Class Path: Class Path Wild Cards*][1] for examples and caveats. 

Notes:

  1. Classpath wildcards were first introduced in Java 6. Earlier versions of Java do not treat "*" as a wildcard. 
  2. You cannot put other characters before or after the "*"; e.g. "someFolder/*.jar" is not a wildcard.
  3. A wildcard matches only files with the suffix ".jar" or ".JAR".  ZIP files are ignored, as are JAR files with a different suffixes.
  4. A wildcard matches only JAR files in the directory itself, not in its subdirectories.
  5. When a group of JAR files is matched by a wildcard entry, their relative order on the classpath is not specified.


  [1]: https://docs.oracle.com/javase/8/docs/technotes/tools/windows/classpath.html#A1100762

## Load a resource from the classpath
It can be useful to load a resource (image, text file, properties, KeyStore, ...) that is packaged inside a JAR. For this purpose, we can use the `Class` and `ClassLoader`s.

Suppose we have the following project structure : 

    program.jar
    |
    \-com
      \-project
        |
        |-file.txt
        \-Test.class  

And we want to access the contents of `file.txt` from the `Test` class. We can do so by asking the classloader : 

    InputStream is = Test.class.getClassLoader().getResourceAsStream("com/project/file.txt");

By using the classloader, we need to specify the fully qualified path of our resource (each package).

Or alternatively, we can ask the Test class object directly

    InputStream is = Test.class.getResourceAsStream("file.txt");

Using the class object, the path is relative to the class itself. Our `Test.class` being in the `com.project` package, the same as `file.txt`, we do not need to specify any path at all.

We can, however, use absolute paths from the class object, like so :

     is = Test.class.getResourceAsStream("/com/project/file.txt");


## Classpath path syntax
The classpath is a sequence of entries which are directory pathnames, JAR or ZIP file pathnames, or JAR / ZIP wildcard specifications.

 - For a classpath specified on the command line (e.g. `-classpath`) or as an environment variable, the entries must be separated with `;` (semicolon) characters on Windows, or `:` (colon) characters on other platforms (Linux, UNIX, MacOSX and so on).  

 - For the `Class-Path` element in a JAR file's `MANIFEST.MF`, use a single space to separate the entries. 

Sometimes it is necessary to embed a space in a classpath entry

 - When the classpath is specified on the command line, it is simply a matter of using the appropriate shell quoting.  For example:

       export CLASSPATH="/home/user/My JAR Files/foo.jar:second.jar"
       
   (The details may depend on the command shell that you use.)

 - When the classpath is specified in a JAR file's a "MANIFEST.MF" file, URL encoding must be used.  

        Class-Path: /home/user/My%20JAR%20Files/foo.jar second.jar



## Dynamic Classpath
Sometimes, just adding all the JARs from a folder isn't enough, for example when you have native code and need to select a subset of JARs. In this case, you need two `main()` methods. The first one builds a classloader and then uses this classloader to call the second `main()`.

Here is an example which selects the correct SWT native JAR for your platform, adds all your application's JARs and then invokes the real `main()` method: http://stackoverflow.com/questions/2706222/create-cross-platform-java-swt-application/3204032#3204032

## Mapping classnames to pathnames
The standard Java toolchain (and 3rd-party tools designed to interoperate with them) have specific rules for mapping the names of classes to the pathnames of files and other resources that represent them.

The mappings are as follows

 - For classes in the default package, the pathnames are simple filenames.
 - For classes in a named package, the package name components map to directories.
 - For named nested and inner classes, the filename component is formed by joining the class names with a `$` character.
 - For anonymous inner classes, numbers are used in place of names. 

This is illustrated in the following table:

| Classname | Source pathname | Classfile pathname |
| ------ | ------ | ----- |
| `SomeClass`   | `SomeClass.java`   | `SomeClass.class` |
| `com.example.SomeClass`   | `com/example/SomeClass.java` | `com/example/SomeClass.class` |
| `SomeClass.Inner`   | (in `SomeClass.java` ) | `SomeClass$Inner.class` |
| `SomeClass` anon inner classes | (in `SomeClass.java` ) | `SomeClass$1.class`, `SomeClass$2.class`, etc |




## What the classpath means:  how searches work
The purpose of the classpath is to tell a JVM where to find classes and other resources.  The meaning of the classpath and the search process are intertwined.

The classpath is a form of search path which specifies a sequence of *locations* to look for resources.  In a standard classpath, these places are either, a directory in the host file system, a JAR file or a ZIP file.  In each cases, the location is the root of a *namespace* that will be searched.

The standard procedure for searching for a class on the classpath is as follows:

1. Map the class name to a relative classfile pathname `RP`.  The mapping for class names to class filenames is described elsewhere.

2. For each entry `E` in the classpath:
   - If the entry is a filesystem directory:
       - Resolve `RP` relative to `E` to give an absolute pathname `AP`.
       - Test if `AP` is a path for an existing file.
       - If yes, load the class from that file
   - If the entry is a JAR or ZIP file:
       - Lookup `RP` in the JAR / ZIP file index.
       - If the corresponding JAR / ZIP file entry exists, load the class from that entry.

The procedure for searching for a resource on the classpath depends on whether the resource path is absolute or relative.  For an absolute resource path, the procedure is as above.  For a relative resource path resolved using `Class.getResource` or `Class.getResourceAsStream`, the path for the classes package is prepended prior to searching.

(Note these are the procedures implemented by the standard Java classloaders.  A custom classloader might perform the search differently.)

## The bootstrap classpath
The normal Java classloaders look for classes first in the bootstrap classpath, before checking for extensions and the application classpath.  By default, the bootstrap classpath consists of the "rt.jar" file and some other important JAR files that are supplied by the JRE installation.  These provide all of the classes in the standard Java SE class library, along with various "internal" implementation classes.

Under normal circumstances, you don't need to concern yourself with this.  By default, commands like `java`, `javac` and so on will use the appropriate versions of the runtime libraries.

Very occasionally, it is necessary to override the normal behavior of the Java runtime by using an alternative version of a class in the standard libraries.  For example, you might encounter a "show stopper" bug in the runtime libraries that you cannot work around by normal means.  In such a situation, it is possible to create a JAR file containing the altered class and then add it to the bootstrap classpath which launching the JVM.

The `java` command provides the following `-X` options for modifying the bootstrap classpath:

  - `-Xbootclasspath:<path>` replaces the current boot classpath with the path provided.
  - `-Xbootclasspath/a:<path>` appends the provided path to the current boot classpath.
  - `-Xbootclasspath/p:<path>` prepends the provided path to the current boot classpath.

Note that when use the bootclasspath options to replace or override a Java class (etcetera), you are technically modifying Java.  There *may be* licensing implications if you then distribute your code.  (Refer to the terms and conditions of the Java Binary License ... and consult a lawyer.)



