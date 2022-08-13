---
title: "Getting started with internationalization"
slug: "getting-started-with-internationalization"
draft: false
images: []
weight: 1
type: docs
toc: true
---

## Installation or Setup
ICU(International Components for Unicode) can be installed as a library for the following languages:

* [Java: ICU4J](http://icu-project.org/apiref/icu4j/)
* [C/C++: ICU4C](http://icu-project.org/apiref/icu4c/)
* [Python: pyICU](https://pypi.python.org/pypi/PyICU/)
* [Perl: Wrappers for ICU (PICU)](http://www.jebriggs.com/ICU-Wrapper-1.00.00.tar.gz)
* [C#: ICU-dotnet](https://github.com/sillsdev/icu-dotnet)
* [Erlang: icu4e](https://github.com/beerriot/icu4e)
* [Ruby: ffi-icu](https://github.com/fantasticfears/ffi-icu)
* [Haskell: text-icu](https://bitbucket.org/bos/text-icu/src)
* [Lua: ICU4Lua](http://luaforge.net/projects/icu-lua/)

Here is a series of installion steps for a few implementations:

* ICU4J:
  * place icu4j.jar in your CLASSPATH
    * Optionally, place icu4j-charset.jar</strong> in your CLASSPATH for charset API support
  * To build ICU4J, you will need JDK 7+ and Apache Ant 1.9+
  * Set the JDK and Ant environment variables
    *  set JAVA_HOME=C:\jdk1.8.0
    *  set ANT_HOME=C:\apache-ant
    *  set PATH=%JAVA_HOME%\bin;%ANT_HOME%\bin;%PATH%
  * Run the desired target defined in build.xml. The default target is "jar" which compiles ICU4J library class files and create ICU4J jar files. For example:
    * C:\icu4j>ant
 
* ICU4C
  * Place the path to ICU4C in the LD_LIBRARY_PATH environment variable so that dynamic linker can find it. For example:
    * export LD_LIBRARY_PATH=/opt/icu/lib
  * Or Windows:
    * set PATH=%PATH%;C:\icu\dist\bin; set INCLUDE=%INCLUDE%;C:\icu\dist\include; set LIB=%LIB%;C:\icu\dist\bin
  * Run the configure script and build ICU4C using a makefile:
    * ./configure --prefix=/opt/icu
  * Or CygWin:
    * set PATH=%PATH%;C:\Cygwin\bin C:> dos2unix * C:> dos2unix -f configure
    * C:> "C:\Program Files (x86)\Microsoft Visual Studio 10.0\VC\vcvarsall.bat" x86
    * C:> bash runConfigureICU Cygwin/MSVC -prefix=/cygdrive/c/icu/dist
  * Run make and make install:
    * make && make install

