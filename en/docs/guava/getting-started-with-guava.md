---
title: "Getting started with guava"
slug: "getting-started-with-guava"
draft: false
images: []
weight: 1
type: docs
toc: true
---

## Setup
Dependency on Guava can be added in your Java project by using any build system. 

**Maven:**

    <dependency>
      <groupId>com.google.guava</groupId>
      <artifactId>guava</artifactId>
      <version>19.0</version>
    </dependency>

**Gradle:**

    dependencies {
      compile 'com.google.guava:guava:19.0'
    }

**Ivy**

    <dependency org="com.google.guava" name="guava" rev="19.0" />

**Buildr**

    compile.with 'com.google.guava:guava:jar:19.0'

**Manual Dependency**

You can also just manually [download JARs][1] from Guava's release page for the classes, sources and javadocs.


Note that JDK 1.6 or newer is required for Guava 12.0 through 20.0. See Version list for more info.
Guava users who target Java 5 should use the [Guava JDK5 backport][2]. This includes users who target Android Froyo and earlier.


  [1]: https://github.com/google/guava/wiki/ReleaseHistory
  [2]: http://mvnrepository.com/artifact/com.google.guava/guava-jdk5

