---
title: "Getting started with jackson"
slug: "getting-started-with-jackson"
draft: false
images: []
weight: 1
type: docs
toc: true
---

## Setup
Jackson is a Java JSON library. As such, setting it up is as simple as adding it to the classpath of the Java project where its usage is intented.

Firstly, the jars need to be downloaded to a location on the local disk:

 - jackson-core
 - jackson-annotations (leverages annotation usage)
 - jackson-databind (general data-binding functionality)

They can be found for download in one of the central Maven repositories, under the **com.fasterxml.jackson.core** group:

 http://repo1.maven.org/maven2/com/fasterxml/jackson/core/

What is now left is adding the libraries on the classpath when running the Java application:

    java -cp "<<jackson_lib_location>>/*" my.package.MainClass

Alternatively, using **Maven**, the dependencies just need to be added to the *pom.xml* file of the project:

    <dependency>
      <groupId>com.fasterxml.jackson.core</groupId>
      <artifactId>jackson-core</artifactId>
      <version>2.8.8</version>
    </dependency>
    
    <dependency>
      <groupId>com.fasterxml.jackson.core</groupId>
      <artifactId>jackson-annotations</artifactId>
      <version>2.8.8</version>
    </dependency>
    
    <dependency>
      <groupId>com.fasterxml.jackson.core</groupId>
      <artifactId>jackson-databind</artifactId>
      <version>2.8.8</version>
    </dependency>

**Ivy** version:

    <dependency org="com.fasterxml.jackson.core" name="jackson-core" rev="2.8.8"/>
    <dependency org="com.fasterxml.jackson.core" name="jackson-annotations" rev="2.8.8"/>
    <dependency org="com.fasterxml.jackson.core" name="jackson-databind" rev="2.8.8"/>

**Gradle** version:

    dependencies {
        compile group: 'com.fasterxml.jackson.core', name: 'jackson-core', version: '2.8.8'
        compile group: 'com.fasterxml.jackson.core', name: 'jackson-annotations', version: '2.8.8'
        compile group: 'com.fasterxml.jackson.core', name: 'jackson-databind', version: '2.8.8'
    }

