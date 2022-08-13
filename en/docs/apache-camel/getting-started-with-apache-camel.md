---
title: "Getting started with apache-camel"
slug: "getting-started-with-apache-camel"
draft: false
images: []
weight: 1
type: docs
toc: true
---

## Installation or Setup
Detailed instructions on adding the required Camel dependencies.


----------


<h2>Maven Dependency</h2>

One of the most common ways to include Apache Camel in your application is through a Maven dependency. By adding the dependency block below, Maven will resolve the Camel libraries and dependencies for you. 

    <dependency>
      <groupId>org.apache.camel</groupId>
      <artifactId>camel-core</artifactId>
      <version>2.17.3</version>
    </dependency>

----------

<h2>Gradle</h2>

Another common way to include Apache Camel in your application is through a Gradle dependency. Simply add the dependency line below and Gradle will import the Camel library and its dependencies for you. 

    // https://mvnrepository.com/artifact/org.apache.camel/camel-core
    compile group: 'org.apache.camel', name: 'camel-core', version: '2.17.3'

----------

<h2>Spring Boot</h2>

As of Camel 2.15, you can now leverage Apache Camel's Spring Boot dependency. The difference with this Camel library is that it provides an opinionated auto-configuration, including auto-detection of Camel routes. 

    <dependency>
        <groupId>org.apache.camel</groupId>
        <artifactId>camel-spring-boot</artifactId>
        <version>${camel.version}</version> <!-- use the same version as your Camel core version -->
    </dependency>



## Camel Domain Specific Language
Camel's DSL (Domain Specific Language) is one of the features that makes Camel standout from other Integration frameworks. While some other frameworks also feature a DSL concept, typically in the form of a XML file, the DSL was in such cases always a custom based language.

Camel offers multiple DSLs in programming languages such as Java, Scala, Groovy, and in XML.

For example a simple file copy route can be done in various ways as shown in the list below

 - Java DSL
      
       from("file:data/in").to("file:data/out");

- Blueprint/Spring DSL (XML)
    
      <route>
        <from uri="file:data/inbox"/>
        <to uri="file:data/out"/>
      </route>

- Scala DSL

      from "file:data/inbox" -> "file:data/out"


