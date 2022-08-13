---
title: "Getting started with primefaces"
slug: "getting-started-with-primefaces"
draft: false
images: []
weight: 1
type: docs
toc: true
---

## Installing PrimeFaces
PrimeFaces can be used in all web applications based on [Java Server Faces][1] (version 2.x) which are run on Servlet Containers (e.g. [Wildlfy][2] or [Tomcat][3] or [GlassFish][4]).

There are several ways you can add PrimeFaces to your application.

## Manually

[Download][5] the `primefaces-{version}.jar` and add it to you classpath.

## Maven

<!-- language: lang-xml -->

    <dependency>  
        <groupId>org.primefaces</groupId>  
        <artifactId>primefaces</artifactId>  
        <version>{version}</version>  
    </dependency>

For older versions (3.5 and below) you additionally have to add the PrimeFaces repository:


<!-- language: lang-xml -->

    <repository>  
        <id>prime-repo</id>  
        <name>PrimeFaces Maven Repository</name>  
        <url>http://repository.primefaces.org</url>  
        <layout>default</layout>  
    </repository>

## Gradle 


<!-- language: lang-groovy -->

    repositories {
        mavenCentral()
            maven {
                url "http://repository.primefaces.org"
            }
    }

    dependencies {
        compile "org.primefaces:primefaces:{version}"
    }

## NetBeans
 
PrimeFaces is bundled with the Java EE bundle of [NetBeans](https://netbeans.org/). When you create a new "Java Web -> Web Application" you can select JavaServer Faces as framework. Then you configure JSF to use PrimeFaces components. It will copy the library to your project.

If you have created a Maven web application, you can select project properties and select JavaServer Faces as framework and then select PrimeFaces as mentioned above. Your `pom.xml` will be modified to include the PrimeFaces dependency.

  [1]: https://www.wikiod.com/jsf/getting-started-with-jsf
  [2]: http://wildfly.org/
  [3]: http://tomcat.apache.org/
  [4]: https://glassfish.java.net/
  [5]: http://primefaces.org/downloads

## Hello world
After [adding PrimeFaces to your JSF project](https://www.wikiod.com/primefaces/getting-started-with-primefaces#Installing PrimeFaces), you can start using it in your pages using the namespace:

<!-- language: lang-xml -->
    xmlns:p="http://primefaces.org/ui"  

or, for PrimeFaces Mobile:

<!-- language: lang-xml -->
    xmlns:p="http://primefaces.org/mobile"  

This example should render a spinner:

<!-- language: lang-xml -->
    <html xmlns="http://www.w3.org/1999/xhtml"  
          xmlns:h="http://java.sun.com/jsf/html"  
          xmlns:f="http://java.sun.com/jsf/core"  
          xmlns:p="http://primefaces.org/ui">
        <h:head>  
        </h:head>      
        <h:body>
            <p:spinner />
        </h:body>
    </html>  

