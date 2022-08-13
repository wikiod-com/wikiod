---
title: "Getting started with java-ee"
slug: "getting-started-with-java-ee"
draft: false
images: []
weight: 1
type: docs
toc: true
---

## What is Java EE?
Java EE stands for Java Enterprise Edition. Java EE extends the Java SE (which stands for Java Standard Edition). Java EE is a set of technologies and related specifications that are oriented towards the development of large-scale enterprise applications. Java EE is developed in a community driven process.
So far the following versions of Java EE have been released:

 - J2EE 1.2 (December12, 1999)
 - J2EE 1.3 (September 24, 2001)
 - J2EE 1.4 (November 11, 2003)
 - Java EE 5 (May 11, 2006)
 - Java EE 6 (December 10, 2009)
 - Java EE 7 (April 5, 2013)

And Java EE 8 is expected to be released on the first half of 2017.

A key concept of the Java EE is that every Java EE version is comprised by a set of specific technologies. These technologies address specific JSRs (Java Specification Requests). In order for a programmer to use these technologies he needs to download an implementation of the Java EE technology specifications. The Java Community provides a reference implementation for each technology but other Java EE compliant technologies are developed and can also be used. The community provides a set of tests, namely the Java Compatibility Kit (JCK) that can be used by the developers of a JSR implementation to check if it is compliant or not with the JSR. The following table gives an overview of the technologies that comprise Java EE 7 and the related JSR that define the specs.

| Java EE 7 Technology | JSR |
| ------ | ------ |
| Java Platform, Enterprise Edition 7 (Java EE 7)|JSR 342 |
|Java API for WebSocket | JSR 356 |
|Java API for JSON Processing | JSR 353 |
|Java Servlet 3.1 | JSR 340 |
|JavaServer Faces 2.2 | JSR 344 |
|Expression Language 3.0 | JSR 341 |
|JavaServer Pages 2.3 | JSR 245 |
|Standard Tag Library for JavaServer Pages (JSTL) 1.2 | JSR 52 |
|Batch Applications for the Java Platform | JSR 352 |
|Concurrency Utilities for Java EE 1.0 | JSR 236 |
|Contexts and Dependency Injection for Java 1.1 | JSR 346 |
|Dependency Injection for Java 1.0 | JSR 330 |
|Bean Validation 1.1 | JSR 349 |
|Enterprise JavaBeans 3.2 | JSR 345 |
|Interceptors 1.2 (Maintenance Release) | JSR 318 |
|Java EE Connector Architecture 1.7 | JSR 322 |
|Java Persistence 2.1 | JSR 338 |
|Common Annotations for the Java Platform 1.2 | JSR 250 |
|Java Message Service API 2.0 | JSR 343 |
|Java Transaction API (JTA) 1.2 | JSR 907 |
|JavaMail 1.5 | JSR 919 |
|Java API for RESTful Web Services (JAX-RS) 2.0 | JSR 339 |
|Implementing Enterprise Web Services 1.3 | JSR 109 |
|Java API for XML-Based Web Services (JAX-WS) 2.2 | JSR 224 |
|Web Services Metadata for the Java Platform | JSR 181 |
|Java API for XML-Based RPC (JAX-RPC) 1.1 (Optional) | JSR 101 |
|Java APIs for XML Messaging 1.3 | JSR 67 |
|Java API for XML Registries (JAXR) 1.0 | JSR 93 |
|Java Authentication Service Provider Interface for Containers 1.1 | JSR 196 |
|Java Authorization Contract for Containers 1.5 | JSR 115 |
|Java EE Application Deployment 1.2 (Optional) | JSR 88 |
|J2EE Management 1.1 | JSR 77 |
|Debugging Support for Other Languages 1.0 | JSR 45 |
|Java Architecture for XML Binding (JAXB) 2.2 | JSR 222 |
|Java API for XML Processing (JAXP) 1.3 | JSR 206 |
|Java Database Connectivity 4.0 | JSR 221 |
|Java Management Extensions (JMX) 2.0 | JSR 003 |
|JavaBeans Activation Framework (JAF) 1.1 | JSR 925 |
|Streaming API for XML (StAX) 1.0 | JSR 173 |



## Installation
First of all, you cannot "install" Java EE. Java EE consists of a number of specifications.
You can install implementations of those specifications however. 

Depending on your needs, there are lots of possibilities. 
To install most (or all) of the specifications, you can choose a Java EE 7 compatible Application Server. 
Depending on your needs, you can choose between application servers that implement the web profile or application servers that implement the full profile.
For a list of Java EE7 compatible application servers see [Java EE Compatibility]( http://www.oracle.com/technetwork/java/javaee/overview/compatibility-jsp-136984.html).


 


## Installing Payara Server Full
**Prerequisites**

 - JDK 1.7 or later installed. You can find the Oracle JDK's [here.][1]

**Steps** 
- Download [Payara Server Full][2].
- Unzip the Payara Server at some location on your computer. 
We will use `C:\payara41` as INSTALL_DIR for Windows users and `/payara41` for Linux/Mac users.

**Starting / stopping Payara from the command prompt**
- Windows: Open a command prompt and execute the following command to start/stop Payara:

    `"C:\payara41\bin\asadmin" start-domain`

    `"C:\payara41\bin\asadmin" stop-domain`
- Linux/Max: Open a terminal and execute the following command to start/stop Payara:

    `/payara41/bin/asadmin start-domain`

    `/payara41/bin/asadmin stop-domain`

**Starting Payara from Netbeans**
- Add the Payara server to Netbeans (see previous chapter)
- Go to the 'Services' tab (Windows -> Services).
- Expand the 'Servers' item.
- Right-click on the Payara server and select 'Start'.
- (Optional) Right-click on the Payara server and select 'View Domain Admin Console' to go to the console.

To check that you're running the Application Server, open a browser and go to http://localhost:4848 to see the Payara Server Console. 

Voila! Now it's time to build your first application using JavaEE and deploy it to your server!

[1]: http://www.oracle.com/technetwork/es/java/javase/downloads/index.html
[2]: http://www.payara.fish/downloads

## Building my First JavaEE Application (Hello World)
Let's understand something. JavaEE consists of a number of specifications. When you install an Application Server (Payara for example), you install all of the specifications at once. For example there's a specification for an ORM called **JPA** (Java Persistence API), a specification to build *Component Based* Web Applications called **JSF** (Java Server Faces), a specification to build REST web services and clients called **JAX-RS**.

So as you might guess, there's not only one way to build a simple Hello World application in JavaEE. 

Secondly, the JavaEE spec has a specific structure of folders that looks something like this (simplified):

    /projectname/src/main/java
    /projectname/src/main/resources
    /projectname/src/main/resources/META-INF
    /projectname/src/main/webapp
    /projectname/src/main/webapp/WEB-INF
Inside the `/projectname/src/main/java` we put all the java classes that we need.

Inside the `/projectname/src/main/webapp` we put html files, css files, javascript files, etc.

Inside the `/projectname/src/main/webapp/WEB-INF`goes some optional configuration files, such as *web.xml* and *beans.xml*.

For simplicity we will use the NetBeans IDE (it's free) to build our first JavaEE Application. You can find it [here.][1] Choose the JavaEE version and install it.

## Create new project ##
 - Open NetBeans IDE
 - Go to File > New Project > Maven > Web Application and click Next.
 - Change **project name** to **HelloJavaEE**, then click Next and Finish.

## Clean and build the project ##
 - Go to Run > Clean and Build Project (HelloJavaEE).

## Deploy the WAR file ##
 - In a browser, go to http://localhost:4848 (follow instructions to [install payara server][2]).
 - Go to Applications > Click Deploy, Click on Select File and choose your war file (HelloJavaEE-1.0-SNAPSHOT.war) under `../NetBeansProjects/HelloJavaEE/target`.

## Deploy the WAR file directly from Netbeans ##
- Install Payara (see next chapter).
- In Netbeans, go to the 'Services' tab (Window-> Services if you don't see it).
- Right-click on Servers and select 'Add Server...'
- Select 'GlassFish Server', give it a name and click next.
- Point to your local Payara installation, select 'Local Domain' and click next.
- Leave the domain location settings as they are (Domain: domain1, Host: localhost, DAS Port: 4848, HTTP Port: 8080).
- Go to the 'Projects' tab (Windows -> Projects).
- Right-click on your project and select 'Properties'.
- In the left hand pane, go to 'Run' and select the Server you just added.
- (Optional) Change the context path. If you set the context path to '/MyFirstApplication' the default URL will be 'http://localhost:8080/MyFirstApplication'.

## View results ##
 - In a browser, go to http://localhost:8080/HelloJavaEE-1.0-SNAPSHOT 

Voila! That's your first app using JavaEE technology. You should now start creating other "Hello World" apps using different specifications like JPA, EJB, JAX-RS, JavaBatch, etc...


  [1]: https://netbeans.org/downloads/
  [2]: https://www.wikiod.com/java-ee/getting-started-with-java-ee#Installation

