---
title: "Getting started with cdi"
slug: "getting-started-with-cdi"
draft: false
images: []
weight: 1
type: docs
toc: true
---

## Quick setup in a Java SE environment
If you are working with a Java EE 6+ application server, CDI is part of the container and you do not need to do anything to start using it. But CDI is not limited to Java EE application servers. It can be used in Java SE applications or simple servlet containers just as easily. Let's take a look at using CDI in a simple command-line application.

## Step 1. Add dependencies to your POM.

    <dependency>
        <groupId>org.jboss.weld.se</groupId>
        <artifactId>weld-se-core</artifactId>
        <version>3.0.0.Alpha15</version>
    </dependency>


## Step 2. Add beans.xml

CDI requires an empty beans.xml file so it can scan the JAR for classes.  So create

    src/main/resources/META-INF/beans.xml

with the following

    <beans xmlns="http://xmlns.jcp.org/xml/ns/javaee"
           xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance"
           xsi:schemaLocation="http://xmlns.jcp.org/xml/ns/javaee http://xmlns.jcp.org/xml/ns/javaee/beans_1_1.xsd"
           bean-discovery-mode="all">
        <scan>
            <exclude name="org.jboss.weld.**" />
        </scan>
    </beans>

## Step 3. Initialize CDI

In this example, the main(String []) method initializes CDI and then CDI is used to get an instance of the class itself to start running the SE application.

    import java.util.Arrays;
    import java.util.List;
    import javax.enterprise.inject.spi.CDI;
    import javax.inject.Inject;
    
    public class Main {
        public static void main(String[] args) {
            CDI<Object> cdi = CDI.getCDIProvider().initialize();
            Main main = cdi.select(Main.class).get();
            main.main(Arrays.asList(args));
        }
    
        @Inject
        protected MyService myService;
    
        protected void main(List<String> args) {
            System.out.println("Application starting");
    
            // MyService object injected by CDI
            myService.go();
        }
    }


That's it, really simple.

## Installation or Setup
Detailed instructions on getting cdi set up or installed.

## Implementations
CDI is a [Java EE specification][1]. It specifies how things should be done, and which features must be provided, but it isn't actually a specific library or set of code. In order to use CDI, you will need to use a CDI *implementation*.

The reference implementation of the CDI spec is a set of libraries known as [Weld][2]. An alternative implementation of the CDI spec exists as [Apache OpenWebBeans][3]. Either of these implementations will be able to give you the features of CDI. If you are not using a Java EE application server that ships with one of these implementations, it will be up to you to select and install one of these implementations into your application or runtime.

  [1]: http://www.cdi-spec.org/
  [2]: http://weld.cdi-spec.org/
  [3]: http://openwebbeans.apache.org/

