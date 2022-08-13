---
title: "JSF Annotations"
slug: "jsf-annotations"
draft: false
images: []
weight: 9941
type: docs
toc: true
---

I get many informations from this web sites:

 - http://www.jmdoudoux.fr/java/dej/chap-annotations.html
 - http://docs.oracle.com/javaee/6/tutorial/doc/girch.html

## Introduction to annotations
**Why annotations?**

Generally we use annotation to facilitate the development and to make the code more clear and clean.

**What are annotations?**

Java 5 annotations provide standardization of metadata in a general goal. This metadata associated with Java features can be exploited in the compilation or execution.

Java was modified to allow the implementation of annotations:
 - A dedicated syntax was added in Java to allow the definition and use of annotations.
 - bytecode is enhanced to allow storage of annotations.

**Where can annotations be used?**

Annotations can be used with :
 
packages, classes, interfaces, constructors, methods, fields, parameters, variables or annotations themselves.

**Categories of annotation**

There are three categories of annotation:

 - **Markers**: These annotations do not have an attribute 

For example `@Deprecated`, `@Override` ...

 - **Single value annotation**: these annotations have only one attribute 

For example `@MyAnnotation ( "test")`

 - **Full annotations**: these annotations have multiple attributes 

For example `@MyAnnotation (arg1 = "test 3", arg2 = "test 2", arg3 = "test3")`

> Like we see before you can create your own annotation



## Managed bean scope annotation
**Create managed bean**

To create a manage bean you need the annotation `@ManagedBean`

for example:

    @ManagedBean
    public class Example {}

You need the package:

    import javax.faces.bean.ManagedBean;

**Managed bean Scope**

We use annotations to define the scope in which the bean will be stored. 

There are many scope of managed bean: `@NoneScoped, @RequestScoped, @ViewScoped, @SessionScoped, @ApplicationScoped`, ...

 - Application (`@ApplicationScoped`): Application scope persists across
   all users’ interactions with a web application.
 - Session (`@SessionScoped`): Session scope persists across multiple HTTP
   requests in a web application.
 - View (`@ViewScoped`): View scope persists during a user’s interaction
   with a single page (view) of a web application.
 - Request (`@RequestScoped`): Request scope persists during a single HTTP
   request in a web application.
 - None (`@NoneScoped`): Indicates a scope is not defined for the
   application.
 - Custom (`@CustomScoped`): A user-defined, nonstandard scope. Its value
   must be configured as a `java.util.Map`. Custom scopes are used
   infrequently.



