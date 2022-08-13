---
title: "Java RESTful Web Services (JAX-RS)"
slug: "java-restful-web-services-jax-rs"
draft: false
images: []
weight: 9889
type: docs
toc: true
---

Unlike SOAP and the WS- stack, which are specified as W3C standards, REST is really a set of principles for designing and using web-based interface. REST / RESTful applications rely heavily on other standards:

    HTTP
    URI, URL
    XML, JSON, HTML, GIF, JPEG, and so forth (resource representations)

The role of JAX-RS (Java API for RESTful Web Services) is to provide APIs that support building RESTful services.  However, JAX-RS is just *one way of doing this*.  RESTful services can be implemented other ways in Java, and (indeed) in many other programming languages.


## Simple Resource
First of all for a JAX-RS application must be set a base URI from which all the resources will be available.
For that purpose the `javax.ws.rs.core.Application` class must be extended and annotated with the `javax.ws.rs.ApplicationPath` annotation. The annotation accepts a string argument which defines the base URI.

    @ApplicationPath(JaxRsActivator.ROOT_PATH)
    public class JaxRsActivator extends Application {
    
        /**
         * JAX-RS root path.
         */
        public static final String ROOT_PATH = "/api";
    
    }

Resources are simple [POJO][1] classes which are annotated with the `@Path` annotation. 


    import javax.ws.rs.GET;
    import javax.ws.rs.Path;
    import javax.ws.rs.Produces;
 
    @Path("/hello")
    public class HelloWorldResource {
        public static final String MESSAGE = "Hello StackOverflow!";
 
        @GET
        @Produces("text/plain")
        public String getHello() {
            return MESSAGE;
        }
    }

When a `HTTP GET` request is sent to `/hello`, the resource responds with a `Hello StackOverflow!` message.


  [1]: http://stackoverflow.com/questions/3326319/what-does-the-term-plain-old-java-object-pojo-exactly-mean


## GET method types


## POST Method


## Name binding


## DELETE method


## Exception Mapper


## UriInfo


## SubResources


## Custom parameter converters


