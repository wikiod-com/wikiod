---
title: "Configuring JAX-RS in Jersey"
slug: "configuring-jax-rs-in-jersey"
draft: false
images: []
weight: 9968
type: docs
toc: true
---

## Java Jersey CORS filter for Cross Origin Requests


## Java Jersey Configuration
This example illustrates how to configure Jersey so that you can begin using it as a JAX-RS implementation framework for your RESTful API.

Assuming that you have already installed [Apache Maven][1], follow these steps to set up Jersey:

1. Create maven web project structure, in terminal (windows) execute the following command 

> mvn archetype:generate -DgroupId= com.stackoverflow.rest -DartifactId=
> jersey-ws-demo
>     -DarchetypeArtifactId=maven-archetype-webapp -DinteractiveMode=false

**Note:** To support Eclipse, use Maven command :
**mvn eclipse:eclipse -Dwtpversion=2.0**

 2. Go to the folder where you created your maven project,in your pom.xml, add the required dependencies

    
    <dependencies>
        <!-- Jersey 2.22.2 -->
        <dependency>
            <groupId>org.glassfish.jersey.containers</groupId>
            <artifactId>jersey-container-servlet</artifactId>
            <version>${jersey.version}</version>
        </dependency>
        <!-- JSON/POJO support -->
        <dependency>
            <groupId>org.glassfish.jersey.media</groupId>
            <artifactId>jersey-media-json-jackson</artifactId>
            <version>${jersey.version}</version>
        </dependency>
    </dependencies>
  
    <properties>
        <jersey.version>2.22.2</jersey.version>
        <project.build.sourceEncoding>UTF-8</project.build.sourceEncoding>
    </properties>

3. In Web.xml, add the following code

  
    <servlet>
        <servlet-name>jersey-serlvet</servlet-name>
        <servlet-class>org.glassfish.jersey.servlet.ServletContainer</servlet-class>
        <init-param>
            <param-name>jersey.config.server.provider.packages</param-name>
            <!-- Service or resources to be placed in the following package --> 
            <param-value>com.stackoverflow.service</param-value>
        </init-param>
       
        <!-- Application configuration, used for registering resources like filters  -->
        <init-param>
            <param-name>javax.ws.rs.Application</param-name>
            <param-value>com.stackoverflow.config.ApplicationConfig</param-value>
        </init-param>
        <load-on-startup>1</load-on-startup>
     </servlet>
    
     <!-- Url mapping, usage-http://domainname:port/appname/api/ -->
     <servlet-mapping>
         <servlet-name>jersey-serlvet</servlet-name>
         <url-pattern>/api/*</url-pattern>
     </servlet-mapping>

 4. The `ApplicationConfig` class


    public class ApplicationConfig extends ResourceConfig {
        public ApplicationConfig() {
            register(OtherStuffIfNeeded.class);
        }
    }

It should also be noted that if you want to go with _no_ web.xml, you could simply get rid of it, and add `@ApplicationPath("/api")` on top of the `ApplicationConfig` class.

    @ApplicationPath("/api")
    public class ApplicationConfig extends ResourceConfig {
        public ApplicationConfig() {
            // this call has the same effect as
            // jersey.config.server.provider.packages
            // in the web.xml: it scans that packages for resources and providers. 
            packages("com.stackoverflow.service");
        }
    }

 5. Build and deploy your maven project.
 6. You can now set up your Java RESTful webservice (JAX-RS) classes to use Jersey's jars.

        

  [1]: https://maven.apache.org/install.html "Maven"

