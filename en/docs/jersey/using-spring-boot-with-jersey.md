---
title: "Using Spring Boot with Jersey"
slug: "using-spring-boot-with-jersey"
draft: false
images: []
weight: 9947
type: docs
toc: true
---

## Simple Application with Spring Boot and Jersey
Spring Boot is a bootstrapping framework for Spring applications. It has seamless support for integrating with Jersey also. One of the advantages of this (from the perspective of a Jersey user), is that you have access to Spring's vast ecosystem.

To get started, create a new _standalone_ (non-wepapp) Maven project. We can create a webapp also, but for this guide, we will just use a standalone app. Once you've create the project, add the following to your `pom.xml`

    <properties>
        <project.build.sourceEncoding>UTF-8</project.build.sourceEncoding>
        <maven.compiler.source>1.8</maven.compiler.source>
        <maven.compiler.target>1.8</maven.compiler.target>
    </properties>
    <parent>
        <groupId>org.springframework.boot</groupId>
        <artifactId>spring-boot-starter-parent</artifactId>
        <version>1.4.0.RELEASE</version>
    </parent>
    <dependencies>  
        <dependency>
            <groupId>org.springframework.boot</groupId>
            <artifactId>spring-boot-starter-jersey</artifactId>
        </dependency>
        <dependency>
           <groupId>org.springframework.boot</groupId>
           <artifactId>spring-boot-starter-tomcat</artifactId>
        </dependency>
    </dependencies>
    <build>
        <plugins>
            <plugin>
                <groupId>org.springframework.boot</groupId>
                <artifactId>spring-boot-maven-plugin</artifactId>
            </plugin>
        </plugins>
    </build>

We just need two dependencies. One for the Jersey Spring Boot module, and another for an embedded Tomcat server. The plugin we will use to run the application to test.

Once you have that, add the following classes to the project.

    com/example
      |
      +-- GreetingApplication.class
      +-- JerseyConfig.class
      |
      + com/example/services
      |   |
      |   +-- GreetingService.class
      |   +-- NiceGreetingService.class
      |
      + com/examples/resources
          |
          +-- GreetingResource.class 

**`GreetingApplication.class`**

This is the bootstrap class (very simple)

    import org.springframework.boot.SpringApplication;
    import org.springframework.boot.autoconfigure.SpringBootApplication;
    
    @SpringBootApplication
    public class GreetingApplication {
        
        public static void main(String[] args) {
            SpringApplication.run(GreetingApplication.class, args);
        }
    }

**`JerseyConfig.class`**

This is the Jersey configuration class

    import javax.ws.rs.ApplicationPath;
    import org.glassfish.jersey.server.ResourceConfig;
    import org.springframework.stereotype.Component;
    
    @Component
    @ApplicationPath("/api")
    public class JerseyConfig extends ResourceConfig {
        public JerseyConfig() {
            packages("com.example");
        }
    }

**`GreetingService.class`** and **`NiceGreetingService.class`**

    public interface GreetingService {
       public String getGreeting(String name);
    }
    
    import org.springframework.stereotype.Component;
    
    @Component
    public class NiceGreetingService implements GreetingService {
        
        @Override
        public String getGreeting(String name) {
            return "Hello " + name + "!";
        }
    }

**`GreetingResource`**

This is the resource class where we will let Spring inject the `GreetingService` into.

    import javax.ws.rs.GET;
    import javax.ws.rs.Path;
    import javax.ws.rs.QueryParam;
    import org.springframework.beans.factory.annotation.Autowired;
    import com.example.service.GreetingService;
    
    @Path("greeting")
    public class GreetingResource {

        private GreetingService greetingService;

        @Autowired
        public GreetingResource(GreetingService greetingService) {
            this.greetingService = greetingService;
        }
    
        @GET
        public String getGreeting(@QueryParam("name") String name) {
            return this.greetingService.getGreeting(name);
        }
    }

And that's it. We can now run the application. Grab a terminal and run the following command from the root of the project.

    mvn spring-boot:run

The application should take a few seconds to get started. There will be some logging, and you will see some Spring ASCII art. After that art, it should be about 30 lines or so of more logging, then you should see 

     15.784 seconds (JVM running for 38.056)

Now the app is started. If you use cURL you can test it with

    curl -v 'http://localhost:8080/api/greeting?name=peeskillet'

If you are on Windows, use double quotes around the URL. If you aren't using cURL, just type it in the browser. You should see the result

    Hello peeskillet!

You may notice the request takes a few seconds on the first request you make. That is because Jersey is not fully loaded when the app launches. We can change that by adding a `application.properties` file in the `src/main/resources` folder. In that file add the following:

    spring.jersey.servlet.load-on-startup=1

