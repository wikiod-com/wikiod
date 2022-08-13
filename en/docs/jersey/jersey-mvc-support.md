---
title: "Jersey MVC Support"
slug: "jersey-mvc-support"
draft: false
images: []
weight: 9965
type: docs
toc: true
---

MVC Frameworks such as Spring MVC are using to create web applications that serve dynamic web pages. Jersey, though known to be a REST Framework, also has support for create dynamic web pages using its MVC module.

## Jersey MVC Hello World
To get started, create a new Maven webapp (how to do this is outside the scope of this example). In your pom.xml, add the following two dependencies

    <dependency>
        <groupId>org.glassfish.jersey.containers</groupId>
        <artifactId>jersey-container-servlet</artifactId>
        <version>2.25.1</version>
    </dependency>
    <dependency>
        <groupId>org.glassfish.jersey.ext</groupId>
        <artifactId>jersey-mvc-jsp</artifactId>
        <version>2.25.1</version>
    </dependency>

Also in the pom, add the `jetty-maven-plugin` that we'll be using the run the application during development

    <build>
        <finalName>jersey-mvc-hello-world</finalName>
        <plugins>
            <plugin>
                <groupId>org.apache.maven.plugins</groupId>
                <artifactId>maven-compiler-plugin</artifactId>
                <version>2.5.1</version>
                <inherited>true</inherited>
                <configuration>
                    <source>1.8</source>
                    <target>1.8</target>
                </configuration>
            </plugin>

            <plugin>
                <groupId>org.eclipse.jetty</groupId>
                <artifactId>jetty-maven-plugin</artifactId>
                <version>9.3.8.v20160314</version>
            </plugin>
        </plugins>
    </build>

Now we can create our controllers. In any MVC framework, the concepts are usually the same. You have a template, and you use a controller to populate a model that will be used to render the template. The term "render" here is used to mean create the final HTML page by combining the template and the model Take for example this template

**src/main/webapp/WEB-INF/jsp/index.jsp**

    <html>
        <head>
            <title>JSP Page</title>
        </head>
        <body>
            <h1>${it.hello} ${it.world}</h1>
        </body>
    </html>

This is a JSP file. JSP is just one of the template engines supported by Jersey. Here we are using two model variables, `hello`, and `world`. It is expected that these two variables will be in the model that is used to render this template. So let's add the controller

    package com.example.controller;
    
    import org.glassfish.jersey.server.mvc.Viewable;
    
    import javax.ws.rs.GET;
    import javax.ws.rs.Path;
    import javax.ws.rs.Produces;
    import javax.ws.rs.core.MediaType;
    import java.util.HashMap;
    import java.util.Map;
    
    @Path("/")
    public class HomeController {
    
        @GET
        @Produces(MediaType.TEXT_HTML)
        public Viewable index() {
            Map<String, String> model = new HashMap<>();
            model.put("hello", "Hello");
            model.put("world", "World");
            return new Viewable("/index", model);
        }
    }

You can see here we're populating the model with the properties `hello` and `world`. Also the controller method returns the name of the view template that is to be used, in this case `index`.  With this the framework knows to grab the "index" template, and use the model provided to render it. 

Now we just need to configure it. Add a `ResourceConfig` subclass with the following

    package com.example;

    import org.glassfish.jersey.server.ResourceConfig;
    import org.glassfish.jersey.server.mvc.jsp.JspMvcFeature;
    
    public class AppConfig extends ResourceConfig {
    
        public AppConfig() {
            packages("com.example.controller");
            property(JspMvcFeature.TEMPLATE_BASE_PATH, "/WEB-INF/jsp");
            register(JspMvcFeature.class);
        }
    }

There are three things going on here:

1. We use `packages` to tell Jersey to scan the `com.example.controller` package for  classes annotated with `@Path` to that it can register it. In this case, it registers our `HomeController`.

2. We are setting the base path for the framework to resolve templates. In this case we are telling Jersey to look in the `WEB-INF/jsp` for templates. You can see the `index.jsp` example above in the in this director. Also in the controller we return just the template name `index`. This will be used to find the template, by prefixing the configure base path, and suffixing an implicit `.jsp`

3. We need to register the feature that handles JSP rendering. As mentioned previously, JSP is not the only rendering engine supported by Jersey. There are a couple more supported out of the box.

The last thing we need to do is configure Jersey in the web.xml

    <filter>
        <filter-name>Jersey</filter-name>
        <filter-class>org.glassfish.jersey.servlet.ServletContainer</filter-class>
        <init-param>
            <param-name>javax.ws.rs.Application</param-name>
            <param-value>com.example.AppConfig</param-value>
        </init-param>
    </filter>

    <filter-mapping>
        <url-pattern>/*</url-pattern>
        <filter-name>Jersey</filter-name>
    </filter-mapping>

Here we are just configuring Jersey to use our `AppConfig` class. One very important thing to point out here, is the use of the `<filter>` instead of what you would normally see, a `<servlet>`. This is required when using JSP as the template engine.

Now we can run it. From the command line run `mvn jetty:run`. This will run the Maven Jetty plugin we configured previously. When you see "Started Jetty Server", the server is ready. Go to the browser URL `http://localhost:8080/`. Voila, "Hello World". Enjoy.

For more information, see the [Jersey Documentation for MVC Templates][1]


[1]: https://jersey.github.io/documentation/latest/mvc.html




