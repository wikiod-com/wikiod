---
title: "Getting started with wicket"
slug: "getting-started-with-wicket"
draft: false
images: []
weight: 1
type: docs
toc: true
---

## Installation or Setup
My first Wicket setup, app showing Hello World on home screen:

    import org.apache.wicket.protocol.http.WebApplication;
    public class HelloWorldApplication extends WebApplication {
        
        public HelloWorldApplication() {
        }
        
        @Override
        public Class getHomePage() {
            return HelloWorld.class;
        }
    }

HelloWorld.java

    import org.apache.wicket.markup.html.WebPage;
    import org.apache.wicket.markup.html.basic.Label;
    public class HelloWorld extends WebPage {
        public HelloWorld() {
            add(new Label("message", "Hello World!"));
        }
    }

HelloWorld.html

    <html>
       <body>
           <span wicket:id="message">Message goes here</span>
        </body>
    </html>


web.xml

    <?xml version="1.0" encoding="UTF-8"?>
    <!DOCTYPE web-app
      PUBLIC "-//Sun Microsystems, Inc.//DTD Web Application 2.3//EN"
      "http://java.sun.com/dtd/web-app_2_3.dtd">
    <web-app>
        <display-name>My first Wicket App</display-name>
        <filter>
            <filter-name>HelloWorldApplication</filter-name>
            <filter-class>org.apache.wicket.protocol.http.WicketFilter</filter-class>
            <init-param>
              <param-name>applicationClassName</param-name>
              <param-value>org.apache.wicket.examples.helloworld.HelloWorldApplication</param-value>
            </init-param>
        </filter>
        <filter-mapping>
            <filter-name>HelloWorldApplication</filter-name>
            <url-pattern>/*</url-pattern>
        </filter-mapping>
    </web-app>

pom.xml

        <dependency>
            <groupId>org.apache.wicket</groupId>
            <artifactId>wicket-core</artifactId>
            <version>${wicket.version}</version>
        </dependency>

