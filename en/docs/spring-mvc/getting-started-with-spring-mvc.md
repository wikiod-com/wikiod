---
title: "Getting started with spring-mvc"
slug: "getting-started-with-spring-mvc"
draft: false
images: []
weight: 1
type: docs
toc: true
---

## First Spring-MVC Project
Create Dynamic Web project, provide following information's as stated below 

 1. Project name : DemoSpringMVCProject
 2. Target runtime : set as Apache Tomcat v7.0 server 

Click on finish, successfully we have created dynamic web project.

**Now we have to setup Spring-MVC framework :**

> 1. Create **web.xml** under  **' WebContent\WEB-INF\ '** folder 

    <?xml version="1.0" encoding="UTF-8"?>
    <web-app xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xmlns="http://java.sun.com/xml/ns/javaee" xmlns:web="http://java.sun.com/xml/ns/javaee/web-app_2_5.xsd" xsi:schemaLocation="http://java.sun.com/xml/ns/javaee http://java.sun.com/xml/ns/javaee/web-app_2_5.xsd" id="WebApp_ID" version="2.5">
    <display-name>Demo9</display-name>
  
    <servlet>
      <servlet-name>spring</servlet-name>
      <servlet-class>org.springframework.web.servlet.DispatcherServlet</servlet-class>
    </servlet>
  
    <servlet-mapping>
      <servlet-name>demo</servlet-name>
      <url-pattern>/</url-pattern>
    </servlet-mapping>
  
    </web-app>

 - Where DispatcherServlet class Intercepts incoming request and determines which controller handles the request.
 - We are going to use servlet-name ' **demo** ' while creating servlet.xml


> 2. Create **demo-servlet.xml** under   **' WebContent\WEB-INF\ '** folder

    <?xml version="1.0" encoding="UTF-8"?>
    <beans xmlns="http://www.springframework.org/schema/beans"
    xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance"
    xmlns:context="http://www.springframework.org/schema/context"
    xsi:schemaLocation="http://www.springframework.org/schema/beans http://www.springframework.org/schema/beans/spring-beans.xsd
        http://www.springframework.org/schema/context http://www.springframework.org/schema/context/spring-context.xsd">
    
    <context:component-scan base-package="com"></context:component-scan>

    <bean class="org.springframework.web.servlet.view.InternalResourceViewResolver">
     <property name="prefix" value="/WEB-INF/jsp/"></property>
     <property name="suffix" value=".jsp"></property>
    </bean>
  
    </beans>

 

- context:component-scan is used scan all controllers defined under **'com'** package.

 - ViewResolver Interface
Use to manage mapping between logical and actual views. Predefined implementation of view resolver are available to map the views. Ex: InternalResourceViewResolver, VelocityViewResolver.

- To search all jsp pages we have defined  **prefix** which is nothing but setter property , it's value is set as **'/WEB-INF/jsp/'**( folder path ) . **Suffix** which is nothing but getter property , it's value is set as **'.jsp'** ( search file with an extension .jsp )


> 3. Add Required Libraries:

Let us add Spring Framework and common logging API libraries in our project. To do this, right click on your project name DemoSpringMVCProject and then follow the following option available in context menu: Build Path -> Configure Build Path to display the Java Build Path window as follows:

Now use Add External JARs button available under Libraries tab to add the following core JARs from Spring Framework and Common Logging installation directories:

 - commons-logging-1.1.1

- spring-aop-4.1.6.RELEASE

- spring-aspects-4.1.6.RELEASE

- spring-beans-4.1.6.RELEASE

- spring-context-4.1.6.RELEASE

- spring-context-support-4.1.6.RELEASE

- spring-core-4.1.6.RELEASE

- spring-expression-4.1.6.RELEASE

- spring-instrument-4.1.6.RELEASE

- spring-instrument-tomcat-4.1.6.RELEASE

- spring-jdbc-4.1.6.RELEASE

- spring-jms-4.1.6.RELEASE

- spring-messaging-4.1.6.RELEASE

- spring-orm-4.1.6.RELEASE

- spring-oxm-4.1.6.RELEASE

- spring-test-4.1.6.RELEASE

- spring-tx-4.1.6.RELEASE

- spring-web-4.1.6.RELEASE

- spring-webmvc-4.1.6.RELEASE

- spring-webmvc-portlet-4.1.6.RELEASE

- spring-websocket-4.1.6.RELEASE


 **Let's move towards controller and jsp pages :**

1. Create a **com.demo.controller** package under **src** folder.
2. Create a **LoginController** class under **com.demo.controller** package


    package com.demo.controller;

    import javax.servlet.http.HttpServletRequest;
    import org.springframework.stereotype.Controller;
    import org.springframework.web.bind.annotation.RequestMapping;

    @Controller
    public class LoginController {
    
    @RequestMapping("/")
    public String startPage(){
        return "login";
    }

    @RequestMapping("/signin")
    public String handleRequest(HttpServletRequest request){
        String name = request.getParameter("name");
        String pass = request.getParameter("password");
        if(name.equals(pass))
        {
            return "welcome";
        }else{
            return "login";
        }
        
       }
    
    }

3. Create a **login.jsp and welcome.jsp** page under **' WebContent\WEB-INF\jsp\ '**

**login.jsp**

       <%@ page language="java" contentType="text/html; charset=ISO-8859-1"
       pageEncoding="ISO-8859-1"%>
       <!DOCTYPE html PUBLIC "-//W3C//DTD HTML 4.01 Transitional//EN" "http://www.w3.org/TR/html4/loose.dtd">
       <html>
       <head>
        <meta http-equiv="Content-Type" content="text/html; charset=ISO-8859-1">
        <title>Login Page</title>
        </head>
        <body>
          <form action="signin">
              <table>
                <tr>
                    <td>User Name : </td>    
                    <td><input type="text" name="name" id="name"/> </td>
                </tr> 
                <tr>
                    <td>Password: </td>    
                    <td><input type="text" name="password"  id="password"/> </td>
                </tr>  
                <tr>
                    <td colspan="2"><input type="submit" value="Login"/></td>    
                </tr> 
              </table>
          </form>
        </body>
        </html>


**welcome.jsp**


    <%@ page language="java" contentType="text/html; charset=ISO-8859-1"
    pageEncoding="ISO-8859-1"%>
    <!DOCTYPE html PUBLIC "-//W3C//DTD HTML 4.01 Transitional//EN" "http://www.w3.org/TR/html4/loose.dtd">
    <html>
    <head>
    <meta http-equiv="Content-Type" content="text/html; charset=ISO-8859-1">
    <title>Insert title here</title>
    </head>
    <body>
      <h1> Welcome to Spring MVC !!! </h1>
    </body>
    </html>

>Add DemoSpringMVCProject in localTomcat server and run it on server.

