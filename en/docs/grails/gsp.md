---
title: "GSP"
slug: "gsp"
draft: false
images: []
weight: 9985
type: docs
toc: true
---

## Parameters
| Variables and scopes | Details |
| ---------- | ------- |
|application | [ServletContext][1] instance|
|applicationContext | Spring [ApplicationContext][2] instance|
|flash | The [flash][3] object|
|grailsApplication | [GrailsApplication][4] instance|
|out | response writer for writing to the output stream|
|params | [params][5] object for retrieving request parameters|
|request | [HttpServletRequest][6] instance|
|response | [HttpServletResponse][7] instance|
|session | [HttpSession][8] instance|
|webRequest | [GrailsWebRequest][9] instance|


  [1]: http://docs.oracle.com/javaee/7/api/javax/servlet/ServletContext.html
  [2]: https://docs.spring.io/spring/docs/current/javadoc-api/org/springframework/context/ApplicationContext.html
  [3]: http://docs.grails.org/latest/ref/Controllers/flash.html
  [4]: http://docs.grails.org/3.0.x/api/grails/core/GrailsApplication.html
  [5]: http://docs.grails.org/latest/ref/Controllers/params.html
  [6]: http://docs.oracle.com/javaee/7/api/javax/servlet/http/HttpServletRequest.html
  [7]: http://docs.oracle.com/javaee/7/api/javax/servlet/http/HttpServletResponse.html
  [8]: http://docs.oracle.com/javaee/7/api/javax/servlet/HttpSession
  [9]: http://grails.github.io/grails-doc/3.0.x/api/org/grails/web/servlet/mvc/GrailsWebRequest.html

## Expressions
In GSP the `<%= %>` syntax is rarely used due to the support for **GSP expressions**. 

A GSP expression is similar to a **JSP EL** expression or a **Groovy GString** and takes the form `${expr}`:

    <html>
      <body>
        Hello ${params.name}
      </body>
    </html>
    
However, unlike JSP EL you can have any Groovy expression within the `${..}` block.

Any Groovy expression can be interpolated in all string literals, apart from single and triple single quoted strings. Interpolation is the act of replacing a placeholder in the string with its value upon evaluation of the string. The placeholder expressions are surrounded by ${} or prefixed with $ for dotted expressions. The expression value inside the placeholder is evaluated to its string representation when the GString is passed to a method taking a String as argument by calling toString() on that expression.

## Basics
GSP supports the usage of `<% %>` scriptlet blocks to embed Groovy code (this is discouraged):

    <html>
       <body>
         <% out << "Hello GSP!" %>
       </body>
    </html>
    
You can also use the `<%= %>` syntax to output values, like in JSP:

    <html>
       <body>
         <%="Hello GSP!" %>
       </body>
    </html>
    
GSP also supports JSP-style server-side comments too:

    <html>
       <body>
         <%-- This is my comment --%>
         <%="Hello GSP!" %>
       </body>
    </html>


## GSP Tags
There are variety of gsp tags available which can be used to create forms, textfield, radio buttons, check boxes, if-else, for each etc.


**<g:if>**

    <g:if test="${session.role == 'admin'}">
       <%-- show administrative functions --%>
    </g:if>
    <g:else>
       <%-- show basic functions --%>
    </g:else>


**<g:each>**

    <g:each in="${[1,2,3]}" var="num">
      <p>Number ${num}</p>
    </g:each>

**form**

    <g:form name="myForm" url="[controller:'book',action:'list']">...</g:form>

**textField**
    
    <g:textField name="myField" value="${myValue}" />

**radio**

    <g:radio name="myGroup" value="1"/>

Follow this link for more info - http://docs.grails.org/latest/guide/theWebLayer.html#tags 

