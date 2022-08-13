---
title: "Getting started with asp-classic"
slug: "getting-started-with-asp-classic"
draft: false
images: []
weight: 1
type: docs
toc: true
---

## Structure of a Simple ASP Page
<!-- language: lang-none -->    

    <%@ Language="VBScript" CodePage = 65001 %>
    <%
    Option Explicit
    Response.Charset = "UTF-8"
    Response.CodePage = 65001
    %>
    <!doctype html>
    <html>
      <head>
        <title>My First Classic ASP Page</title>
      </head>
    
      <body>
        <%="Hello World"%>
      </body>
    </html>

This is a very basic example of a Classic ASP page that returns the phrase "Hello World" to the browser along with the rest of the standard HTML. The HTML portions are static, i.e. the server will send them to the browser as-is. The parts delimited by `<% %>` are what the server will actually process before sending it to the client. 

Note that the `<%="stuff"%>` syntax is shorthand for `<%Response.Write "stuff"%>`.

## Hello World
<!-- language: lang-vbs -->

    <!doctype html>
    <html>
      <head>
        <title>Example Page</title>
      </head>
      <body>
    <%
      'This is where the ASP code begins
      'ASP will generate the HTML that is passed to the browser
      'A single quote denotes a comment, so these lines are not executed
      'Since this will be HTML, we included the html and body tags
      'for Classic ASP we use Response.Write() to output our text
      'like this
      
      Response.Write ("Hello world")
      
      'Now we will end the ASP block and close our body and html tags
    %>
      </body>
    </html>

When response is sent from the Server to the Browser the output will be like this: 

<!-- language: lang-html --> 

    <!doctype html>
    <html>
      <head>
        <title>Example Page</title>
      </head>
      <body>
     Hello world
      </body>
    </html>

