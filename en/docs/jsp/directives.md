---
title: "Directives"
slug: "directives"
draft: false
images: []
weight: 9984
type: docs
toc: true
---

## Syntax
 - <%@ directiveName attributeName="value"%>



## Simple example
Directives, as the name suggests, are direction or instructions for the container to follow when translating a JSP to a servlet. There are 3 directives namely `page`, `include` and `taglib` which you can use in your JSP. 

Below is a simple example of using `page` directive:


    <%@ page isErrorPage="true" %>

This would instruct the container that the JSP file containing this line is an error page.




Directives can be used be put anywhere in your JSP file, but the convention is to put at the top/beginning of the JSP file



