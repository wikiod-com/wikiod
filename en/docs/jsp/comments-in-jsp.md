---
title: "Comments in JSP"
slug: "comments-in-jsp"
draft: false
images: []
weight: 9989
type: docs
toc: true
---

There are multiple ways to add comment in JSP page, as it is java view technology which primarily focuses on view part of web application which includes html majorly and being java technology it can contain java code as well. So one needs to understand how to use proper comments in jsp.

There are 3 types of comments we can use in JSP page:

JSP comment 

HTML comment 

Java comment (which can used inside scriplets)

To see the source code of java class translated by container of JSP page:

Eclipse: `WORKSPACE\.metadata\.plugins\org.eclipse.wst.server.core\tmp0\work\Catalina\localhost\PROJECT_NAME\org\apache\jsp\PAGENAME_jsp.java`

Netbeans: Right-click on page and select view servlet

## JSP Comments Example
    <!DOCTYPE html>
    <html>
    <head>
    <meta http-equiv="Content-Type" content="text/html">
    <title>JSP Comments</title>
    </head>
    <body>
        <%-- JSP comments --%> -- Ignored by container, you can't see this comment in source code of JSP_java class translated by container, doesn't include in response

        <!-- HTML comments --> -- Ignored by browser, You can see this comment in source code of html page but browser ignores it to print on page, included in response

        <% // java comments %> -- Ignored by JRE, This comment can be seen in JSP_java class translated by container, doesn't include in response
        
        <!-- Today is <%= new java.util.Date() %> -->
        
    </body>
    </html>

When you run above code, output will be a blank page. To understand difference between these comments you need to see the source of web page and source code of JSP_Java class translated by container.

## HTML comment in a JSP
You can include HTML comments in JSPs as well. You use the basic html comment syntax:

    <!--Comment goes here-->

The difference between using JSP style comments and HTML style comments is the JSP ones will not be included when the HTML is generated and the HTML style comments will be. So it depends if you want them to be seen if someone views the source HTML. 

