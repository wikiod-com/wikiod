---
title : servlets Tutorial
slug : servlets-tutorial
weight : 9919
draft : false
images : []
type : docs
---

A Servlet is a **Java application programming interface (API)** running on the server machine which can intercept requests made by the client and can generate/send a response accordingly. A well-known example is the [`HttpServlet`][1] which provides methods to hook on [HTTP][2] requests using the popular [HTTP methods][3] such as [`GET`][4] and [`POST`][5]. You can configure `HttpServlet`s to listen on a certain HTTP URL pattern, which is configurable in `web.xml`, or more recently with [Java EE 6][6], with [`@WebServlet`][7] annotation. Many Java EE web frameworks are built on top of servlets, such as JSF, JAX-RS, Spring MVC, Struts, Wicket, etcetera. See also http://stackoverflow.com/questions/2095397/what-is-the-difference-between-jsf-servlet-and-jsp/

  [1]: http://docs.oracle.com/javaee/7/api/javax/servlet/http/HttpServlet.html
  [2]: http://www.w3.org/Protocols/rfc2616/rfc2616.html
  [3]: http://www.w3.org/Protocols/rfc2616/rfc2616-sec9.html
  [4]: http://www.w3.org/Protocols/rfc2616/rfc2616-sec9.html#sec9.3
  [5]: http://www.w3.org/Protocols/rfc2616/rfc2616-sec9.html#sec9.5
  [6]: http://docs.oracle.com/javaee/6/tutorial/doc/bnafd.html
  [7]: http://docs.oracle.com/javaee/7/api/javax/servlet/annotation/WebServlet.html


