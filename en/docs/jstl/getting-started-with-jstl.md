---
title: "Getting started with jstl"
slug: "getting-started-with-jstl"
draft: false
images: []
weight: 1
type: docs
toc: true
---

## Installation
JSTL is part of the Java EE API and included in Java EE application servers such as [WildFly][9], [TomEE][10], [GlassFish][11], but not in barebones servletcontainers such as [Tomcat][12] and [Jetty][13]. JSTL are the taglibs which you import from `http://java.sun.com/jsp/jstl/*` namespace. JSTL must not be confused with a "[custom JSP tag library][14]" (wherein you define a `.tld` file *yourself*). JSTL must also not be confused with taglibs of 3rd party frameworks such as JSF, Spring MVC, Struts, Displaytag, etcetera. JSTL must also not be confused with [Expression Language (EL)][15] (which are those `${}` things).

 1. *Only* when your servletcontainer doesn't ship with JSTL builtin (e.g. Tomcat and Jetty), then just drop the [jstl-1.2.jar][16] straight in webapp's `/WEB-INF/lib` folder (which is covered by the default webapp's classpath, so in a bit smart IDE you don't need to do anything else). For starters, do **not** fiddle around in IDE project's *Build Path* setting. This is Wrong.

    In case you're using Maven, [this][17] is the coordinate:

    <!-- language: xml -->
    
        <dependency>
            <groupId>javax.servlet</groupId>
            <artifactId>jstl</artifactId>
            <version>1.2</version>
        </dependency>

    This is by the way the JSTL API bundled with Apache's JSTL implementation in a single JAR flavor. This does **not** require the `standard.jar` (it's for JSTL 1.1 only). Note that there's also a [`jstl:jstl`][18] dependency, but it's exactly the same file, only with a wrong group ID. Further there's also a [`javax.servlet.jsp.jstl:jstl`][19] dependency, but it is empty.

 2. Declare the taglib in JSP file with the right TLD URI. You can find [here][1] the TLD documentation that applies to both JSTL 1.1 and JSTL 1.2. Click the taglib of interest to get the declaration examples. For example the [JSTL core taglib][2]

    <!-- language: xhtml -->

        <%@ taglib prefix="c" uri="http://java.sun.com/jsp/jstl/core" %>

    If you're using Facelets or JSPX instead of JSP, it should be declared as XML namespace instead

    <!-- language: xhtml -->

        <anyxmlelement xmlns:c="http://java.sun.com/jsp/jstl/core">

You only need to ensure that you have no duplicates of older JSTL versions in the classpath (includes JDK/JRE's `/lib` and server's `/lib`) to avoid collisions. If you have full admin-level control over the server, then you could also place the JAR file in server's `/lib` instead of webapp's `/WEB-INF/lib` so that they get applied to all deployed webapps. At least do NOT extract the JAR file(s) and clutter the classpath with their contents (the loose TLD files) and/or declare the taglibs in *your* webapp's `web.xml` as some poor online tutorials suggest.

  [1]: http://docs.oracle.com/javaee/5/jstl/1.1/docs/tlddocs/
  [2]: http://docs.oracle.com/javaee/5/jstl/1.1/docs/tlddocs/c/tld-summary.html
  [9]: http://wildfly.org
  [10]: http://tomee.apache.org
  [11]: http://glassfish.org
  [12]: http://tomcat.apache.org
  [13]: http://www.eclipse.org/jetty/
  [14]: http://docs.oracle.com/javaee/5/tutorial/doc/bnalj.html
  [15]: http://stackoverflow.com/tags/el/info
  [16]: http://central.maven.org/maven2/javax/servlet/jstl/1.2/jstl-1.2.jar
  [17]: http://mvnrepository.com/artifact/javax.servlet/jstl/1.2
  [18]: http://mvnrepository.com/artifact/jstl/jstl/1.2
  [19]: http://mvnrepository.com/artifact/javax.servlet.jsp.jstl/jstl/1.2


