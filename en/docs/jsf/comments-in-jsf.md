---
title: "Comments in JSF"
slug: "comments-in-jsf"
draft: false
images: []
weight: 9817
type: docs
toc: true
---

JSF as a markup language, supports comments of some parts of code, but we have be carefully, because if we use a normal HTML comment code like this:

    <!-- I want to comment the next button -->
    <!--        
       <h:commandButton value="Push" onclick="alert('Hello');" />
    -->
It's possible that it doesn't has commented anything. This is because JSF process this code as default, even if is commented between tags ``<!--`` and ``-->``.

There are two solutions to comment any JSF code

## Syntax
 - <ui:remove> JSF code that you want to comment </ui:remove>

You can find more information at Oracle documentation:

 - [<ui:remove>][1] at oracle.com
 - [SKIP_COMMENTS][2] at facelets.java.net


  [1]: http://docs.oracle.com/javaee/6/javaserverfaces/2.0/docs/pdldocs/facelets/ui/remove.html
  [2]: https://facelets.java.net/nonav/docs/dev/docbook.html#config-faces-ri

## Use tag <ui:remove>
We need to use tag ``<ui:remove>`` and ``</ui:remove>`` between any JSF code that we want to comment it.

    <ui:remove>
        <h:outputLabel value="Yeah, I'm really commented" />
    </ui:remove>

Of course you need add this xmlns to your header html tag. Check this minimal full example:
  
    <html xmlns="http://www.w3.org/1999/xhtml"
        xmlns:h="http://java.sun.com/jsf/html"
        xmlns:ui="http://java.sun.com/jsf/facelets">
        
        <ui:remove>
            <h:outputLabel value="Yeah, I'm really commented" />
        </ui:remove>
        
    </html>

## Configure facelets.SKIP_COMMENTS
You must to add to web.xml a configuration tag like this:

    <context-param>
        <param-name>facelets.SKIP_COMMENTS</param-name>
        <param-value>true</param-value>
    </context-param>

Now you can use normal HTML comments tag ``<!--`` and ``-->``
    
    <!--
        <h:outputLabel value="Yeah, I'm really commented" />
    -->

Previuosly full example with facelets.SKIP_COMMENTS configured in web.xml will be:
    
    <html xmlns="http://www.w3.org/1999/xhtml"
        xmlns:h="http://java.sun.com/jsf/html"
        xmlns:ui="http://java.sun.com/jsf/facelets">
        
        <!--
            <h:outputLabel value="Yeah, I'm really commented" />
        -->
        
    </html>

