---
title: "JSF Templates"
slug: "jsf-templates"
draft: false
images: []
weight: 9978
type: docs
toc: true
---

JSF provides special tags to create common layout for a web application called **facelets** tags. These tags gives flexibility to manage common parts of a multiple pages at one place.

Namespaces:

    xmlns:h="http://xmlns.jcp.org/jsf/html"
    xmlns:ui="http://xmlns.jcp.org/jsf/facelets"



## How to create a template
## Setting up a template for one application

Create a file named `template.xhtml` under the `/WEB-INF` folder, that way the template files will be accessible only for the framework.

`/WEB-INF/template.xhtml`

    <!DOCTYPE html>
    <html lang="en"
        xmlns="http://www.w3.org/1999/xhtml"
        xmlns:h="http://xmlns.jcp.org/jsf/html"
        xmlns:ui="http://xmlns.jcp.org/jsf/facelets">
    
        <h:head>
            <title><ui:define name="title">Default title</ui:define></title>
        </h:head>
    
        <h:body>
            <!-- Some styles that we might include for our whole application -->
            <h:outputStylesheet name="css/template.css" />

            <!-- Shared content for the application, e.g. a header, this can be an include -->
            <div>
                Application Header
            </div>
    
            <!-- The content we want to define in our template client -->
            <div>
                <ui:insert name="content" />
            </div>
        </h:body>
    </html>

This file will act as a template for the application. Now we'll define a specific view in our view directory.

`/home.xhtml`

    <ui:composition template="/WEB-INF/template.xhtml"
        xmlns="http://www.w3.org/1999/xhtml"
        xmlns:ui="http://xmlns.jcp.org/jsf/facelets">

        <ui:define name="title">Home</ui:define>

        <ui:define name="content">
            Welcome to the application!
        </ui:define>
    </ui:composition>

Have a look at the `template` attribute in this client view, this tells JSF to use the template we want. Then, using `<ui:define>` we define specific content to be inserted where it is told in the template. Accessing `/home.xhtml` in client will render the whole result.

