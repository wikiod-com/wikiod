---
title: "Getting started with struts2"
slug: "getting-started-with-struts2"
draft: false
images: []
weight: 1
type: docs
toc: true
---

## Download
A distribution can be downloaded from the [Apache Struts website][1].
The full distribution contains:

 - the `struts2-core.jar` file
 - related dependencies
 - example applications
 - a copy of the documentation in HTML format
 - the complete source code.

**Blank Application**

The blank web application in the distribution's apps directory is meant as a template. 

There is even a simple batch file in the source code directory that we can use to recompile the application in place. 

We can make a copy of the `blank.war`, deploy it to our container, and use the exploded copy as the basis for our application. 

  [1]: http://struts.apache.org/downloads.html

## New application from Maven Archetype
For those of us using Maven as a build system, we can use the Maven Archetype to create a new application.

First consult the maven archetype catalog

    mvn archetype:generate -DarchetypeCatalog=http://struts.apache.org/

you can use one of the following achetypes:


**The Blank Convention Archetype (struts2-archetype-convention)**

It has the following features:

 - Convention-based validation
 - Example actions
 - Package-level resource bundle
 - Unit-testing
 - Google AppEgine aware

to download it trough maven run the command

    mvn archetype:generate -B -DgroupId=com.mycompany.mysystem \
                                -DartifactId=myWebApp \
                                -DarchetypeGroupId=org.apache.struts \
                                -DarchetypeArtifactId=struts2-archetype-convention \
                                -DarchetypeVersion=<CURRENT_STRUTS_VERSION> \
                                -DremoteRepositories=http://struts.apache.org

**The Blank Archetype (struts2-archetype-blank)**

It has the following features:
    
 - XML-based configuration, demonstrates including additional config file
 - Example actions
 - Package-level resource bundle
 - XML-based validation
 - Unit-testing

to download it trough maven run the command

    mvn archetype:generate -B -DgroupId=com.mycompany.mysystem \
                                -DartifactId=myWebApp \
                                -DarchetypeGroupId=org.apache.struts \
                                -DarchetypeArtifactId=struts2-archetype-blank \
                                -DarchetypeVersion=<CURRENT_STRUTS_VERSION> \
                                -DremoteRepositories=http://struts.apache.org

**The Starter Archetype (struts2-archetype-starter)**

It has the following features:
    
 - Sitemesh integration
 - Action example (instantiated both through Spring and Struts)
 - Spring integration
 - Validation example (action and action-alias level)
 - Conversion example (global and action level)
 - Resource bundle (both global, action and package level)

to download it trough maven run the command

    mvn archetype:generate -B -DgroupId=com.mycompany.mysystem \
                                -DartifactId=myWebApp \
                                -DarchetypeGroupId=org.apache.struts \
                                -DarchetypeArtifactId=struts2-archetype-starter \
                                -DarchetypeVersion=<CURRENT_STRUTS_VERSION> \
                                -DremoteRepositories=http://struts.apache.org

**The AngularJS Archetype (struts2-archetype-angularjs)**

It has the following features:

 - Convention-based Action Configuration
 - Example actions with JSON Result
 - Example for AngularJS and Struts2 Integration

to download it trough maven run the command

    mvn archetype:generate -B -DgroupId=com.mycompany.mysystem \
                                -DartifactId=myWebApp \
                                -DarchetypeGroupId=org.apache.struts \
                                -DarchetypeArtifactId=struts2-archetype-angularjs \
                                -DarchetypeVersion=<CURRENT_STRUTS_VERSION> \
                                -DremoteRepositories=http://struts.apache.org

**The Portlet Blank Archetype (struts2-archetype-portlet)**

It has the following features:

 -     View, Edit, and Help mode examples
 - Simple form for preferences in Edit mode
 - Can be deployed as a servlet or portlet application
 - Can use Maven Jetty plugin to deploy with the pluto-embedded profile
   (usage 'mvn jetty:run -Ppluto-embedded', then access
   http://localhost:8080/<artifactId>/pluto/index.jsp)

to download it trough maven run the command

    mvn archetype:generate -B -DgroupId=com.mycompany.mysystem \
                                -DartifactId=myWebApp \
                                -DarchetypeGroupId=org.apache.struts \
                                -DarchetypeArtifactId=struts2-archetype-portlet \
                                -DarchetypeVersion=<CURRENT_STRUTS_VERSION> \
                                -DremoteRepositories=http://struts.apache.org

**The Portlet Database Archetype (struts2-archetype-dbportlet)**

It has the following features:

 -     Uses Spring and Hsql to show a real database query
 - Builtin caching of query results
 - View, Edit, and Help mode examples
 - Simple form for preferences in Edit mode
 - Can be deployed as a servlet or portlet application
 - Can use Maven Jetty plugin to deploy as a servlet webapp

to download it trough maven run the command

    mvn archetype:generate -B -DgroupId=com.mycompany.mysystem \
                                -DartifactId=myWebApp \
                                -DarchetypeGroupId=org.apache.struts \
                                -DarchetypeArtifactId=struts2-archetype-dbportlet \
                                -DarchetypeVersion=<CURRENT_STRUTS_VERSION> \
                                -DremoteRepositories=http://struts.apache.org

**The Plugin Archetype (struts2-archetype-plugin)**

It has the following features:

 - Example new result type
 - Example XML-based configuration

to download it trough maven run the command

    mvn archetype:generate -B -DgroupId=com.mycompany.mysystem \
                                -DartifactId=myPlugin \
                                -DarchetypeGroupId=org.apache.struts \
                                -DarchetypeArtifactId=struts2-archetype-plugin \
                                -DarchetypeVersion=<CURRENT_STRUTS_VERSION> \
                                -DremoteRepositories=http://struts.apache.org

