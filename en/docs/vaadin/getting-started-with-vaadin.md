---
title: "Getting started with vaadin"
slug: "getting-started-with-vaadin"
draft: false
images: []
weight: 1
type: docs
toc: true
---

## Create Vaadin project with Maven
With Maven you can create Vaadin project with `vaadin-archetype-application` archetype. You can also add that archetype in IDE to create maven project with IDE.

    mvn archetype:generate 
       -DarchetypeGroupId=com.vaadin 
       -DarchetypeArtifactId=vaadin-archetype-application 
       -DarchetypeVersion=7.6.8 
       -DgroupId=myvaadin.project 
       -DartifactId=DemoVaadinProject 
       -Dversion=0.1 
       -Dpackaging=war 

Once you execute above command you will have following project structure.

    DemoVaadinProject 
      |-src
         |-main
             |-java
             |   |-myvaadin
             |         |-project
             |            |-MyUI.java
             |-resource
             |    |-myvaadin
             |         |-project
             |            |-MyAppWidgetset.gwt.xml
             |-webapps
                  |- VAADIN
                       |-theme   
                          |- mytheme.scss
                          |- addons.scss
                          |- styles.scss
                          |- favicon.ico
                            
         
The created default maven project can be imported in IDE directly. To run the maven application we must compile the default widget sets of vaadin. 

Note that we can directly use following maven command to package vaadin application and it will compile the widgetsets by default. You can use maven jetty plugin to deploy the vaadin application on Jetty.

    cd path/to/DemoVaadinProject
    mvn package jetty:run

This will deploy default application and start running it on default port `8080`. You can access the deployed application at http://localhost:8080.

It is ready to run without any changes. By default Vaadin archetype adds default theme, widgetset xml and `MyUI` class which is an entry point for vaadin application. 

In browser we will see following form.

[![enter image description here][1]][1]


  [1]: http://i.stack.imgur.com/QMs9t.png

## Create Vaadin Project in Eclipse
Vaadin plug-in for eclipse provides a quick way to build vaadin project with Apache Ivy dependency manager. Vaadin's [documentation][1] explains how to create vaadin project with the help of Eclipse plugin.

To install the plug-in just go to eclipse marketplace and search *vaadin*. Current version of the plug-in is `3.0.0`. 

After installing the plug-in you will have following quick features,

 - Create `Vaadin6` or `Vaadin 7` project (*Default dependency manager is Ivy*)
 - Compile Widgetsets (*To compile client side widgets*)
 - Compile Theme (*To compile Theme to build final CSS*)
 - Create Widget (*To build your custom widget*)
 - Create Vaadin Theme

So, after setting up the plug-in, just create new Vaadin project with minimal configuration. You can also specify version of vaadin while creating the project.

 - `File > New > Vaadin7 Project`
 - Specify version of vaadin to be used in project
 - Specify Target Runtime you want to use
 - Finish!

It will take time to download all the jars required for vaadin,once Ivy resolve all the dependencies. You can directly run the project on the server and you will see one `Button` with `Click Me` in the browser screen. Kindly note that Vaadin7 is compatible with Java 6 and newer.


  [1]: https://vaadin.com/docs/-/part/framework/getting-started/getting-started-first-project.html

## Vaadin Plugin for Netbeans
Creating a Project with the NetBeans IDE

In the following, we walk you through the creation of a Vaadin project in NetBeans and show how to run it.

Installation of NetBeans and the Vaadin plugin is covered in Installing the NetBeans IDE and Plugin.

Without the plugin, you can most easily create a Vaadin project as a Maven project using a Vaadin archetype. You can also create a Vaadin project as a regular web application project, but it requires many manual steps to install all the Vaadin libraries, create the UI class, configure the servlet, create theme, and so on.

Creating a Project
------------------

    

 1. Select **File ▸ New Project…​** from the main menu or press
    Ctrl+Shift+N. 
 2. In the New Project window that opens, select the
    Vaadin category and one of the Vaadin archetypes from the right.
[![enter image description here][1]][1]

    The archetypes are described in more detail in Overview of Maven Archetypes.

 3. In the **Name and Location** step, enter the project parameters.
[![enter image description here][2]][2]

    **Project Name**

       A project name. The name must be a valid identifier that may only contains alphanumerics, minus, and underscore. It is appended to the group ID to obtain the Java package name for the sources.

    **Project Location**

       Path to the folder where the project is to be created.

    **Group Id**

       A Maven group ID for your project. It is normally your organization domain name in reverse order, such as com.example. The group ID is also used as a prefix for the Java source package, so it should be Java-compatible package name.

    **Version**

       Initial version of your application. The number must obey the Maven version numbering format.

    **Package**

       The Java package name to put sources in.
    
    **Additional Creation Properties**

    The properties control various names. They are specific to the archetype you chose.

    Click Finish.

    Creating the project can take a while as Maven loads all the needed dependencies.

Exploring the Project
---------------------

The project wizard has done all the work for you: a UI class skeleton has been written to the src directory. The project hierarchy shown in the Project Explorer is shown in [A new Vaadin project in NetBeans][3].
[![enter image description here][4]][4]
Figure 1. A new Vaadin project in NetBeans

**mytheme**

   The theme of the UI. See Themes for information about themes.

**MyUI.java**

   The UI class, which is the main entry-point of your application. See Server-Side Applications for information about the basic structure of Vaadin applications.

The Vaadin libraries and other dependencies are managed by Maven. Notice that the libraries are not stored under the project folder, even though they are listed in the Java Resources ▸ Libraries ▸ Maven Dependencies virtual folder.
Running the Application

Once created, you can run it in a server as follows.

    

 1. In Projects tab, select the project and click in the Run Project
    button in the tool bar (or press F6).
 2. In the Select deployment server window, select a server from the Server list. It should show either GlassFish or Apache Tomcat or
    both, depending on what you chose in NetBeans installation.

    [![netbeans server][5]][5]

    Also, select Remember Permanently if you want to use the same server also in future while developing applications.

    Click OK.

The widget set will be compiled at this point, which may take a while.

If all goes well, NetBeans starts the server in port 8080 and, depending on your system configuration, launches the default browser to display the web application. If not, you can open it manually, for example, at http://localhost:8080/myproject. The project name is used by default as the context path of the application.

Now when you edit the UI class in the source editor and save it, NetBeans will automatically redeploy the application. After it has finished after a few seconds, you can reload the application in the browser.


  [1]: http://i.stack.imgur.com/BrCdE.png
  [2]: http://i.stack.imgur.com/j1aSA.png
  [3]: https://vaadin.com/docs/-/part/framework/getting-started/getting-started-netbeans.html#figure.getting-started.netbeans.exploring
  [4]: http://i.stack.imgur.com/fsN19.png
  [5]: http://i.stack.imgur.com/R63U4.png

## First program - "Hello World"
Copy paste this code and launch your program :

   

    @Theme(ValoTheme.THEME_NAME) //[optional] adds Vaadin built in theming 
    public class SampleUI extends UI {
        
    @Override
    protected void init(VaadinRequest request) {
             final VerticalLayout rootLayout = new VerticalLayout();
             Label label = new Label("Hello World"!);
             rootLayout.addComponent(label);
             setContent(rootLayout);
         }
     }

After lauching was successful, please navigate to [localhost:8080/yourApplicationName][1]
or http://localhost:8080/ to see your app is up and running.

  [1]: http://localhost:8080/yourApplicationName
  [2]: http://localhost:8080/


## Installation or Setup
https://vaadin.com/framework/get-started

