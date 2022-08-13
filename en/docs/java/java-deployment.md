---
title: "Java deployment"
slug: "java-deployment"
draft: false
images: []
weight: 9914
type: docs
toc: true
---

There are a variety of technologies for "packaging" Java applications, webapps and so forth, for deployment to the platform on which they will run.  They range from simple library or executable `JAR` files, `WAR` and `EAR` files, through to installers and self-contained executables.

At the most fundamental level, a Java program can be deployed by copying a compiled class (i.e. a ".class" file) or a directory tree containing compiled classes.  However Java is normally deployed in one of the following ways:

  - By copying a JAR file or collection of JAR files to the system where they will be executed; e.g. using `javac`.

  - By copying or uploading a WAR, EAR or similar file to a "servlet container" or "application server".

  - By running some kind of application installer that automates the above.  The installer might also install an embedded JRE.

  - By putting the JAR files for the application onto a web server to allow them to be launched using Java WebStart.

The Creating JAR, WAR and EAR files example summarizes the different ways to create these files.

There are numerous open source and commercial "installer generator" and 
"EXE generator" tools for Java.  Similarly, there are tools for obfuscating Java class files (to make reverse engineering harder) and for adding runtime license checking.  These are all out of scope for the "Java Programming Language" documentation.

## Making an executable JAR from the command line
To make a jar, you need one or more class files. This should have a main method if it is to be run by a double click.

For this example, we will use:

    import javax.swing.*;
    import java.awt.Container;
    
    public class HelloWorld {
    
        public static void main(String[] args) {
            JFrame f = new JFrame("Hello, World"); 
            JLabel label = new JLabel("Hello, World");
            Container cont = f.getContentPane();
            cont.add(label);
            f.setSize(400,100); 
            f.setVisible(true);
            f.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
        }
    
    }

It has been named HelloWorld.java

Next, we want to compile this program. 

You may use any program you want to do this. To run from the command line, 
see the documentation [on compiling and running your first java program.][1]

Once you have HelloWorld.class, make a new folder and call it whatever you want.

Make another file called manifest.txt and paste into it

    Main-Class: HelloWorld
    Class-Path: HelloWorld.jar

Put it in the same folder with HelloWorld.class \
Use the command line to make your current directory (`cd C:\Your\Folder\Path\Here` on windows) your folder.

Use Terminal and change directory to the directory (`cd /Users/user/Documents/Java/jarfolder` on Mac) your folder

When that is done, type in `jar -cvfm HelloWorld.jar manifest.txt HelloWorld.class` and press enter. This makes a jar file (in the folder with your manifest and HelloWorld.class) using the .class files specified and named HelloWorld.jar. See the Syntax section for information about the options (like -m and -v). \
After these steps, go to your directory with the manifest file and you should find HelloWorld.jar \
Clicking on it should display `Hello, World` in a text box.


  [1]: https://www.wikiod.com/java/getting-started-with-java-language

## Creating JAR, WAR and EAR files
The JAR, WAR and EAR files types are fundamentally ZIP files with a "manifest" file and (for WAR and EAR files) a particular internal directory / file structure.

The recommended way to create these files is to use a Java-specific build tool which "understands" the requirements for the respective file types.  If you don't use a build tool, then IDE "export" is the next option to try.

(*Editorial note: the descriptions of how to create these files are best placed in the documentation for the respective tools.  Put them there.  Please show some self-restraint and DON'T shoe-horn them into this example!*)

Creating JAR and WAR files using Maven
--------------------------------------

Creating a JAR or WAR using Maven is simply a matter of putting the correct `<packaging>` element into the POM file; e,g,

    <packaging>jar</packaging>

or

    <packaging>war</packaging>

For more details.  Maven can be configured to create "executable" JAR files by adding the requisite information about the entry-point class and external dependencies as plugin properties for the maven jar plugin.  There is even a plugin for creating "uberJAR" files that combine an application and its dependencies into a single JAR file.

Please refer to the Maven documentation ( https://www.wikiod.com/maven )for more information.

Creating JAR, WAR and EAR files using Ant
-----------------------------------------

The Ant build tool has separate "tasks" for building JAR, WAR and EAR.  Please refer to the Ant documentation ( https://www.wikiod.com/ant ) for more information.

Creating JAR, WAR and EAR files using an IDE
--------------------------------------------

The three most popular Java IDEs all have built-in support for creating deployment files.  The functionality is often described as "exporting".

  - Eclipse - https://www.wikiod.com/eclipse
  - NetBeans - https://www.wikiod.com/netbeans
  - Intellij-IDEA - https://www.wikiod.com/intellij-idea/exporting

Creating JAR, WAR and EAR files using the `jar` command.
--------------------------------------------------------

It is also possible to create these files "by hand" using the `jar` command.  It is simply a matter of assembling a file tree with the correct component files in the correct place, creating a manifest file, and running `jar` to create the JAR file.

Please refer to the `jar` command Topic ( https://www.wikiod.com/java ) for more information 

## Introduction to Java Web Start
The Oracle Java Tutorials summarize [Web Start][1] as follows:

> Java Web Start software provides the power to launch full-featured applications with a single click. Users can download and launch applications, such as a complete spreadsheet program or an Internet chat client, without going through lengthy installation procedures.

Other advantages of Java Web Start are support for signed code and explicit declaration of platform dependencies, and support for code caching and deployment of application updates.

Java Web Start is also referred to as JavaWS and JAWS.  The primary sources of information are:

- [The Java Tutorials - Lesson: Java Web Start][1]
- [Java Web Start Guide][2]
- [Java Web Start FAQ][2]
- [JNLP Specification][2]
- [`javax.jnlp` API Documentation][2]
- [Java Web Start Developers Site][2]

Prerequisites
-------------

At a high level, Web Start works by distributing Java applications packed as JAR files from a remote webserver.  The prerequisites are:

  - A pre-existing Java installation (JRE or JDK) on the target machine where the application is to run.  Java 1.2.2 or higher is required:
    - From Java 5.0 onwards, Web Start support is included in the JRE / JDK.
    - For earlier releases, Web Start support is installed separately.  
    - The Web Start infrastructure includes some Javascript that can be included in a web page to assist the user to install the necessary software.

  - The webserver that hosts the software must be accessible to the target machine.

  - If the user is going to launch a Web Start application using a link in a web page, then:
      - they need a compatible web browser, and 
      - for modern (secure) browsers, they need to be told how to tell the browser to allow Java to run ... without compromising web browser security.

An example JNLP file
--------------------

The following example is intended to illustrate the basic functionality of JNLP.

    <?xml version="1.0" encoding="UTF-8"?>
    <jnlp spec="1.0+" codebase="https://www.example.com/demo" 
        href="demo_webstart.jnlp">
        <information>
            <title>Demo</title>
            <vendor>The Example.com Team</vendor>
        </information>
        <resources>
            <!-- Application Resources -->
            <j2se version="1.7+" href="http://java.sun.com/products/autodl/j2se"/>
            <jar href="Demo.jar" main="true"/>
        </resources>
        <application-desc
             name="Demo Application"
             main-class="com.example.jwsdemo.Main"
             width="300"
             height="300">
         </application-desc>
         <update check="background"/>
    </jnlp>    

As you can see, a JNLP file XML-based, and the information is all contained in the `<jnlp>` element.

 - The `spec` attribute gives the version of the JNPL spec that this file conforms to.
 - The `codebase` attribute gives the base URL for resolving relative `href` URLs in the rest of the file.
 - The `href` attribute gives the definitive URL for this JNLP file.
 - The `<information>` element contains metadata the application including its title, authors, description and help website.
 - The `<resources>` element describes the dependencies for the application including the required Java version, OS platform and JAR files.
 - The `<application-desc>` (or `<applet-desc>`) element provides information needed to launch the application.

Setting up the web server
-------------------------

The webserver must be configured to use `application/x-java-jnlp-file` as the MIMEtype for `.jnlp` files.

The JNLP file and the application's JAR files must be installed on the webserver so that they are available using the URLs indicated by the JNLP file.

Enabling launch via a web page
------------------------------

If the application is to be launched via a web link, the page that contains the link must be created on the webserver.

 - If you can assume that Java Web Start is already installed on the user's machine, then the web page simply needs to contain a link for launching the application. For example.

       <a href="https://www.example.com/demo_webstart.jnlp">Launch the application</a>

 - Otherwise, the page should also include some scripting to detect the kind of browser the user is using and request to download and install the required version of Java.

**NOTE:** It is a bad idea to encourage users to encourage to install Java this way, or even to enable Java in their web browsers so that JNLP web page launch will work.

Launching Web Start applications from the command line
------------------------------------------------------

The instructions for launching an Web Start application from the command line are simple. Assuming that the user has a Java 5.0 JRE or JDK installed, the simply need to run this:

    $ javaws <url>

where `<url>` is the URL for the JNLP file on the remote server.


  [1]: https://docs.oracle.com/javase/tutorial/deployment/webstart/
  [2]: http://www.oracle.com/technetwork/java/javase/javawebstart/index.html

## Creating an UberJAR for an application and its dependencies


