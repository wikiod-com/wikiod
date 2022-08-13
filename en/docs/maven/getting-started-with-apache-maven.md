---
title: "Getting started with Apache Maven"
slug: "getting-started-with-apache-maven"
draft: false
images: []
weight: 1
type: docs
toc: true
---

## Installation or Setup
Binary releases of Maven can be downloaded [from the Maven website](https://maven.apache.org/download.cgi).

The binary comes as a zip archive or as a tar.gz archive. After downloading it, the instructions from [the install page](https://maven.apache.org/install.html) can be followed:

 - Ensure the `JAVA_HOME` environment variable is set and points to your JDK installation (not JRE). For example, on a Windows machine, this installation folder can correspond `C:\Program Files\Java\jdk1.8.0_51`.
 - Extract the distribution archive in the directory of your choice.
 - Add the `bin` directory of the created directory (named `apache-maven-3.3.9` for Maven 3.3.9) to the `PATH` environment variable. (Reference to [change it on Windows](http://superuser.com/questions/284342/what-are-path-and-other-environment-variables-and-how-can-i-set-or-use-them)).
 - Verify that the set-up is correct by running `mvn -version` on the command line.

There is no need to set the `M2_HOME` or `MAVEN_HOME` environment variable.

## Configuring Proxy Settings
If your Internet connection is provided via a proxy Maven will not be able to download jars from remote repositories - a common problem faced by companies. 

To solve this, Maven needs to be provided the details and credentials of the proxy by going to *{Maven install location} → conf →* `settings.xml`. Scroll down to the `<proxies>` tag and enter the details here, using the format mentioned in the comments.

***For Eclipse users***

Eclipse uses it's own `settings.xml` file for running Maven, whose location can be found by going to the menu *Window → Preferences → Maven → User Settings → User Settings:*. If the file is not available in the location mentioned, simply create it yourself or create a duplicate of the file from the above location *{Maven install location} → conf →* `settings.xml`.  

***For IntelliJ users***

Open the settings and navigate to Maven -> Importing. (This may be nested under Build, Execution, Deployment -> Build Tools ->, depending on the IntelliJ version you're using.)

Set the field named "VM options for importer" like:

    -DproxySet=true -DproxyHost=<HOST> -DproxyPort=<PORT>    
    -DproxySet=true -DproxyHost=myproxy.com -DproxyPort=8080

Apply and restart IntelliJ.

## Installation on Mac OSX with Brew
1. In a terminal run `brew install maven`
2. Once the install is over check that maven works correctly with `mvn -v`. The output should look something like: 

<pre><code>Apache Maven 3.3.9 
Maven home: /usr/local/Cellar/maven/3.3.9/libexec
Java version: 1.8.0_121, vendor: Oracle Corporation
Java home: /Library/Java/JavaVirtualMachines/jdk1.8.0_121.jdk/Contents/Home/jre
Default locale: en_US, platform encoding: UTF-8
OS name: "mac os x", version: "10.12.4", arch: "x86_64", family: "mac"
</code></pre>

If this does not work, make sure you have a JDK installed in your environment `javac -version`

## Installation on Ubuntu
 1. In a terminal run `sudo apt-get install maven`
 2. Once the install is over check that it works correctly with `mvn -v` the output should look like:

        Apache Maven 3.3.9
        Maven home: /usr/share/maven
        Java version: 1.8.0_121, vendor: Oracle Corporation
        Java home: /usr/lib/jvm/java-8-openjdk-amd64/jre
        Default locale: en_US, platform encoding: UTF-8
        OS name: "linux", version: "4.8.0-parrot-amd64", arch: "amd64", family: "unix"

If this does not work, make sure you have a JDK installed in your environment`javac -version`



