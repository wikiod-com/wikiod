---
title: "Getting started with grails"
slug: "getting-started-with-grails"
draft: false
images: []
weight: 1
type: docs
toc: true
---

## Creating an application
To create a Grails application, use the `grails create-app` command. The following command creates a Grails application, named `myapp` in the current directory:

    grails create-app fancy-app

Running it, is as simple as visiting the, newly created, application directory:

    cd fancy-app

and then

    grails run-app 
    // in order to run the app on a different port, e.g. 8888, use this instead
    grails run-app -port 8888
    // in order to run the app with a secure communication
    grails run-app -https

## Grails Installation
Note: GRAILS requires a Java JDK installed (a runtime environment JRE is not sufficient) on your system, before setting up Grails. Please refer to, [how to install JDK][1]. As of this writing, it is recommended to install the latest JDK.

----
**For Mac OSX, Linux, Cygwin, Solaris and FreeBSD:**

The simplest way to manage Grails versions is using [sdkman](http://sdkman.io).
If `sdkman` is installed, you can then install any version of Grails using

```bash
sdk install grails [version]
```

This will take care of all steps to get this right. If you skip the `version`, the latest version of grails will be installed. For more about using `sdkman`, refer to [sdkman usage page][2].

----
**For Linux:**

```bash
    GRAILS_HOME=$HOME/bin/grails/current
    # abbreviating it using "..." for brevity
    PATH=$GRAILS_HOME/bin:$JAVA_HOME/bin: ... :$PATH
```

----
**For Windows:**

 1. Download a Java JDK from [Oracle][3] and install on your Windows machine. Take note of the installation folder.
 2. Download a version of Grails manually from the [Downloads](https://grails.org/download.html) page.
 3. Extract the Grails file, anywhere you like. 
 4. **Important:** You must set up 2 new environment variables `JAVA_HOME` and `GRAILS_HOME` (for Windows 10 found under *Control Panel \ System and Security \ System \ Advanced System settings \ Advanced tab \ Environment Variables) *, pointing to the extracted directories, e.g.
> Name: JAVA_HOME

> Value: C:\Programs\Java\jdk1.8.0_31

> Name: GRAILS_HOME

> Value: c:\grails\grails-3.2.4

 5. **Important:** You must extend the Windows `PATH` variable to include both JAVA_HOME and GRAILS_HOME. The path variable is also found in then control panel (see 4), e.g. add the following at the end:
> ;C:\Programs\Java\jdk1.8.0_31\bin;c:\grails\grails-3.2.4;c:\grails\grails-3.2.4\bin

 5. To verify your installation is correct, open a Command Prompt and type `GRAILS -VERSION`. You should get something like:
```
| Grails Version: 3.2.4
| Groovy Version: 2.4.6
| JVM Version: 1.8.0_65
```

  [1]: https://www.wikiod.com/java/getting-started-with-java-language#Setting %PATH% and %JAVA_HOME% after installing on Windows
  [2]: http://sdkman.io/usage.html
  [3]: http://www.oracle.com/technetwork/java/javase/downloads/jdk8-downloads-2133151.html

## Testing an Application
The create-* commands in Grails automatically create unit or integration tests for you within the src/test/groovy directory. It is of course up to you to populate these tests with valid test logic, information on which can be found in the section on Unit and integration tests.

To execute tests you run the test-app command as follows:

    grails test-app

## Creating a Model
A model (see: Model-View-Controller pattern) in Grails is represented by a so-called **Domain Class**. Domain classes can define both the persistence and presentation of information in grails. Domain classes can also contain validations.

To manage a fleet of cars in your Grails application you could define a domain class to describe, store and represent various cars in your fleet.

To create a stub for a domain class execute the following command inside your application folder:

    grails create-domain-class org.fleetmanager.Car

Next, open the generated car.groovy file and edit your domain class as follows:

    package org.fleetmanager

    class Car {
       String      manufacturer
       String      model
       String      color
       Integer     year
       Date        acquisitionDate
       Boolean     isElectric
    }

Finally, generate a controller for your car domain and a view using the following Grails command:

    grails generate-all org.fleetmanager.Car

Now, you can run your applications, select the car controller and manage your fleet.


