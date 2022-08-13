---
title: "Adding modules to wildfly"
slug: "adding-modules-to-wildfly"
draft: false
images: []
weight: 9992
type: docs
toc: true
---

## Adding module com.stackoverflow
 - Pack your com.stackoverflow.${whatever} clases on a jar called stackoverflow.jar
 - Create folder `com/stackoverflow/main` on `${WILDFLY_HOME}/modules`
 - Put the jar on the last dir
 - Create a file called module.xml with the following content:


    <?xml version="1.0" encoding="UTF-8"?>
    <module xmlns="urn:jboss:module:1.1" name="com.stackoverflow">
        <resources>
            <resource-root path="stackoverflow.jar"/>
        </resources>
    </module>

And now you have you module available to applications

## Adding module com.stackoverflow with versioning
For some reason you develop a new version of com.stackoverflow (say version 1.1), then you should:

 - Create folder `com/stackoverflow/1.1` on `${WILDFLY_HOME}/modules`
 - Put the new jar on last dir
 - Create file module.xml on last dir with the following content


    <?xml version="1.0" encoding="UTF-8"?>
    <module xmlns="urn:jboss:module:1.1" name="com.stackoverflow" slot="1.1">
        <resources>
            <resource-root path="stackoverflow.jar"/>
        </resources>
    </module>

Note the `slot="1.1"` modification on this example


## Multiple modules directories
By default modules are placed on `${WILDFLY}/modules` directory but you can have more directories with modules, just edit your standalone.conf (or standalone.conf.bat if you are on Microsoft Windows) and properly set the variable `JBOSS_MODULEPATH` 

For example in Unix/Linux/MacOSX:

    JBOSS_MODULEPATH="$JBOSS_HOME/modules:$JBOSS_HOME/myownmodules1:$JBOSS_HOME/myownmodules2"

Or in Windows:

    set  "JBOSS_MODULEPATH=%JBOSS_HOME%\modules;%JBOSS_HOME%\myownmodules1;%JBOSS_HOME%\myownmodules2"

