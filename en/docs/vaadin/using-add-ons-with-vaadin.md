---
title: "Using add-ons with Vaadin"
slug: "using-add-ons-with-vaadin"
draft: false
images: []
weight: 9991
type: docs
toc: true
---

## Using add-ons in a Maven project
To view an browse Vaadin add-ons in the Directory, you must be registered to vaadin.com. After the initial discovery of artifact details, e.g. for download and usage, registration is not required. Also, the usage of add-ons in a Maven project is not IDE-specific and the same instructions apply.

From a normal Maven project, start by editing your pom.xml :

 1. Add the Vaadin add-on repository

        <repositories>
          <repository>
            <id>vaadin-addons</id>
            <url>http://maven.vaadin.com/vaadin-addons</url>
          </repository>
          ...
       <repositories>

 2. Add the Vaadin Maven plugin in the maven build
  
        <plugin>
          <groupId>com.vaadin</groupId>
          <artifactId>vaadin-maven-plugin</artifactId>
          <version>7.6.8</version>
          <configuration>
            <extraJvmArgs>-Xmx512M -Xss1024k</extraJvmArgs>
            <webappDirectory>${basedir}/target/classes/VAADIN/widgetsets</webappDirectory>
            <draftCompile>false</draftCompile>
            <compileReport>false</compileReport>
            <style>OBF</style>
            <strict>true</strict>
          </configuration>
          <executions>
             <execution>
               <goals>
                 <goal>update-theme</goal>
                 <goal>update-widgetset</goal>
                 <goal>compile</goal>
               </goals>
             </execution>
           </executions>
         </plugin>

 3. Add the add-on as a normal dependency

        <dependency>
          <groupId>org.vaadin</groupId>
          <artifactId>viritin</artifactId>
          <version>1.54</version>
        </dependency>

 4. If the add-on has client-side code, you need to update the widgetset XML and compile the widgetset:

        mvn vaadin:update-widgetset vaadin:compile

Use the add-on in Java code as you would use any other Vaadin component. 

Note that if you used a Vaadin Maven archetype to generate the project, you only need to go through steps 3 and 4, as the generated pom.xml contains the necessary information.


## Add-ons In Eclipse
Download the .jar file from [vaadin add-ons](http://vaadin.com/add-ons) And put it in the lib folder of WEB-INF then right click on the .jar file and click on to Build Path --> Add To Build Path



