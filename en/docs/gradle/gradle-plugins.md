---
title: "Gradle Plugins"
slug: "gradle-plugins"
draft: false
images: []
weight: 9945
type: docs
toc: true
---

## How to write a standalone plugin
To create a custom standalone Gradle plug-in using java (you can also use Groovy) you have to create a structure like this:

    plugin
    |-- build.gradle
    |-- settings.gradle
    |-- src
        |-- main
        |   |-- java
        |   |-- resources
        |       |-- META-INF
        |           |-- gradle-plugins
        |-- test

# Setup gradle configuration

In the `build.gradle` file you define your project.

    apply plugin: 'java'
    apply plugin: 'maven'
    
    dependencies {
        compile gradleApi()
    } 


The `java` plugin will be used to write java code.  
The `gradleApi()` dependency will give us all method and propertiess needed to create a Gradle plugin.


In the `settings.gradle` file:

    rootProject.name = 'myplugin' 

It will define the **artifact id** in Maven.  
If `settings.gradle` file is not present in the plugin directory the default value will be the name of the directory.


# Create the Plugin

Define a class in the `src/main/java/org/sample/MyPlugin.java` implementing the `Plugin` interface.

    import org.gradle.api.Plugin;
    import org.gradle.api.Project;

    public class MyPlugin implements Plugin<Project> {
    
        @Override
        public void apply(Project project) {
             project.getTasks().create("myTask", MyTask.class);
        }
    
    } 

Define the task extending the `DefaultTask` class:

    import org.gradle.api.DefaultTask;
    import org.gradle.api.tasks.TaskAction;
    
    public class MyTask extends DefaultTask {
    
        @TaskAction
        public void myTask() {
            System.out.println("Hello World");
        }
    }

# Plugin Class declaration

In the `META-INF/gradle-plugins` folder you have to create a properties file defining the `implementation-class` property that identifies the Plugin implementation class.

In the `META-INF/gradle-plugins/testplugin.properties`

    implementation-class=org.sample.MyPlugin.java

Notice that the **properties filename matches the plugin id**.

# How to build and publish it

Change the `build.gradle` file adding some info to upload the plugin in a maven repo:

    apply plugin: 'java'
    apply plugin: 'maven'
    
    dependencies {
        compile gradleApi()
    }
    
    repositories {
        jcenter()
    }
    
    
    group = 'org.sample'
    version = '1.0'
    
    uploadArchives {
        repositories {
            mavenDeployer {
            repository(url: mavenLocal().url)
            }
        }
    } 

You can build and publish the Gradle plug-in to the Maven repo defined in the `plugin/build.gradle` file using the following command.

    $ ./gradlew clean uploadArchives 



# How to use it

To use the plugin add in the `build.gradle` of your project:

    buildscript {
         repositories {
             mavenLocal()
         }
     dependencies {
        classpath group: 'org.sample',    // Defined in the build.gradle of the plugin
                  name: 'myplugin',       // Defined by the rootProject.name 
                  version: '1.0'
        }
     }
    
    apply plugin: 'testplugin'            // Defined by the properties filename

Then you can call the task using:

     $ ./gradlew myTask



## Simple gradle plugin from `buildSrc`
Simple example of how to create a custom plugin and DSL for your gradle project.  
This sample uses one of the three possible ways of creating plugins.  
The three ways are:
- inline
- buildSrc
- standalone plugins

This example shows creating a plugin from the **buildSrc** folder.

This sample will create five files

    // project's build.gradle
    build.gradle
    // build.gradle to build the `buildSrc` module
    buildSrc/build.gradle
    // file name will be the plugin name used in the `apply plugin: $name`
    // where name would be `sample` in this example
    buildSrc/src/main/resources/META-INF/gradle-plugins/sample.properties
    // our DSL (Domain Specific Language) model
    buildSrc/src/main/groovy/so/docs/gradle/plugin/SampleModel.groovy
    // our actual plugin that will read the values from the DSL
    buildSrc/src/main/groovy/so/docs/gradle/plugin/SamplePlugin.groovy
    
build.gradle:

    group 'so.docs.gradle'
    version '1.0-SNAPSHOT'

    apply plugin: 'groovy'
    // apply our plugin... calls SamplePlugin#apply(Project)
    apply plugin: 'sample'

    repositories {
        mavenCentral()
    }

    dependencies {
        compile localGroovy()
    }

    // caller populates the extension model applied above
    sample {
        product = 'abc'
        customer = 'zyx'
    }

    // dummy task to limit console output for example
    task doNothing <<{}

buildSrc/build.gradle

    apply plugin: 'groovy'

    repositories {
        mavenCentral()
    }

    dependencies {
        compile localGroovy()
    }

buildSrc/src/main/groovy/so/docs/gradle/plugin/SamplePlugin.groovy:

    package so.docs.gradle.plugin

    import org.gradle.api.Plugin
    import org.gradle.api.Project

    class SamplePlugin implements Plugin<Project> {
        @Override
        void apply(Project target) {
            // create our extension on the project for our model
            target.extensions.create('sample', SampleModel)
            // once the script has been evaluated the values are available
            target.afterEvaluate {
                // here we can do whatever we need to with our values
                println "populated model: $target.extensions.sample"
            }
        }
    }

buildSrc/src/main/groovy/so/docs/gradle/plugin/SampleModel.groovy:

    package so.docs.gradle.plugin

    // define our DSL model
    class SampleModel {
        public String product;
        public String customer;

        @Override
        public String toString() {
            final StringBuilder sb = new StringBuilder("SampleModel{");
            sb.append("product='").append(product).append('\'');
            sb.append(", customer='").append(customer).append('\'');
            sb.append('}');
            return sb.toString();
        }
    }

buildSrc/src/main/resources/META-INF/gradle-plugins/sample.properties

    implementation-class=so.docs.gradle.plugin.SamplePlugin

Using this setup we can see the values supplied by the caller in your DSL block

     $ ./gradlew -q doNothing
    SampleModel{product='abc', customer='zyx'}

