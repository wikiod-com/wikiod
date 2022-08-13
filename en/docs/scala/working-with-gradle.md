---
title: "Working With Gradle"
slug: "working-with-gradle"
draft: false
images: []
weight: 9978
type: docs
toc: true
---

## Create your own Gradle Scala plugin
After going through the **Basic Setup** example, you may find yourself repeating most part of it in every single Scala Gradle project. Smells like boilerplate code...

What if, instead of applying the [Scala plugin] offered by Gradle, you could apply your own Scala plugin, which would be responsible for handling all your common build logic, extending, at the same time, the already existing plugin. 

This example is going to transform the previous build logic into a reusable Gradle plugin.  

Luckyly, in Gradle, you can easily write custom plugins with the help of the Gradle API, as outlined in the [documentation]. As language of implementation, you can use Scala itself or even Java. However, most of the examples you can find throughout the docs are written in Groovy. If you need more code samples or you want to understand what lies behind the Scala plugin, for instance, you can check the gradle [github repo]. 

# **Writing the plugin**
#### **Requirements**
The custom plugin will add the following functionality when applied to a project:
- a `scalaVersion` property object, which will have two overridable default properties
    - major =  "2.12"
    - minor = "0"
- a `withScalaVersion` function, which applied to a dependency name, will add the scala major version to ensure binary compatibility
(sbt `%%` operator might ring a bell, otherwise go [here] before proceeding)
- a `createDirs` task to create the necessary directory tree, exactly as in the previous example
  
#### **Implementation guideline**
1. create a new gradle project and add the following to `build.gradle`
```
apply plugin: 'scala'
apply plugin: 'maven'

repositories {
    mavenLocal()
    mavenCentral()
}

dependencies {
    compile gradleApi()
    compile "org.scala-lang:scala-library:2.12.0"
}
```
__Notes__:
- the plugin implementation is written in Scala, thus we need the Gradle Scala Plugin
- in order to use the plugin from other projects, the Gradle Maven Plugin is used; this adds the `install` task used for saving the project jar to the Maven Local Repository  
- `compile gradleApi()` adds the `gradle-api-<gradle_version>.jar` to the classpath

2. create a new Scala class for the plugin implementation
```scala
package com.btesila.gradle.plugins 
 
import org.gradle.api.{Plugin, Project}  
 
class ScalaCustomPlugin extends Plugin[Project] {
    override def apply(project: Project): Unit = {
        project.getPlugins.apply("scala")
    }
}
```
__Notes__:
- in order to implement a Plugin, just extend `Plugin` trait of type `Project` and override the `apply` method
- within the apply method, you have access to the `Project` instance that the plugin is applied to and you can use it for adding build logic to it
- this plugin does nothing but apply the already existing Gradle Scala Plugin

3. add the `scalaVersion` object property  

Firstly, we create a `ScalaVersion` class, which will hold the two version properties
```
class ScalaVersion {
  var major: String = "2.12"
  var minor: String = "0"
}

```  
One cool thing about Gradle plugins is the fact that you can always add or override specific properties. A plugin receives this kind of user input via the `ExtensionContainer` attached to a gradle `Project` instance. For more details, check [this] out.  
By adding the following to the `apply` method, we are basically doing this:
- if there is not a `scalaVersion` property defined in the project, we add one with the default values
- otherwise, we get the existing one as instance of `ScalaVersion`, to use it further
 
 ```
 var scalaVersion = new ScalaVersion
 if (!project.getExtensions.getExtraProperties.has("scalaVersion"))
    project.getExtensions.getExtraProperties.set("scalaVersion", scalaVersion)
 else
    scalaVersion = project.getExtensions.getExtraProperties.get("scalaVersion").asInstanceOf[ScalaVersion]
```
This is equivalent to writing the following to the build file of the project that applies the plugin:
```
ext {
    scalaVersion.major = "2.12"
    scalaVersion.minor = "0"

}

```

4. add the `scala-lang` library to the project dependencies, using the `scalaVersion`  

```
project.getDependencies.add("compile", s"org.scala-lang:scala-library:${scalaVersion.major}.${scalaVersion.minor}")

```
This is equivalent to writing the following to the build file of the project that applies the plugin: 
```
compile "org.scala-lang:scala-library:2.12.0"
```

5. add the `withScalaVersion` function  
```
val withScalaVersion = (lib: String) => {
    val libComp = lib.split(":")
    libComp.update(1, s"${libComp(1)}_${scalaVersion.major}")
    libComp.mkString(":")
}
project.getExtensions.getExtraProperties.set("withScalaVersion", withScalaVersion)
```
6. finally, create the `createDirs` task and add it to the project  
Implement a Gradle task by extending `DefaultTask`:
```
class CreateDirs extends DefaultTask {
  @TaskAction
  def createDirs(): Unit = {
    val sourceSetContainer = this.getProject.getConvention.getPlugin(classOf[JavaPluginConvention]).getSourceSets

    sourceSetContainer forEach { sourceSet =>
      sourceSet.getAllSource.getSrcDirs.forEach(file => if (!file.getName.contains("java")) file.mkdirs())
    }
  }
}
```
__Note__: the `SourceSetContainer` has information about all source directories present in the project. What the Gradle Scala Plugin does, is to add the extra source sets to the Java ones, as you can see in the[plugin docs].

Add the `createDir` task to the project by appending this to the `apply` method:
```
project.getTasks.create("createDirs", classOf[CreateDirs])

```

In the end, your `ScalaCustomPlugin` class should look like this:
```
class ScalaCustomPlugin extends Plugin[Project] {
  override def apply(project: Project): Unit = {
    project.getPlugins.apply("scala")

    var scalaVersion = new ScalaVersion
    if (!project.getExtensions.getExtraProperties.has("scalaVersion"))
      project.getExtensions.getExtraProperties.set("scalaVersion", scalaVersion)
    else
      scalaVersion = project.getExtensions.getExtraProperties.get("scalaVersion").asInstanceOf[ScalaVersion]

    project.getDependencies.add("compile", s"org.scala-lang:scala-library:${scalaVersion.major}.${scalaVersion.minor}")

    val withScalaVersion = (lib: String) => {
      val libComp = lib.split(":")
      libComp.update(1, s"${libComp(1)}_${scalaVersion.major}")
      libComp.mkString(":")
    }
    project.getExtensions.getExtraProperties.set("withScalaVersion", withScalaVersion)

    project.getTasks.create("createDirs", classOf[CreateDirs])
  }
}

```
#### **Installing the plugin project to the local Maven repository**
This is done really easy by running `gradle install`  
You can check the installation by going to local repository directory, usually found at `~/.m2/repository` 

#### **How does Gradle find our new plugin?**
Each Gradle plugin has an `id` which is used in the `apply` statement. For instance, by writing the following to the build file,
it translates to a trigger to Gradle to find and apply the plugin with id `scala`.
```
apply plugin: 'scala'
```


In the same way, we would like to apply our new plugin in the following way,
```
apply plugin: "com.btesila.scala.plugin"

```
meaning that our plugin will have the `com.btesila.scala.plugin` id.  

In order to set this id, add the following file:   
**src/main/resources/META-INF/gradle-plugin/com.btesil.scala.plugin.properties**
```
implementation-class=com.btesila.gradle.plugins.ScalaCustomPlugin

```
Afterwards, run again `gradle install`.

## **Using the plugin**
1. create a new empty Gradle project and add the following to the build file
```
buildscript {
    repositories {
        mavenLocal()
        mavenCentral()
    }

    dependencies {
        //modify this path to match the installed plugin project in your local repository
        classpath 'com.btesila:working-with-gradle:1.0-SNAPSHOT'
    }
}

repositories {
    mavenLocal()
    mavenCentral()
}


apply plugin: "com.btesila.scala.plugin"

```
2. run `gradle createDirs` - you should now have all the source directories generated
3. override the scala version by adding this to the build file:
```
ext {
    scalaVersion.major = "2.11"
    scalaVersion.minor = "8"

}
println(project.ext.scalaVersion.major)
println(project.ext.scalaVersion.minor)
```
4. add a dependency library that is binary compatible with the Scala version
```
dependencies {
    compile withScalaVersion("com.typesafe.scala-logging:scala-logging:3.5.0")
}

```

That's it! You can now use this plugin across all your projects without repeating the same old boilerplate.

[Scala Plugin]: https://docs.gradle.org/current/userguide/scala_plugin.html
[documentation]: https://docs.gradle.org/current/userguide/custom_plugins.html
[here]: http://www.scala-sbt.org/0.13/docs/Library-Dependencies.html#Getting+the+right+Scala+version+with
[this]: https://docs.gradle.org/current/userguide/custom_plugins.html#sec:getting_input_from_the_build
[github repo]: https://github.com/gradle/gradle
[plugin docs]: https://docs.gradle.org/current/userguide/scala_plugin.html#sec:scala_source_set_properties


## Basic Setup

1. Create a file named `SCALA_PROJECT/build.gradle` with these contents:

    ```
    group 'scala_gradle'
    version '1.0-SNAPSHOT'
    
    apply plugin: 'scala'
    
    repositories {
        jcenter()
        mavenCentral()
        maven {
            url "https://repo.typesafe.com/typesafe/maven-releases"
        }
    }
    
    dependencies {
        compile group: 'org.scala-lang', name: 'scala-library', version: '2.10.6'
    }
    
    
    task  "create-dirs" << {
        sourceSets*.scala.srcDirs*.each { it.mkdirs() }
        sourceSets*.resources.srcDirs*.each { it.mkdirs() }
    }
    
    ```

2. Run `gradle tasks` to see available tasks.
3. Run `gradle create-dirs` to create a `src/scala, src/resources` directory.
4. Run `gradle build` to build the project and download dependencies.


