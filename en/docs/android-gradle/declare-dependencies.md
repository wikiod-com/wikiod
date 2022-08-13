---
title: "Declare Dependencies"
slug: "declare-dependencies"
draft: false
images: []
weight: 9959
type: docs
toc: true
---

## Declare dependencies for flavors
Dependencies can be added for specific product flavors in a similar fashion as [build configurations][1].

    android {
        ...   
        productFlavors {
            flavor1 {
                //...
            }
            flavor2 {
                //...
            }
        }
    }
    
    dependencies {
        flavor1Compile 'com.android.support:appcompat-v7:24.1.1'
        flavor1Compile 'com.google.firebase:firebase-crash:9.4.0'
            
        flavor2Compile 'com.android.support:appcompat-v7:24.1.1'
    } 


  [1]: https://www.wikiod.com/android-gradle/declare-dependencies#Declare Dependencies for Configurations

## How to add dependencies
The example below describes how to declare three different types of direct dependencies in the app/ module's `build.gradle` file: 

       android {...}
        ...
        dependencies {
            // The 'compile' configuration tells Gradle to add the dependency to the
            // compilation classpath and include it in the final package.
        
            // Dependency on the "mylibrary" module from this project
            compile project(":mylibrary")
        
            // Remote binary dependency
            compile 'com.android.support:appcompat-v7:24.1.0'
        
            // Local binary dependency
            compile fileTree(dir: 'libs', include: ['*.jar'])
        }

## How to add a repository
To download dependencies, declare the repository so Gradle can find them. To do this, add a `repositories { ... }` to the app/ module's `build.gradle` in the top-level file.


    repositories {
      // Gradle's Java plugin allows the addition of these two repositories via method calls:
      jcenter()
      mavenCentral()
    
      maven { url "http://repository.of/dependency" }

      maven { 
          credentials {
              username 'xxx'
              password 'xxx'
          }

      url 'http://my.maven
      }
    }


  [1]: https://bintray.com/bintray/jcenter
  [2]: http://mvnrepository.com/

## Module dependencies
In a multi-project `gradle build`, you can have a dependency with another module in your build.

Example:

      dependencies {
            // Dependency on the "mylibrary" module from this project
            compile project(":mylibrary")
      }

The `compile project(':mylibrary')` line declares a local Android library module named "mylibrary" as a dependency, and requires the build system to compile and include the local module when building your app.


## Local binary dependencies
You can have a dependency with a single jar or multiple jar files.

With a single jar file you can add:

    dependencies {
        compile files('libs/local_dependency.jar')
    }

It's possible to add a directory of jars to compile.

    dependencies {
            compile fileTree(dir: 'libs', include: ['*.jar'])
    }


The compile `fileTree(dir: 'libs', include: ['*.jar']`) line tells the build system to include any JAR files inside the `app/libs/` directory in the compilation classpath and in the final package of your app. 

If you have modules that require local binary dependencies, copy the JAR files for these dependencies into `<moduleName>/libs` inside your project.

If you need to add an **[aar files][1]** you can read more details here.


  [1]: https://www.wikiod.com/android-gradle/how-to-include-aar-files-in-a-project-in-android

## Remote binary dependencies
You can add remote dependencies in Gradle usign this structure:

    compile 'group:name:version'

or this alternative syntax:

    compile group: 'xxx', name: 'xxxxx', version: 'xxxx'

For example:

    compile 'com.android.support:appcompat-v7:24.1.0'

The `compile 'com.android.support:appcompat-v7:24.1.0`' line declares a dependency on version 24.1.0 of the Android Support Library.


## Declare Dependencies for Configurations
Dependencies can be added for specific configuration like test/androidTest

    androidTestCompile 'com.android.support.test.espresso:espresso-core:2.2.1'
    testCompile 'junit:junit:3.8.1'

Alternatively create your own configuration

    configurations {
        myconfig
    }
And then download dependency for this config

    myconfig group: 'com.mycompany', name: 'my_artifact', version: '1.0.0'

## Declare dependencies for build types
Dependencies can be added for specific [Build types][1]:

    android {
        ...   
        buildTypes {
            release {
                //...
            }
    
            debug {
                //....
            }
        }
    }
    
    dependencies {
        debugCompile 'com.android.support:appcompat-v7:24.1.1'
        releaseCompile 'com.google.firebase:firebase-crash:9.4.0'            
    } 


  [1]: https://www.wikiod.com/android-gradle/configure-build-types

