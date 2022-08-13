---
title: "How to include aar files in a project in Android"
slug: "how-to-include-aar-files-in-a-project-in-android"
draft: false
images: []
weight: 9976
type: docs
toc: true
---

## How to add .aar dependency in a module?
In a module (library or application) where you need the aar file you have to add in your `build.gradle` the repository:

    repositories {
        flatDir {
            dirs 'libs'
        }
    }

and add the dependency:

    dependencies {
        compile(name:'nameOfYourAARFileWithoutExtension', ext:'aar')
    }

Pay attention to the relative path of the libs folder that you are using in the module.

## The aar file doesn't include the transitive dependencies
The **aar** file **doesn't contain the [transitive][1] dependencies** and doesn't have a pom file which describes the dependencies used by the library.

It means that, if you are importing a aar file using a `flatDir` repo **you have to specify the dependencies also in your project**.

You should use a **maven repository** (you have to publish the library in a private or public maven repo), you will not have the same issue.  
In this case, gradle downloads the dependencies using the pom file which will contains the dependencies list.

  [1]: https://maven.apache.org/guides/introduction/introduction-to-dependency-mechanism.html#Transitive_Dependencies


This works with aar libraries that are published to a remote or local maven repository, In your case it sounds like the library will not be published to even a local maven repository. I can't find any definitive information as to if it will work in your circumstances, but you should give it a shot.

