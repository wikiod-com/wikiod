---
title: "Gradle - Information of Tags"
slug: "gradle---information-of-tags"
draft: false
images: []
weight: 9990
type: docs
toc: true
---

## Gradle - Information of Tags
Gradle:
It is used to make build for any software, it is a Domain specific language used to configure and fulfill all plugins, libraries downloaded from repositories.

Use Plugins:

    Apply plugin: ‘com.android.application’

Plugin is property in key value form. In above statement plugin denotes to key and right side string in single coats becomes its value.

Gradle is DSL (Domain specific language):

It contains different `blocks:Tags`

    repositories { } 
    dependencies {}
    android {} 

Repositories and dependencies are used to configure requirements for application code.
Android block is used to add android specific code or information into application.
We also generate our custom tags and define our own custom code, library and information. 

By using `“task” tag :`

    task genrateTestDb (depends on: ….) {
     }

Gradle files for any application

`Build.gradle` -These file is working for all project.
`Settings.gradle` – define all sub directories or projects are included in application.

`Build.gradle` contains below:

    repositories { 
    mavenCentral()
    } 

Above repositories tag hold `mevenCentral()` it means all dependencies are downloaded from `mevenCentral()` .we can use `jcenter()` or any other source too.
Dependencies block holds all **compile time dependencies** that’s should be downloaded from `repositories`. 

    dependencies {
    compile ‘org.codehous.groovy:groovy-all:2.3.2’
    }
Above is `meven` library : 
syntax: 

`org.codehous.groovy` - > group id

`groovy-all` - > order fact id , that’s is a name gradle used to identify library .

`2.3.2’` - > version 

`Settings.gradle` – it’s include tag for all sub projects that’s is added into project.

    Include ‘googlechart’, ‘chuckgroovy’



