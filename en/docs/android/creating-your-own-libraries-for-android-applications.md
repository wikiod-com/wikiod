---
title: "Creating your own libraries for Android applications"
slug: "creating-your-own-libraries-for-android-applications"
draft: false
images: []
weight: 9957
type: docs
toc: true
---

## Create a library available on Jitpack.io
Perform the following steps to create the library:

1. Create a GitHub account.
2. Create a Git repository containing your library project.
3. Modify your library project's `build.gradle` file by adding the following code:

       apply plugin: 'com.github.dcendents.android-maven'

       ...

       // Build a jar with source files.
       task sourcesJar(type: Jar) {
           from android.sourceSets.main.java.srcDirs
           classifier = 'sources'
       }
    
       task javadoc(type: Javadoc) {
           failOnError  false
           source = android.sourceSets.main.java.sourceFiles
           classpath += project.files(android.getBootClasspath().join(File.pathSeparator))
           classpath += configurations.compile
       }
    
       // Build a jar with javadoc.
       task javadocJar(type: Jar, dependsOn: javadoc) {
           classifier = 'javadoc'
           from javadoc.destinationDir
       }
    
       artifacts {
           archives sourcesJar
           archives javadocJar
       }

   Make sure that you commit/push the above changes to GitHub.

4. Create a release from the current code on Github.
5. Run `gradlew install` on your code.
6. Your library is now available by the following dependency:

       compile 'com.github.[YourUser]:[github repository name]:[release tag]'

## Creating library project
To create a libary , you should use `File -> New -> New Module -> Android Library`. This will create a basic library project.

When that's done, you must have a project that is set up the following manner:

    [project root directory]
        [library root directory]
        [gradle]
        build.gradle //project level
        gradle.properties
        gradlew
        gradlew.bat
        local.properties
        settings.gradle //this is important!

Your `settings.gradle` file must contain the following:

    include ':[library root directory]'

Your `[library root directory]` must contain the following:
  
    [libs]
    [src]
       [main]
          [java]
             [library package]
       [test]
          [java]
             [library package]
    build.gradle //"app"-level
    proguard-rules.pro

Your "app"-level `build.gradle` file must contain the following:

    apply plugin: 'com.android.library'
    
    android {
        compileSdkVersion 23
        buildToolsVersion "23.0.2"
    
        defaultConfig {
            minSdkVersion 14
            targetSdkVersion 23
        }
    }

With that, your project should be working fine!

## Using library in project as a module
To use the library, you must include it as a dependency with the following line:

    compile project(':[library root directory]')



