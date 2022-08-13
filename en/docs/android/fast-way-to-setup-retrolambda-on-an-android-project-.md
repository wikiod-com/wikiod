---
title: "Fast way to setup Retrolambda on an android project."
slug: "fast-way-to-setup-retrolambda-on-an-android-project"
draft: false
images: []
weight: 9987
type: docs
toc: true
---

Retrolambda is a library which allows to use Java 8 lambda expressions, method references and try-with-resources statements on Java 7, 6 or 5.

The Gradle Retrolambda Plug-in allows to integrate Retrolambda into a Gradle based build. This allows for example to use these constructs in an Android application, as standard Android development currently does not yet support Java 8.



## Setup and example how to use:
**Setup Steps:**
1. Download and install jdk8.
2. Add the following to your project’s main build.gradle

       buildscript {
           repositories {
               mavenCentral()
           }
    
           dependencies {
               classpath 'me.tatarka:gradle-retrolambda:3.2.3'
           }
       }

3. Now add this to your application module’s build.gradle

       apply plugin: 'com.android.application' // or apply plugin: 'java'
       apply plugin: 'me.tatarka.retrolambda'

4. Add these lines to your application module’s build.gradle to inform the IDE of the language level:

       android {
           compileOptions {
               sourceCompatibility JavaVersion.VERSION_1_8
               targetCompatibility JavaVersion.VERSION_1_8
           }
       }


**Example:**

So things like this:

    button.setOnClickListener(new View.OnClickListener() {
        @Override
        public void onClick(View v) {
            log("Clicked");
        }
    });

Become this:

    button.setOnClickListener(v -> log("Clicked"));


