---
title: "Using third party plugins"
slug: "using-third-party-plugins"
draft: false
images: []
weight: 9993
type: docs
toc: true
---

## Adding a third party plugin to build.gradle
**Gradle (All Versions)**
*This method works for all versions of gradle*

Add the buildscript code at the beginning of your build.gradle file. 

    buildscript {
      repositories {
        maven {
          url "https://plugins.gradle.org/m2/"
        }
      }
      dependencies {
        classpath "org.example.plugin:plugin:1.1.0"
      }
    }
    
    apply plugin: "org.example.plugin"

**Gradle (Versions 2.1+)** *This method only works for projects using Gradle 2.1 or later.*

    plugins {
      id "org.example.plugin" version "1.1.0"
    }

## build.gradle with multiple third party plugins
**Gradle (All Versions)**

When adding multiple third party plugins you do not need to separate them into different instances of the buildscript(All) or plugin(2.1+) code, new plug ins can be added alongside pre-existing plugins.

    buildscript {
      repositories {
        maven {
          url "https://plugins.gradle.org/m2/"
        }
      }
      dependencies {
        classpath "org.example.plugin:plugin:1.1.0"
        Classpath "com.example.plugin2:plugin2:1.5.2"
      }
    }
    
    apply plugin: "org.example.plugin"
    apply plugin: "com.example.plugin2"

**Gradle (Versions 2.1+)**

    plugins {
      id "org.example.plugin" version "1.1.0"
      id "com.example.plugin2" version "1.5.2"
    }

