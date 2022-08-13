---
title: "Publish .aar file to Apache Archiva with Gradle"
slug: "publish-aar-file-to-apache-archiva-with-gradle"
draft: false
images: []
weight: 9990
type: docs
toc: true
---

## Simple implementation example
    apply plugin: 'com.android.library'
    apply plugin: 'maven'
    apply plugin: 'maven-publish'
    android {
    compileSdkVersion 21
    buildToolsVersion "21.1.2"
    
    
    repositories {
        mavenCentral()
    }
    
    defaultConfig {
        minSdkVersion 9
        targetSdkVersion 21
        versionCode 1
        versionName "1.0"
    }
    
    buildTypes {
        release {
            minifyEnabled false
            proguardFiles getDefaultProguardFile('proguard-android.txt'), 'proguard-rules.pro'
        }
    }
    
    
    dependencies {
       compile fileTree(include: ['*.jar'], dir: 'libs')
       provided 'com.android.support:support-v4:21.0.3'
       provided 'com.android.support:appcompat-v7:21.0.3'
    }
    
    task sourceJar(type: Jar) {
       classifier "source"
    }
    
    publishing {
       publications {
    
           repositories.maven {
               url 'myurl/repositories/myrepo'
               credentials {
                   username "user"
                   password "password"
               }
           }
    
           maven(MavenPublication) {
               artifacts {
                   groupId 'com.mycompany'
                   artifactId 'mylibrary'
                   version '1.0'
                   artifact 'build/outputs/aar/app-release.aar'
               }
           }
       }
    
    }

