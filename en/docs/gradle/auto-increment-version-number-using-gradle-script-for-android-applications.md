---
title: "Auto Increment Version Number Using Gradle Script For Android Applications"
slug: "auto-increment-version-number-using-gradle-script-for-android-applications"
draft: false
images: []
weight: 9999
type: docs
toc: true
---

## How To Call Auto Increment Method When Build
    
    
    gradle.taskGraph.whenReady {taskGraph ->
        if (taskGraph.hasTask(assembleDebug)) {  /* when run debug task */
            autoIncrementBuildNumber()
        } else if (taskGraph.hasTask(assembleRelease)) { /* when run release task */
            autoIncrementBuildNumber()
        }
    }

## Auto Increment Method Definition
     /*Wrapping inside a method avoids auto incrementing on every gradle task run. Now it runs only when we build apk*/
    ext.autoIncrementBuildNumber = {

        if (versionPropsFile.canRead()) {
            def Properties versionProps = new Properties()
            versionProps.load(new FileInputStream(versionPropsFile))
            versionBuild = versionProps['VERSION_BUILD'].toInteger() + 1
            versionProps['VERSION_BUILD'] = versionBuild.toString()
            versionProps.store(versionPropsFile.newWriter(), null)
        } else {
            throw new GradleException("Could not read version.properties!")
        }
    }

## Read and Assign Version Number from a property file to a variable
def versionPropsFile = file('version.properties')
    def versionBuild

    /*Setting default value for versionBuild which is the last incremented value stored in the file */
    if (versionPropsFile.canRead()) {
        def Properties versionProps = new Properties()
        versionProps.load(new FileInputStream(versionPropsFile))
        versionBuild = versionProps['VERSION_BUILD'].toInteger()
    } else {
        throw new GradleException("Could not read version.properties!")
    }

