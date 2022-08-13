---
title: "Gradle Wrapper"
slug: "gradle-wrapper"
draft: false
images: []
weight: 9950
type: docs
toc: true
---

## Gradle Wrapper introduction
Gradle has the ability to add a wrapper to projects. This wrapper alleviates the need for all users or continuous integration systems to have Gradle installed. It also prevents version issues where there is some incompatibility between the version the project uses and that which users have installed.  It does this by installing a version of gradle locally in the project.

Users of the project simply run:

    > ./gradlew <task> # on *Nix or MacOSX
    > gradlew <task>   # on Windows

To setup a project to use a wrapper, developers:

1. Execute:


    gradle wrapper [--gradle-version 2.0]

Where `--gradle-version X` is optional and if not provided (or the wrapper task isn't included, as shown below), the version used is the version of gradle being used.

1. To force the project to use a specific version, add the following to the `build.gradle`:


    task wrapper(type: Wrapper) {
        gradleVersion = '2.0'
    }


When the `gradle wrapper` command is run it creates the files:

    the_project/
      gradlew
      gradlew.bat
      gradle/wrapper/
        gradle-wrapper.jar
        gradle-wrapper.properties

The official documentation on this feature is at https://docs.gradle.org/current/userguide/gradle_wrapper.html.

## Use locally served Gradle in the Gradle Wrapper
If you want to keep on-premises copy of the Gradle and let the Wrapper use it in the builds, you can set the `distributionUrl` pointing to your copy on the `wrapper` task:

    task wrapper(type: Wrapper) {
        gradleVersion = '2.0'
        distributionUrl = "http\://server/dadada/gradle-${gradleVersion}-bin.zip"
    }
after executing `gradle wrapper`, the shell script `gradlew` is created and the `gradle/wrapper/gradle-wrapper.properties` is configured to use provided URL to download the Gradle.

## Gradle Wrapper and Git
As discussed in the introduction, the gradle wrapper functionality works because a jar is downloaded into the project to be used when the `gradlew` command is run.  However this may not get committed and after the next time the project is checked out, `gradlew` will fail to run with the error:

    Error: Could not find or load main class org.gradle.wrapper.GradleWrapperMain

This will be because your .gitignore will likely include `*jar` for Java projects.  When the gradle wrapper was initialised, it copies to the file `gradle/wrapper/gradle-wrapper.jar`.  Thus you need to add it to the git index and commit it.  Do so with:

    git add -f gradle/wrapper/gradle-wrapper.jar
    git ci

With the `-f` being to force it.

## Using the Gradle Wrapper behind a proxy
The first time a user runs a project's `gradlew`, it should be realized that it will do two key things:

 1. Check to see if the version of the gradle used by the wrapper is already in ~/.gradle/wrapper/dists
 2. If not, download the archive of the version from the internet

If you're in an environment that requires all external traffic to go through a proxy, step two is going to fail (unless it's a transparent proxy environment). As a result, you need to ensure your have the *JVM* proxy parameters set.

For example, if you have a basic proxy setup with no authentication, simply set the environment variable `JAVA_OPTS` or `GRADLE_OPTS` with:

    -Dhttps.proxyPort=<proxy_port> -Dhttps.proxyHost=<hostname>

So a completed example on windows would be:

    set JAVA_OPTS=-Dhttps.proxyPort=8080 -Dhttps.proxyHost=myproxy.mycompany.com

If however your environment also requires authentication, then you'll also want to review your other options at https://docs.oracle.com/javase/8/docs/api/java/net/doc-files/net-properties.html.

*NOTE: This proxy configuration is in **addition** to any proxy configuration for your dependency repository access.*

