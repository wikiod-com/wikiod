---
title: "Gradle Init Scripts"
slug: "gradle-init-scripts"
draft: false
images: []
weight: 9977
type: docs
toc: true
---

## Add default repository for all projects
Add a init.gradle to your user gradle folder. The init.gradle is recognized on every project.

    Unix: ~/.gradle/init.gradle

> These are also alternative locations where init script can be placed and
> loaded automatically:-
> 
>  - Any ***.gradle** file in **USER_HOME/.gradle/init.d** 
>  - Any ***.gradle** file in    the Gradle installation’s **init.d** directory

init.gradle with mavenLocal as repository in all projects.

    allprojects {
        repositories {
            mavenLocal()
        }
    }

With that you have your local maven cache available in all repositories.
A use case could be to use a jar that you put in ther with "gradle install" in another project without adding the mavenLocal repository to the build.gradle or adding a nexus/artifactory server.

