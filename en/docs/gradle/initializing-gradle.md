---
title: "Initializing Gradle"
slug: "initializing-gradle"
draft: false
images: []
weight: 9983
type: docs
toc: true
---

### Terminology
* [Task][1] - an atomic piece of work which a build performs. Tasks have `inputs`, `outputs` and task dependencies.
* [`dependencies {}`][2] - Declares `File` or binary dependencies necessary to execute tasks. For example, `org.slf4j:slf4j-api:1.7.21` is shorthand [coordinates][3] to a Maven dependency.
* [`repositories {}`][4] - How Gradle finds files for external dependencies. Really, just a collection of files organized by group, name, and version. For example: `jcenter()` is a convenience method for `maven { url 'http://jcenter.bintray.com/' } }`, a [Bintray Maven repository][5].


  [1]: https://docs.gradle.org/current/userguide/more_about_tasks.html
  [2]: https://docs.gradle.org/current/userguide/artifact_dependencies_tutorial.html
  [3]: https://maven.apache.org/pom.html#Maven_Coordinates
  [4]: https://docs.gradle.org/current/userguide/artifact_dependencies_tutorial.html#N10660
  [5]: https://bintray.com/bintray/jcenter

## Initializing a New Java Library
**Prerequisite: [Installing Gradle][1]**

Once you have Gradle installed, you can setup a new or existing project by running

    cd $PROJECT_DIR
    gradle init --type=java-library

*Note that there are [other project types][2] like Scala you can get started with, but we'll use Java for this example.*

You will end up with:

    .
    ├── build.gradle
    ├── gradle
    │   └── wrapper
    │       ├── gradle-wrapper.jar
    │       └── gradle-wrapper.properties
    ├── gradlew
    ├── gradlew.bat
    ├── settings.gradle
    └── src
        ├── main
        │   └── java
        │       └── Library.java
        └── test
            └── java
                └── LibraryTest.java

You can now run `gradle tasks` and see that you can build a `jar`, run `test`s, produce `javadoc`s and much more even though your `build.gradle` file is:

    apply plugin: 'java'
    
    repositories {
        jcenter()
    }
    
    dependencies {
        compile 'org.slf4j:slf4j-api:1.7.21'
        testCompile 'junit:junit:4.12'
    }


  [1]: https://www.wikiod.com/gradle/getting-started-with-gradle
  [2]: https://docs.gradle.org/current/userguide/build_init_plugin.html
  [3]: https://docs.gradle.org/current/userguide/tutorial_using_tasks.html
  [4]: https://docs.gradle.org/current/dsl/org.gradle.api.Project.html#org.gradle.api.Project:dependencies(groovy.lang.Closure)
  [5]: https://docs.gradle.org/current/dsl/org.gradle.api.Project.html#org.gradle.api.Project:repositories(groovy.lang.Closure)

