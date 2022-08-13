---
title: "Dependencies"
slug: "dependencies"
draft: false
images: []
weight: 9940
type: docs
toc: true
---

## Add a Local JAR File Dependency
# Single JAR

Sometimes you have a local JAR file you need to add as a dependency to your Gradle build. Here's how you can do this:

    dependencies {
        compile files('path/local_dependency.jar')
    }

Where `path` is a directory path on your filesystem and `local_dependency.jar` is the name of your local JAR file. The `path` can be relative to the build file.

# Directory of JARs

It's also possible to add a directory of jars to compile. This can be done like so:

    dependencies {
            compile fileTree(dir: 'libs', include: '*.jar')
    }

Where `libs` would be the directory containing the jars and `*.jar` would be the filter of which files to include.

# Directory of JARs as repository

If you only want to lookup jars in a repository instead of directly adding them as a dependency with their path you can use a flatDir repository.

    repositories {
        flatDir {
            dirs 'libs'
        }
    }

Looks for jars in the *`libs`* directory and its child directories.

## Add a Dependency
Dependencies in Gradle follow the same format as [Maven][1]. Dependencies are structured as follows:

    group:name:version

Here's an example:

    'org.springframework:spring-core:4.3.1.RELEASE'

To add as a compile-time dependency, simply add this line in your `dependency` block in the Gradle build file:

    compile 'org.springframework:spring-core:4.3.1.RELEASE'

An alternative syntax for this names each component of the dependency explicitly, like so:

    compile group: 'org.springframework', name: 'spring-core', version: '4.3.1.RELEASE'

This adds a dependency at compile time.

You can also add dependencies only for tests. Here's an example:

    testCompile group: 'junit', name: 'junit', version: '4.+'


  [1]: https://www.wikiod.com/maven

## Adding repositories
You have to point Gradle to the location of your plugins so Gradle can find them. 
Do this by adding a `repositories { ... }` to your `build.gradle`.

Here's an example of adding three repositories, [JCenter][1], [Maven Repository][2], and a custom repository that offers dependencies in Maven style.

    repositories {
      // Adding these two repositories via method calls is made possible by Gradle's Java plugin
      jcenter()
      mavenCentral()
    
      maven { url "http://repository.of/dependency" }
    }


  [1]: https://bintray.com/bintray/jcenter
  [2]: http://mvnrepository.com/

## List Dependencies
Calling the `dependencies` task allows you to see the dependencies of the root project:


    gradle dependencies

The results are dependency graphs (taking into account transitive dependencies), broken down by configuration. To restrict the displayed configurations, you can pass the `--configuration` option followed by one chosen configuration to analyse:

    gradle dependencies --configuration compile

To display dependencies of a subproject, use `<subproject>:dependencies` task. For example to list dependencies of a subproject named `api`:


    gradle api:dependencies



## Depend on Another Gradle Project
In the case of a multi-project gradle build, you may sometimes need to depend on another project in your build. To accomplish this, you'd enter the following in your project's dependencies:

    dependencies {
        compile project(':OtherProject')
    }

Where `':OtherProject'` is the gradle path for the project, referenced from the root of the directory structure.

To make `':OtherProject'` available in the context of the `build.gradle` file add this to the corresponding `settings.gradle`

    include ':Dependency'
    project(':Dependency').projectDir = new File('/path/to/dependency')

For a more detailed explanation, you can reference Gradle's official documentation [here][1].


  [1]: https://docs.gradle.org/current/userguide/multi_project_builds.html#sec:project_jar_dependencies

## Add .aar file to Android project using gradle
1. Navigate to project's `app` module and create `libs` directory.
2. Place your `.aar` file there. For example `myLib.aar`.
3. Add the code below to `android` block of `app` level's `build.gradle` file.

```
  repositories {
        flatDir {
            dirs 'libs'
        }
    }
```

This way you defined a new extra repository that points to `app` module's `libs` folder.

4. Add the code below to `dependencies` block or the `build.gradle` file:

```
compile(name:'myLib', ext:'aar')
```





