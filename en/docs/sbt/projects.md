---
title: "Projects"
slug: "projects"
draft: false
images: []
weight: 9996
type: docs
toc: true
---

## Multiple Projects in the same Build (Subprojects)
Sometimes a build combines multiple source directories, each of which is their own 'project'. For instance, you might have a build structure like this:

  projectName/
    build.sbt
    project/
      <SBT sub-build information>
    src/
      main/
        ...
      test/
        ...
    core/
      src/
        main/
          ...
        test/
          ...
    webapp/
      src/
        main/
          ...
        test/
          ...

In the above project, the code in `projectName/src` is considered the `root` project. There are two other modules, or 'subprojects', `core` and `webapp`.

Configuring a subproject is similar to configuring the root project, except that the subdirectory is specified in the project. This example shows a root project that aggregates a `core` and `webapp` project. 

    lazy val root = (project in file(".")).aggregate(core,webapp).dependsOn(core, webapp)

    lazy val core = (project in file("core"))

    lazy val webapp = (project in file("webapp")).dependsOn(core)

The values passed to `file()` are the directories relative to the project root. 

The `webapp` project depends on the `core` project, which is indicated by the `dependsOn` clause, which takes the `core` value specified on the line above. `dependsOn` and `lazy` evaluation ensure that dependencies are available before projects utilize them. In this case, `webapp` depends on `core`, so `core` will be compiled before the build attempts to compile `webapp`.

`aggregate` makes tasks defined in one project available to the project that aggregates it. For instance, executing `compile` in the `root` project will also execute `compile` in `core` and `webapp`.

## Configure Macros in a Project
In the `build.sbt` file (or where the project is defined if it is in another location), add the following setting:

    scalacOptions += "-language:experimental.macros"

For instance, a project might be defined like this:

    lazy val main = project.in(file("."))  // root project
      .settings(scalacOptions += "-language:experimental.macros",
                addCompilerPlugin("org.scalamacros" % "paradise" % "2.1.0" cross CrossVersion.full))

In the above example, the [`paradise`][1] plugin is included in order to provide complete support of Scala `2.10.x`.
     


  [1]: http://docs.scala-lang.org/overviews/macros/paradise.html

## Display Settings
When in the SBT console, to list all definable settings for a project:

    settings

Or, to get a subproject's (for example, named `webapp`) settings:

    project webapp
    settings

The first line above navigates into the specific subproject.

To show the value of a specific setting (for instance, `organization`):

    show organization

This will display the value of that setting.


