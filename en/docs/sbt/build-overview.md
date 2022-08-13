---
title: "Build Overview"
slug: "build-overview"
draft: false
images: []
weight: 9995
type: docs
toc: true
---

Official documentation is at [www.scala-sbt.org][1].


  [1]: http://www.scala-sbt.org/documentation.html

## Directory Structure
The standard structure for a project built by SBT is:

    projectName/
        build.sbt
        project/
          <SBT sub-build information>
        src/
          main/
            scala/
               <Scala source files>
            java/
               <Java source files>
            resources/
               <Resource files>
          test/
            scala/
               <Scala test files>
            java/
               <Java test files>
            resources/
               <Resource files>

Other directories may exist, but the build deals primarily with these. In the base directory `build.sbt` is placed, whose contents at a minimum are:

- `name := <name of build>`: This is the name of the project.
- `version := <version number>`: This is the version of the project for downstream code to reference.
- `scalaVersion := <version of Scala>`: This is the version of Scala that the project's bytecode is built against.

The `project` directory is where the `meta-build` (as opposed to the `proper-build`) files are placed. This directory can have it's own `build.sbt` file that executes in exactly the same manner, creating an environment for the `proper-build` SBT build to execute. This is recursive, so the `project` directory can have it's own `project` directory where a `meta-meta-build` occurs, and so on.

Upon building, SBT will create a `target` directory in which class files and other components are placed.

## Cheat Sheet
This sheet assumes that you are in the root directory of the project, containing the `build.sbt`. `$` indicates a command prompt and `>` indicates commands run inside the SBT console.

# Compile a project

    $ sbt compile

# Test a project

    $ sbt test

# Enter SBT REPL:

    $ sbt

# Enter Scala Console with Built Project Available

    $ sbt
    > console

# Generate Scaladoc

This is an example of executing an [SBT 'Task'][1]. The SBT site has more information on [generating Scaladoc documentation][2].

    $ sbt doc

or:

    $ sbt
    > doc


  [1]: http://www.scala-sbt.org/0.13/docs/Tasks.html
  [2]: http://www.scala-sbt.org/1.0/docs/Howto-Scaladoc.html

