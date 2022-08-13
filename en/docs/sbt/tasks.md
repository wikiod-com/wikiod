---
title: "Tasks"
slug: "tasks"
draft: false
images: []
weight: 9997
type: docs
toc: true
---

## Create a Simple Task
All that is needed to define a task is a declaration of it's type and a description:

    lazy val exampleTask = taskKey[Unit]("An example task that will return no value.")

Because `Unit` is the type, this task is composed entirely of side-effects. Once defined, to implement actions:

    exampleTask := {
      val s: TaskStreams = streams.value
      s.log.info("The example task was executed.")
    }

If these are defined in `build.sbt`, you can load the project and execute it:

    > exampleTask
    [info] The example task was executed.

