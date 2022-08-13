---
title: "Getting started with daily development"
slug: "getting-started-with-daily-development"
draft: false
images: []
weight: 9998
type: docs
toc: true
---

## Daily continuous development example with Scala
<!-- language: lang-bash -->

    # install sbt with homebrew (if you didn't)
    brew install sbt

    # - create a new scala project
    # - name your project name when asked like: hello-world
    sbt new sbt/scala-seed.g8

    # go to the new project directory that you named
    cd hello-world

    # run sbt to open the sbt shell
    sbt

    # run your project in continuous running mode
    ~ run

    # to continuously see the test outputs
    # open up a new terminal tab, run sbt, type:
    ~ test

    # to continuously compile
    # open up a new terminal tab, run sbt, type:
    ~ compile

**~** used for continuous operations in sbt as seen above.

