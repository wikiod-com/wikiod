---
title: "Pub"
slug: "pub"
draft: false
images: []
weight: 9991
type: docs
toc: true
---

When you install the Dart SDK, one of the tools that you get is pub. The pub tool provides commands for a variety of purposes. One command installs packages, another starts up an HTTP server for testing, another prepares your app for deployment, and another publishes your package to [pub.dartlang.org][1]. You can access the pub commands either through an IDE, such as WebStorm, or at the command line.

For an overview of these commands, see [Pub Commands][2].


  [1]: https://pub.dartlang.org/
  [2]: https://www.dartlang.org/tools/pub/cmd

## pub build
Use pub build when you’re ready to deploy your web app. When you run pub build, it generates the [assets][1] for the current package and all of its dependencies, putting them into new directory named build.

To use `pub build`, just run it in your package’s root directory. For example:

    $ cd ~/dart/helloworld
    $ pub build
    Building helloworld......
    Built 5 files!


  [1]: https://www.dartlang.org/tools/pub/glossary#asset

## pub serve
This command starts up a development server, or dev server, for your Dart web app. The dev server is an HTTP server on localhost that serves up your web app’s [assets][1].

Start the dev server from the directory that contains your web app’s `pubspec.yaml` file:

    $ cd ~/dart/helloworld
    $ pub serve
    Serving helloworld on http://localhost:8080


  [1]: https://www.dartlang.org/tools/pub/glossary#asset

