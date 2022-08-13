---
title: "Runtime Commands"
slug: "runtime-commands"
draft: false
images: []
weight: 9984
type: docs
toc: true
---

## Adding shutdown hooks
Sometimes you need a piece of code to execute when the program stops, such as releasing system resources that you open. You can make a thread run when the program stops with the `addShutdownHook` method:

    Runtime.getRuntime().addShutdownHook(new Thread(() -> {
        ImportantStuff.someImportantIOStream.close();
    }));

