---
title: "Create a watcher"
slug: "create-a-watcher"
draft: false
images: []
weight: 9988
type: docs
toc: true
---

## Watcher task
`config.paths.html` represents the path to your HTML file. 

    gulp.task("watch", function() {
        gulp.watch(config.paths.html, ["html"]);
    });

The task should be added to default as well:

    gulp.task("default", ["html", "watch"]);

