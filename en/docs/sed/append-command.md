---
title: "Append command"
slug: "append-command"
draft: false
images: []
weight: 9990
type: docs
toc: true
---

## Insert line after first match
Given a file file.txt with the following content:

    line 1
    line 2
    line 3

You can add a new line after first matching line with the `a` command.

For portable use the `a` command must be followed immediately by an escaped newline, with the text-to-append on its own line or lines.

    sed '
    /line 2/a\
    new line 2.2
    ' file.txt

<!-- if version [eq GNU sed] --> 

Some versions of `sed` allow the text-to-append to be inline with the `a` command:

    sed '/line 2/a new line 2.2' file.txt

<!-- end version if -->

The above commands will output:

    line 1
    line 2
    new line 2.2
    line 3


