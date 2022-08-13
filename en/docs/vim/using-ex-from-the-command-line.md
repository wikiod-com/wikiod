---
title: "Using ex from the command line"
slug: "using-ex-from-the-command-line"
draft: false
images: []
weight: 9993
type: docs
toc: true
---

## Substitution from the command line
If you would like to use vim in a manner similar to `sed`, you may use the `-c` flag to run an ex command from the command line. This command will run automatically before presenting the file to you. For example, to replace `foo` with `bar`:

    vim file.txt -c "s/foo/bar"

This will open up the file with all instances of `foo` replaced with `bar`. If you would to like to make changes to the file *without* having to manually save, you can run multiple ex commands, and have the last command write and quit. For example:

    vim file.txt -c "s/foo/bar" -c "wq"

*Important note:*

You can *not* run multiple ex commands separated by a bar `|`. For example

    vim file.txt -c "s/foobar | wq"

Is *not* correct; however, it CAN be done if you use `ex`.

    ex -c ":%s/this/that/g | wq" file.txt

