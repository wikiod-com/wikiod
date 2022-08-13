---
title: "Vimscript"
slug: "vimscript"
draft: false
images: []
weight: 9980
type: docs
toc: true
---

The commands in a Vimscript file are executed in `command mode` by default. Therefore all non-`command mode` directives should be prefixed.

## Hello World
When attempting to print something for debugging in vimscript, it is tempting to simply do the following.

    echo "Hello World!"

However, in the context of a complex plugin, there are often many other things happening right after you attempt to print your message, so it is important to add `sleep` after your message so you can actually see it before it disappears.

    echo "Hello World!"
    sleep 5

## Using Normal Mode Commands in Vimscript
Since a Vimscript file is a collection of Command mode actions, the user needs to specify that the desired actions should be executed in normal mode.

Therefore executing a normal mode command like `i`, `a`, `d` etc. in Vimscript is done by prepending the command with `normal`:

Going to the bottom of the file and selecting the last 5 rows:

    normal GV5k

Here the `G` instructs vim to change the cursor position to the last row, the `V` to go to linewise visual mode , and the `5k` to go 5 rows up.

Inserting your name at the end of the row:

    normal ABoris

where the `A` puts the editor in insert mode at the end of the row and the rest is the text to insert.

