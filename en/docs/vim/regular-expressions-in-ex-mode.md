---
title: "Regular expressions in Ex Mode"
slug: "regular-expressions-in-ex-mode"
draft: false
images: []
weight: 9994
type: docs
toc: true
---

## Edit a regular expression in Ex mode
Suppose you are searching for a `Title Case` pattern in a large text file and you want to edit a incorrect regular expression:

 1. First, go into `Ex` mode by typing `q:`
 2. You will now see all the commands that you typed in `commandline` mode, press `j` to go the regular expression you want to edit (`/\v[A-Z]\w+\s[A-Z]\w+`)
 3. Once done, press `ESC` to go to normal mode
 4. Then press `Enter` to run the search patten

Here is a screen shot demonstrating a `Title Case` search

[![enter image description here][1]][1]

[![enter image description here][2]][2]


  [1]: http://i.stack.imgur.com/KE4wK.png
  [2]: http://i.stack.imgur.com/H7J89.png

