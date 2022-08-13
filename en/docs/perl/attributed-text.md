---
title: "Attributed Text"
slug: "attributed-text"
draft: false
images: []
weight: 9996
type: docs
toc: true
---

## Printing colored Text
    #!/usr/bin/perl
    
    use Term::ANSIColor;
    
    print color("cyan"), "Hello", color("red"), "\tWorld", color("green"), "\tIt's Me!\n", color("reset");

[![enter image description here][1]][1]


  [1]: http://i.stack.imgur.com/FXQAm.png

