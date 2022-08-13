---
title: "co-processes"
slug: "co-processes"
draft: false
images: []
weight: 9989
type: docs
toc: true
---

## Hello World
    # create the co-process
    coproc bash
    
    # send a command to it (echo a)
    echo 'echo Hello World' >&"${COPROC[1]}"
    
    # read a line from its output
    read line <&"${COPROC[0]}"
    
    # show the line
    echo "$line"
The output is "Hello World".

