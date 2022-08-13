---
title: "Advanced sed commands"
slug: "advanced-sed-commands"
draft: false
images: []
weight: 9959
type: docs
toc: true
---

## Insert a new line before matching pattern - using eXchange
Given a file file.txt with the following content:

    line 1
    line 2
    line 3

You can add a new line using below command

    sed '/line 2/{x;p;x;}' file.txt

The above command will output

    line 1
    
    line 2
    line 3

**Explanation:**

`x` command is eXchange. sed has a buffer that you can use to store some lines. This command exchanges this buffer with current line (so current line goes to this buffer and buffer content becomes current line).

`p` command prints current line. 


