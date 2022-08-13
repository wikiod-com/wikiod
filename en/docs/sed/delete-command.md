---
title: "Delete command"
slug: "delete-command"
draft: false
images: []
weight: 9978
type: docs
toc: true
---

## Delete one line containing a pattern
Given a file **file.txt** with the following content:

    line 1
    line 2
    line 3

You can delete a line from file content with the `d` command. 

The pattern to match is surrounded with default  `/` delimiter and the `d` command follows the pattern:

    sed '/line 2/d' file.txt

The above command will output:

    line 1
    line 3

To edit the file *in place*, use the `-i` option:

    sed -i '/line 2/d' file.txt


