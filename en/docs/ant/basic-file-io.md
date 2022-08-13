---
title: "Basic File IO"
slug: "basic-file-io"
draft: false
images: []
weight: 9998
type: docs
toc: true
---

## Write a file using echo
Simply specifying a _file_ destination, [`echo`][1] will create, write, or append to a file.

    <echo file=example.txt" append="false">
        hello world
    </echo>


  [1]: https://ant.apache.org/manual/Tasks/echo.html

## Print the contents of a file
To print the contents of a file, we can use [`loadfile`][1] to read a file into a local property, and then use [`echo`][2] to print the value of it.

    <loadfile property="contents" srcFile="example.txt" />
    <echo message="${contents}" />


  [1]: https://ant.apache.org/manual/Tasks/loadfile.html
  [2]: https://ant.apache.org/manual/Tasks/echo.html

