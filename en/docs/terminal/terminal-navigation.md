---
title: "Terminal Navigation"
slug: "terminal-navigation"
draft: false
images: []
weight: 9991
type: docs
toc: true
---

This topic contains information about navigating throughout the filesystem using the terminal. 

## Basic Navigation Commands
**Change Directory**

Navigate to a directory

    cd [directory]

Example: Changing to the `projects` directory

    username$ cd projects

<br>

Navigate to the root directory (regardless of working directory)

    cd /

<br>

Navigate to the parent directory (the directory containing the working directory)

    cd ..

<br>

Navigate to the home directory

    cd ~

<br>

<br>

**Print Working Directory**

Prints the path of the directory that you are working in

    pwd

Example: Printing the working directory while in the `projects` folder

    username$ pwd
    /Users/username/projects

<br>

**List**

List the files that are in the current working directory

    ls

Example: Listing the files in the `projects` directory

    username$ ls
    PythonProject
    JavaProject
    screenshot.png
    index.html
    paper.pdf

Files are listed with their file extension, directories are listed with no extension.

Use `ls -l` to get more information about files (size, timestamp, owner)

