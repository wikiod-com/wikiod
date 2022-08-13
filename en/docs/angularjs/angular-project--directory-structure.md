---
title: "Angular Project - Directory Structure"
slug: "angular-project---directory-structure"
draft: false
images: []
weight: 9974
type: docs
toc: true
---

## Directory Structure
A common question among new Angular programmers - "What should be the structure of the project?". A good structure helps toward a scalable application development. When we start a project we have two choices, **Sort By Type** (left) and **Sort By Feature** (right). The second is better, especially in large applications, the project becomes a lot easier to manage.

[![enter image description here][1]][1]

## **Sort By Type** (left) ##

The application is organized by the files' type.

 - **Advantage** - Good for small apps, for programmers only starting to use Angular, and is easy to convert to the second method. 
- **Disadvantage** - Even for small apps it starts to get more difficult to find a specific file. For instance, a view and it's controller are in two seperate folders.

## **Sort By Feature** (right) ##

The suggested organizing method where the filed are sorted by features' type.

All of the layout views and controllers go in the layout folder, the admin content goes in the admin folder, and so on.

- **Advantage** -  When looking for a section of code determining a certain feature it's all located in one folder.
- **Disadvantage** - Services are a bit different as they “service” many features.

You can read more about it on [Angular Structure: Refactoring for Growth][2]


The suggested file structure combining both of the aforementioned methods:

[![enter image description here][3]][3]

*Credit to:* [Angular Style Guide][4]


  [1]: http://i.stack.imgur.com/TTloJ.jpg
  [2]: https://johnpapa.net/angular-growth-structure/
  [3]: http://i.stack.imgur.com/nxXRu.png
  [4]: https://github.com/mgechev/angularjs-style-guide#directory-structure

