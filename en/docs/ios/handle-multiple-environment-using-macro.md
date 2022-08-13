---
title: "Handle Multiple Environment using Macro"
slug: "handle-multiple-environment-using-macro"
draft: false
images: []
weight: 9986
type: docs
toc: true
---

## Handle multiple environment using multiple target and macro
For example, we have two environments: CI - Staging and want to add some customizations for each environment. Here I will try to customize server URL, app name.

First, we create two targets for 2 environments by duplicating the main target:

[![Create targets][1]][1]

For each target, we will define a custom macro. Here I will define macro named "CI" in build settings of target CI, macro named "STAGING" for target Staging.

The development target (MultipleEnvironment target):
[![DEV][2]][2]

Target CI:

[![enter image description here][3]][3]

Target Staging:

[![enter image description here][4]][4]

Create scheme for each target:
[![enter image description here][8]][8]

We will create a header file to define SERVER URL as below:

[![enter image description here][5]][5]

It means, 
- If we run/archive using the default target (MultipleEnvironment), the SERVER_URL is http://192.168.10.10:8080/ 
- If we run/archive using CI target, the SERVER_URL is http://ci.api.example.com/
- If we run/archive using STAGING target, the SERVER_URL is http://stg.api.example.com/


If you want to do more customize, for example: Change app name for each target:

[![enter image description here][6]][6]

[![enter image description here][7]][7]

Almost done. Now we want to show current SERVER_URL to main screen:
[![enter image description here][9]][9]

Now, let's see if we run the app with the default target (MultipleEnvironment)
[![enter image description here][10]][10]
[![enter image description here][11]][11]

**CI target**:

[![enter image description here][12]][12]
[![enter image description here][13]][13]

**Staging target**:

[![enter image description here][14]][14]
[![enter image description here][15]][15]

As you can see, value of SERVER_URL and app name is changed for each target :)

  [1]: http://i.stack.imgur.com/724av.png
  [2]: http://i.stack.imgur.com/U42W4.jpg
  [3]: http://i.stack.imgur.com/UewwM.jpg
  [4]: http://i.stack.imgur.com/yyvPz.jpg
  [5]: http://i.stack.imgur.com/FYQyz.png
  [6]: http://i.stack.imgur.com/WbNfj.jpg
  [7]: http://i.stack.imgur.com/3eKJ0.jpg
  [8]: http://i.stack.imgur.com/k1uFR.png
  [9]: http://i.stack.imgur.com/HzQIX.jpg
  [10]: http://i.stack.imgur.com/vjuaX.png
  [11]: http://i.stack.imgur.com/N0ysl.png
  [12]: http://i.stack.imgur.com/A0jKT.png
  [13]: http://i.stack.imgur.com/6DlmC.png
  [14]: http://i.stack.imgur.com/YaEAJ.png
  [15]: http://i.stack.imgur.com/SPfU7.png

