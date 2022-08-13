---
title: "Build configuration tutorial"
slug: "build-configuration-tutorial"
draft: false
images: []
weight: 9987
type: docs
toc: true
---

## Creating a Build Configuration Manually
Inside a Project configuration, you can `Create build configuration`: `Manually`

Provide a `Name` and a `Description`. The Build Configuration ID is generated from the ProjectName, and the Build Configuration Name's.
[![Build Configuration for Hello World][1]][1]

Once your configuraion is Saved, you can specify a Version Control Settings. This will point to your repository where TeamCity will find the sources of your application to build.

We will create a new Build Steps. The step will be a Command Line Step, which will Display "Hello World" inside the logs.

[![enter image description here][2]][2]

Once you have saved this step, you can run the Build configuration, and inside the `Build Log` tab, you will find somethingh like this:

     [00:00:00]Step 1/1: Hello World (Command Line)
     [00:00:00][Step 1/1] Starting: C:\TeamCity\buildAgent\temp\agentTmp\custom_script4323583874650153904.cmd
     [00:00:00][Step 1/1] in directory: C:\TeamCity\buildAgent\work\362562ae9b31fb9a
     [00:00:00][Step 1/1] Hello World
     [00:00:00][Step 1/1] Process exited with code 0

The Step 1/1 Name is what you defined inside the box `Step Name`. This configuration will create a temporary script, here `custom_script4323583874650153904.cmd` with the following line of code : 

     echo Hello World


  [1]: http://i.stack.imgur.com/7I6st.png
  [2]: http://i.stack.imgur.com/sKvKD.png

