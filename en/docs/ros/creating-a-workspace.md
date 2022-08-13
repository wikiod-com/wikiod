---
title: "creating a workspace"
slug: "creating-a-workspace"
draft: false
images: []
weight: 9998
type: docs
toc: true
---

This tutorial shows how to create a workspace. A workspace is a set of directories in which a related set of ROS code lives. One can have multiple ROS workspaces, but it is possible to work only in one at a time.

## Creating a Workspace
In order to create a workspace, one should run the following in the terminal:

    $ mkdir -p ~/workspace_name/src
    $ cd ~/workspace_name/src
    $ catkin_init_workspace
    $ cd ~/workspace_name/
    $ catkin_make

The previous commands creates a workspace named `workspace_name`. Once a workspace has been created, it is important to source it in order to work with it:

    $ source ~/workspace_name/devel/setup.bash


