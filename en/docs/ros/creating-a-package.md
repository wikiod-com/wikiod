---
title: "Creating a package"
slug: "creating-a-package"
draft: false
images: []
weight: 9998
type: docs
toc: true
---

This tutorial shows how to create a package in ROS. Packages sit inside workspaces, in the src directory. Each package directory must have a `CMakeLists.txt` and a `package.xml` files.

## Creating a package using rospy
Assuming a workspace named `workspace_name` has been previously created in the home directory, a package named `package_name` can be created by executing the following command lines.

    $ cd ~/workspace_name/src/
    $ catkin_create_pkg package_name rospy



