---
title: "Getting started with yii"
slug: "getting-started-with-yii"
draft: false
images: []
weight: 1
type: docs
toc: true
---

## Installation or Setup
**Setup for Yii 1.1**

**Step 1 - downloading Yii**

Download the Yii framework bundle from the [Yii website](http://www.yiiframework.com/download/)

Inside the downloaded bundle there are 3 folders, namely:

    demos
    framework
    requirements

`demos`, as the name suggests contains a number of demo Yii applications. 

`framework` contains the Yii framework. This is the main folder we will use for the setup

`requirements` contains code to check if a server meets the requirements for running Yii

Copy the `framework` folder to your local server. It's recommended to keep the `framework` folder in the root directory of your application. In this setup guide we will be using `localhost/yii-setup/` as our root project directory


**Step 2 - the command line**

Open the command line and enter the framework folder. For this example we would go to
  > c:\wamp\www\yii-setup\framework\

We will now use `yiic` to generate a skeleton application. We do this by entering the command:

 > yiic webapp path\to\root\directory

Where path/to/root/directory will be the path to your root directory, so in our example the command would be:

 > yiic webapp c:\wamp\www\yii-setup\

If you receive an error at this point, your command line is not configured to execute php. You will need to enable php execution from the command line to continue. Otherwise,
you will be prompted if you would like to create a new application at the entered path. Press `y` and hit the return key

Your Yii skeleton application will be created under the specified path

## API


 - Class Reference - [API v1.0][1]
 - Class Reference - [API v1.1][2]

  [1]: http://www.yiiframework.com/doc/api/1.0
  [2]: http://www.yiiframework.com/doc/api/1.1

