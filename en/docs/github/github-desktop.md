---
title: "GitHub Desktop"
slug: "github-desktop"
draft: false
images: []
weight: 9994
type: docs
toc: true
---

How to install and work with GitHub Desktop?

GitHub Desktop is -as the name implies- an desktop environment for Windows and MacOS which includes the main features of Git like cloning, pushing, pulling (sync in GitHub Desktop), merging...

The Desktop clients main purpose is to deliver an easier way of working with git (and GitHub). In the background it uses the same commands as most users would use from the commandline.

## Installation and Setup
The installation is quite simple as there are seperate installers for MacOS and Windows machines available [here][1]. Currently two versions are for download: one beta and one stable. 

Setup will start after you downloaded the program and you´ll need to login with your GitHub credentials. That is really the only step because after that you can start to create a repository or clone one.

>Note: during the installation not only GitHub Desktop will be installed but Git too. So you don´t need to install it seperate.


  [1]: https://desktop.github.com/

## Cloning a repository
As it is with GitHub Desktop most of the work is pretty simple: You select "Clone a repository" (In the stable version the plus on the upper left) and there are some repositories (your own and the repos from each company you are in) recommended. Alternatively you can paste a link to any other repository you might want to clone.

> Note: in the newer version (beta) there are no (not jet?) recommendations.

[![Clone dialog old version][1]][1]


  [1]: https://i.stack.imgur.com/wKIlb.png

## Branching
You can select a branch at the upper left. When you selected the right branch you need to press the sync button (upper right) which does now the same as ```git checkout BRANCHNAME```. 
>In the older version you are able to view 2 different branches at once and compare the pushes. Furthermore you could view a timeline of your project (see beneath)

[![Timeline with difference view][1]][1]

**Creating a new branch**

You can create a new branch by clicking on the branch symbol (old client) or under ```File --> New Branch```.
> Note that you can select of which branch the new branch uses as base by clicking on the branch name.

  [1]: https://i.stack.imgur.com/oP3zY.png

## Push and Pull (or: the Sync Button)
**Pull (Sync)**

Like in the command line you need to pull the current state of the repository once in a while. In GitHub Desktop this process is called by the ```sync``` Button at the top right corner. 

**Push**

When you made local changes and want to push them you make a commit by writing something into the summary textbox. Then you press ```Commit to YOURCURRENTBRANCH```. Now you´ll need to press the sync button and your push is made. 

>Note: You can use emoticons, mentions and referals to other commits or issues directly from the textbox.

So the **Sync** button can be used to ```Push```,```Pull``` or ```Checkout```.

