---
title: "Simulator Builds"
slug: "simulator-builds"
draft: false
images: []
weight: 9993
type: docs
toc: true
---

Where to find simulator build ? 

Go to ~/Library/Developer/CoreSimulator/Devices/

You will find directories with alphanumeric names

then click on the one of the directories and make following selection

Data/Containers/Bundle/Application/

Again you will find directories with alphanumeric names if you click on that you will find

Simulator build over there

Note:

Installing iOS device build on simulator will not work out.

iPhone simulator uses i386 architecture
iPad simulator builds uses x8

## Installing the build manually on simulator
    xcrun simctl install booted *.app

