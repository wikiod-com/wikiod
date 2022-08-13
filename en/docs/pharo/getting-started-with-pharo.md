---
title: "Getting started with pharo"
slug: "getting-started-with-pharo"
draft: false
images: []
weight: 1
type: docs
toc: true
---

## Installation or Setup
**Easy Install**

Go to http://pharo.org/download and select a fitting download and run it.

**Details**

There are a lot of different ways to install Pharo. Pharo itself consists of a vm and an image. In addition it needs its sources and plugins, and has some dependencies: 

 - It is a cross-platform environment, running on OS-X (and iOS), Windows and several unix variants (a.o. Ubuntu and Android). 
 - It runs on a virtual machine that can run on several processor architectures (Intel, ARM). The virtual machine is shared with Squeak, Cuis and Newspeak. With Pharo 5 a new and much faster vm has been introduced using a different image format and FFI. 
 - There are 32-bit and 64-bit variants.
 - In addition to the standard image there is the PharoLauncher that integrates with our CI infrastructure and supports downloading and running all kinds of images, a.o. preconfigured seaside, magritte and moose ones, older releases and the latest development versions of Pharo.   

