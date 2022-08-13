---
title : Docker Tutorial
slug : docker-tutorial
weight : 9444
draft : false
images : []
type : docs
---

Docker is an [open-source][1] project that automates the deployment of applications inside [software containers.][2] These application containers are similar to lightweight virtual machines, as they can be run in isolation to each other and the running host.

Docker requires features present in recent linux kernels to function properly, therefore on Mac OSX and Windows host a virtual machine running linux is required for docker to operate properly. Currently the main method of installing and setting up this virtual machine is via [Docker Toolbox](https://www.docker.com/toolbox) that is using VirtualBox internally, but there are plans to integrate this functionality into docker itself, using the native virtualisation features of the operating system. On Linux systems docker run natively on the host itself.


  [1]: https://en.wikipedia.org/wiki/Open-source_software
  [2]: https://en.wikipedia.org/wiki/Operating-system-level_virtualization

