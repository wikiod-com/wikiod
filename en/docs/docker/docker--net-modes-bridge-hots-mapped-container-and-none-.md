---
title: "Docker --net modes (bridge, hots, mapped container and none)."
slug: "docker---net-modes-bridge-hots-mapped-container-and-none"
draft: false
images: []
weight: 9979
type: docs
toc: true
---

Getting Started

**Bridge Mode**
It's a default and attached to docker0 bridge. Put container on a completely separate network namespace.

**Host Mode**
When container is just a process running in a host, we'll attach the container to the host NIC.

**Mapped Container Mode**
This mode essentially maps a new container into an existing containers network stack. It's also called 'container in container mode'.

**None**
It tells docker put the container in its own network stack without configuration

## Bridge Mode, Host Mode and Mapped Container Mode
**Bridge Mode**

    $ docker run –d –-name my_app -p 10000:80 image_name

Note that we did not have to specify **--net=bridge** because this is the default working mode for docker. This allows to run multiple containers to run on same host without any assignment of dynamic port. So **BRIDGE** mode avoids the port clashing and it's safe as each container is running its own private network namespace.

**Host Mode**

    $ docker run –d –-name my_app –net=host image_name

As it uses the host network namespace, no need of special configuraion but may leads to security issue.

**Mapped Container Mode**

This mode essentially maps a new container into an existing containers network stack. This implies that network resources such as IP address and port mappings of the first container will be shared by the second container. This is also called as 'container in container' mode. 
Suppose you have two contaienrs as web_container_1 and web_container_2 and we'll run web_container_2 in mapped container mode. Let's first download web_container_1 and runs it into detached mode with following command,

    $ docker run -d --name web1 -p 80:80 USERNAME/web_container_1

Once it’s downloaded let’s take a look and make sure its running. Here we just mapped a port into a container that's running in the default bridge mode. Now, let’s run a second container in mapped container mode.  We’ll do that with this command.

    $ docker run -d --name web2 --net=container:web1 USERNAME/web_container_2

Now, if you simply get the interface information on both the contaienrs, you will get the same network config. This actually include the HOST mode that maps with exact info of the host. The first contaienr ran in default bridge mode and second container is running in mapped container mode. We can obtain very similar results by starting the first container in host mode and the second container in mapped container mode.

