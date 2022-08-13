---
title: "How Eclipse Remote Debugging works behind the scenes"
slug: "how-eclipse-remote-debugging-works-behind-the-scenes"
draft: false
images: []
weight: 9993
type: docs
toc: true
---

## How does Eclipse Remote Debugging work behind the scences
Eclipse debugging starts with what is referred to as Agents.

The **JVM**, which runs the complied `.class` sources has a feature that allows externally libraries (written in either Java or C++) to be injected into the JVM, just about runtime. These external libraries are referred to as Agents and they have the ability to modify the content of the `.class` files been run. These Agents have access to functionality of the JVM that is not accessible from within a regular Java code running inside the JVM and they can be used to do interesting stuff like injecting and modify the running *source code, profiling* etc. Tools like **JRebel** makes use of this piece of functionality to achieve their magic.

And to pass an Agent Lib to a JVM, you do so via start up arguments, using the 
    
     agentlib:libname[=options] format.

We were actually passing an Agent Lib named `jdwp` to the JVM running Tomcat. The `jdwp` is a JVM specific, optional implementation of the *JDWP (Java Debug Wire Protocol)* that is used for defining communication between a debugger and a running JVM. It’s implementation, if present is supplied as a native library of the JVM as either `jdwp.so` or `jdwp.dll`

**So what does it do?**

In simple terms, the jdwp agent we pass is basically serving the function of being a link between the JVM instance running an application and a Debugger (which can be located either remote or local). Since it is an Agent Library, It does have the ability to intercept the running code, create a bridge between the JVM and a debugger, and have the functionality of a debugger applied on the JVM.
Since in the JVM architecture, the debugging functionality is not found within the JVM itself but is abstracted away into external tools (that are aptly referred to as debuggers), these tools can either reside on the local machine running the JVM being debugged or be run from am external machine. It is this de-coupled, modular architecture that allows us to have a JVM running on a remote machine and using the JDWP, have a remote debugger be able to communicate with it.  
In short, this is how Eclipse debugger works.


