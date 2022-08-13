---
title: "Debug remote liferay server via Eclipse"
slug: "debug-remote-liferay-server-via-eclipse"
draft: false
images: []
weight: 9982
type: docs
toc: true
---

## Debug remote liferay server via Eclipse(without Liferay Remote IDE connector eclipse plugin)
To debug a server instance, start in debug mode. To do so, configure these parameters to be passed to the server:

    -Xdebug -Xrunjdwp:transport=dt_socket,address=8000,server=y,suspend=n
to setenv.bat(Windows) or setenv.sh(Unix)

These initialize the server in debug mode, and listen for debug requests on the given port. Start the server and post the config.

In eclipse, the remote debug config needs to be configured to attach the source to the remote server. Follow the given steps:

 1. Go to *Run->Debug Configurations->Remote java application*:

[![enter image description here][1]][1]

 2. Create a new configuration from Remote Java Application:

[![enter image description here][2]][2]
 
 3. Enter the given details:


     Host name:localhost(For local instance)or Ip of the machine 
     Port:8000(By default)

 4. Click *Debug* to intitiate attachments to the server instance.



  [1]: https://i.stack.imgur.com/FtGBh.png
  [2]: https://i.stack.imgur.com/pkkM9.png

