---
title: "Tomcat deployment procedure"
slug: "tomcat-deployment-procedure"
draft: false
images: []
weight: 9994
type: docs
toc: true
---

## Procedure when nothing else helps
Once a while concecuent deploys to internal tomcat start giving constant error, without any clear cause (Listener start or ClassNotFoundException). When nothing seems to cure it, this procedure saves the world:

> 1 delete Servers folder
> 
> 2 restart Eclipse
> 
> 3 create new server, add project and start

Works like charm and is not so lengthy.

If from some reason this fails, my original lengthy procedure (where the other is a short cut that should do the same) is here:

> 1 stop server
>
> 2 project -> clean
>
> 3 project build (I had automatic build disabled)
>
> 4 delete server
>
> 5 delete Servers folder
>
> 6 restart Eclipse
>
> 7 create new server, add project and start.

With this seven step thing problems with deploy never come out of your code and control.

**Note:**

*You dont't need else than page refresh if all goes smoothly. This procedure is done once per error message to be sure you get rid of the unclear error, if your code looks ok and you kind of did nothing to receive the error. The error is either containing word ClassNotFoundException or ListenerStart, depending on environment in use. Note also that this does not cure ClassNotFoundExceptions caused by missing libraries in a project.*

