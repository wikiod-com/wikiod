---
title: "Remote Debugging in Eclipse"
slug: "remote-debugging-in-eclipse"
draft: false
images: []
weight: 9990
type: docs
toc: true
---

## Configure Eclipse Remote Debugging for an application
The following are the steps to start an Eclipse remote debugger. This is useful when the application is not started from a server instance within Eclipse. This feature is really powerful and can also help debugging code which resides in the test or production environment. Let's have a look at the settings:

**Eclipse Settings:**  
1.Click the Run Button  
2.Select the Debug Configurations  
3.Select the “Remote Java Application”  
4.New Configuration  
     a)    Name : GatewayPortalProject  
b) Project : GatewayPortal-portlet  
c)    Connection Type: Socket Attach  
d)    Connection Properties:  
i)    localhost   ii)    8787  

**For JBoss:**

1.Change the `/path/toJboss/jboss-eap-6.1/bin/standalone.conf` in your vm as follows:
   Uncomment the following line by removing the #:

    JAVA_OPTS="$JAVA_OPTS -agentlib:jdwp=transport=dt_socket,address=8787,server=y,suspend=n"


**For Tomcat :**

In **catalina.bat** file :

Step 1: 

    CATALINA_OPTS="-Xdebug -Xrunjdwp:transport=dt_socket,address=8000,server=y,suspend=n"

Step 2: 

    JPDA_OPTS="-agentlib:jdwp=transport=dt_socket,address=8000,server=y,suspend=n"

Step 3: Run Tomcat from command prompt like below: 

    catalina.sh jpda start

Then you need to set breakpoints in the Java classes you desire to debug. 
 




