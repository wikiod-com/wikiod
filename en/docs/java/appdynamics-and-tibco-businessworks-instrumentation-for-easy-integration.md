---
title: "AppDynamics and TIBCO BusinessWorks Instrumentation for Easy Integration"
slug: "appdynamics-and-tibco-businessworks-instrumentation-for-easy-integration"
draft: false
images: []
weight: 9988
type: docs
toc: true
---

As AppDynamics aims to provide a way to measure application performance, speed of development, delivery (deployment) of applications is an essential factor in making DevOps efforts a true success.
Monitoring a TIBCO BW application with AppD is generally simple and not time consuming but when deploying large sets of applications rapid instrumentation is key.
This guide shows how to instrument all of your BW applications in a single step without modifying each application before deploying. 

## Example of Instrumentation of all BW Applications in a Single Step for Appdynamics
1. Locate and open your TIBCO BW bwengine.tra file typlically under TIBCO_HOME/bw/5.12/bin/bwengine.tra  (Linux environment)

2. Look for the line that states:
# *** Common variables. Modify these only. ***

3. Add the following line right after that section 
 tibco.deployment=%tibco.deployment%
 

4. Go to the end of the file and add (replace ? with your own values as needed or remove the flag that does not apply):
java.extended.properties=-javaagent\:/opt/appd/current/appagent/javaagent.jar -Dappdynamics.http.proxyHost\=? -Dappdynamics.http.proxyPort\=? -Dappdynamics.agent.applicationName\=? -Dappdynamics.agent.tierName\=? -Dappdynamics.agent.nodeName\=%tibco.deployment% -Dappdynamics.controller.ssl.enabled\=? -Dappdynamics.controller.sslPort\=? -Dappdynamics.agent.logs.dir\=? -Dappdynamics.agent.runtime.dir\=? -Dappdynamics.controller.hostName\=? -Dappdynamics.controller.port\=? -Dappdynamics.agent.accountName\=? -Dappdynamics.agent.accountAccessKey\=?

5. Save file and redeploy. All your applications should now be instrumented automatically at deployment time.


