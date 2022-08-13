---
title: "Deploying a Plugin"
slug: "deploying-a-plugin"
draft: false
images: []
weight: 9979
type: docs
toc: true
---

## Deploying to Glassfish
So, you make first a .war file let's say a portlet of name `<YOUR PLUGIN>.war`. You wanna have it running on a glassfish domain under Liferay portal.

Steps to success:

 1. Navigate to Control Panel -> Plugins Installation on Liferay
 2. Hit **Install new portlets**
 3. Hit **Configuration**
 4. Fill in to Deploy Directory a new place for deployment let's say `<YOUR DOMAIN>/autodeploy2`
 5. Check that in the next line target is `<YOUR DOMAIN>/autodeploy` (it is the Glassfish default deployment directory)
 6. Hit **Save**

Now deployment will be done by copy pasting files to that new directory `<YOUR DOMAIN>/autodeploy2`. The rest of it is handled automatically. Setting takes action immediately.

Done with deployment: Make a victory jig and enjoy :)

..you stop dancing and face a bug. You want a new revision to be deployed.. In this case, continue reading.

So, you have built your war again and want to re-deploy. Do the following:

 1. undeploy old stuff from `<YOUR DOMAIN>/autodeploy` folder by deleting the war file. Don't delete any other file.
 2. result is that `<YOUR PLUGIN>.war_UnDeployed` file will appear.
 3. deploy new file by copying the newly built war in `<YOUR DOMAIN>/autodeploy2` folder.
 4. result is that `<YOUR PLUGIN>.war_deployed` will appear in `<YOUR DOMAIN>/autodeploy` folder.

Make a dance again :)

