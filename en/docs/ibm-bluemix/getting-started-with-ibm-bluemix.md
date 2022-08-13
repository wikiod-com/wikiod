---
title: "Getting started with ibm-bluemix"
slug: "getting-started-with-ibm-bluemix"
draft: false
images: []
weight: 1
type: docs
toc: true
---

## Continuous deployment of RESTful API (Node.js) via GitHub using IBM Bluemix Toolchain
# Step 1: Create your GitHub account

If you already have a GitHub account, please proceed to Step 2. Otherwise, please follow below:

1.a Go to [Github](https://github.com) page.

1.b Enter your desired username, your email address and then your desired password. Afterwards, click the **Sign up for GitHub** button.

![Github](https://i.stack.imgur.com/zTG1B.png)

# Step 2: Create your IBM Bluemix account

Please refer to [Bluemix Get Started](https://www.ibm.com/developerworks/learn/cloud/bluemix/quick-start/) on how to create account or follow the previous example on how to create your Bluemix account on this page.

# Step 3: Deploy your Rest API package to GitHub

3.a If you do not know how to use Git, please read the [Git Tutorial]() or you can use the [SourceTree](https://www.sourcetreeapp.com/) a GUI based Git implementation. Please read the [SourceTree Tutorial](https://confluence.atlassian.com/sourcetreekb/commit-push-and-pull-a-repository-on-sourcetree-785616067.html) to learn more about SourceTree.

3.b Upload your Rest API code to GitHub using the **git push** command. In contrast, you can fork or clone my [Rest API](https://github.com/ariescamitan/node-api-with-express) on GitHub then apply your own changes as needed then upload the code to your GitHub account.
Note: My sample Rest API contains a sample implementation of MongoDB and APICache.

# Step 4: Deploy your GitHub repository to IBM Bluemix using Toolchain (Continuous Deployment tool)

4.a Log-on to your [Bluemix](https://console.ng.bluemix.net/dashboard/apps/) account.

![Bluemix Logon][1]

4.b Click Menu to show other items. 

![Menu][2]

Then select "Services". 

![Services][3]

And then, click "DevOps".

![DevOps][4]

4.c Select Toolchains. Then, click **Create a Toolchain** button.

![Toolchains][5]

4.d Select **Other Templates**

![Other Templates][6]

4.e Enter desired **Toolchain Name**, please note this will show in your URL for API's.

![Toolchain Name][7]

4.f Wait for a few seconds for Bluemix to create your Toolchain. Afterwards, click **Add a Tool**.

![Add a Tool][8]

4.g Choose **GitHub**

![Tool Github][9]

4.h Now, we need to link our existing repository on GitHub to this Toolchain. Under **Repository type**, please choose **Existing**. Then, choose the correct repository URL for your Rest API source. After that, it is up to you if you want to track the changes of the repository via Toolchain. To do that please check the **Track deployment of code changes** check box.

![GitHub Tool Config][10]

4.i Now what we need is something to build and deploy our Rest API repository automatically. So we need to add another tool.

![Add tool again][11]

4.j Let's add the Delivery Pipeline

![Choose Delivery Pipeline][12]

Then, name it **Build and Deploy**.

![Build and Deploy][13]

After a few seconds, the newly added Delivery Pipeline will show up. Click it.

![Click the Build and Deploy][14]

4.k We need to add stages to our Delivery Pipeline. Click **Add Stage**.

![Add stage][15]

4.l Create the **Build stage** in which will automatically build our package. Follow the steps on the figure below:

![Build Stage][16]

We need to Add a **JOB** that will run the **npm install** command.

![enter image description here][17]

Select the **Build** option.

![Build Option][18]

Then, select **npm** under the **Builder Type**. And then, under the **Build Shell Command** automatically configure that it will run the **npm install** command.

![npm install][19]

Make sure to click **Save** button to save the changes.

![Save the Build][20]

4.m After creating the build stage, now we need to create the **Deploy** stage. Go and add another stage by clicking the **Add Stage**.

![Add Deploy Stage][21]

Follow the steps defined in the figure below:

![Deploy Config][22]

Then, under **JOBS** tab, click **Add Job**.

![Add Job][23]

And then, select **Deploy** option.

![Deploy Option][24]

Under **Deployer Type** select **Cloud Foundry** (IBM Bluemix default Cloud Service).

![Deployer Type][25]

In this case, I selected **dev** space. For real project, you may want to select a better space that can handle your actual production usage. Afterwards, click the **Save** button.

![dev space][26]

4.n Now all stages are all configured. By default, all stages is in **Stop** status. We need to **Run** all our stages by clicking the **Play** like buttons.

![Run Stages][27]

4.o If you see similar figure below means you successfully configured and implemented our **Continuous Deployment** of our RESTful API's via IBM Bluemix using ToolChain. 

![Success][28]

Try clicking the link as define above figure to see if the API is running as expected.

![JASON][29]

# Step 5: Test the Rest API using PostMan 

Postman is a powerful API testing tool and can be install via Google Chrome as an extension. Make sure on your testing to follow the route convention you defined on your express code. To know more how to use the Postman tool, please see the [Postman Blog](https://www.getpostman.com/docs/blog_mentions).

## Disclaimer:

I do not own any external link given in this example. Credit to those who own those links.


  [1]: https://i.stack.imgur.com/r3TK0.png
  [2]: https://i.stack.imgur.com/zyKSV.png
  [3]: https://i.stack.imgur.com/wiEY3.png
  [4]: https://i.stack.imgur.com/kttra.png
  [5]: https://i.stack.imgur.com/Vo8Of.png
  [6]: https://i.stack.imgur.com/oKSkT.png
  [7]: https://i.stack.imgur.com/EI4Fx.png
  [8]: https://i.stack.imgur.com/cdNwM.png
  [9]: https://i.stack.imgur.com/REOvp.png
  [10]: https://i.stack.imgur.com/5cCFi.png
  [11]: https://i.stack.imgur.com/JtYmp.png
  [12]: https://i.stack.imgur.com/CZWSm.png
  [13]: https://i.stack.imgur.com/GkMbI.png
  [14]: https://i.stack.imgur.com/zFEDr.png
  [15]: https://i.stack.imgur.com/YxBkC.png
  [16]: https://i.stack.imgur.com/EWhFM.png
  [17]: https://i.stack.imgur.com/AW8pj.png
  [18]: https://i.stack.imgur.com/Ft0xx.png
  [19]: https://i.stack.imgur.com/ZvHLb.png
  [20]: https://i.stack.imgur.com/6O3AG.png
  [21]: https://i.stack.imgur.com/NIlZu.png
  [22]: https://i.stack.imgur.com/5PyO7.png
  [23]: https://i.stack.imgur.com/aORrB.png
  [24]: https://i.stack.imgur.com/A8W6Q.png
  [25]: https://i.stack.imgur.com/XL8NW.png
  [26]: https://i.stack.imgur.com/Le0ev.png
  [27]: https://i.stack.imgur.com/QREuQ.png
  [28]: https://i.stack.imgur.com/3h4rq.png
  [29]: https://i.stack.imgur.com/YCuxq.png

## Installation or Setup
Detailed instructions on getting ibm-bluemix set up or installed.

## Getting IBM Bluemix Setup for deploying a node application
# Step 1: Create a bluemix account

[Create an account](https://console.ng.bluemix.net/registration/) at https://console.ng.bluemix.net/registration/

This will set you up with a 30 day trial. You don't have to pay anything for the free resources and you don't have to set up billing until the end of your trial (though not all services will be available).

# Step 2: Install Bluemix and Cloud Foundry Command Line tools

This is optional, you can perform most actions via the web console from step 1.  The CLI utilities can be downloaded from https://new-console.ng.bluemix.net/docs/starters/install_cli.html and you should install both the bluemix cli and the cloud foundry cli.  

## Step 2a: Connect to Bluemix

`$ bluemix api https://api.ng.bluemix.net`

## Step 2b: Login to Bluemix

`$ bluemix login -u username -o org_name -s space_name`

You need to specify your username, org_name, and space_name which you can get from the web console in Step 1.  

After you log in successfully,  you have now setup bluemix.  You can learn more about the CLI and get additional plugins from http://clis.ng.bluemix.net/ui/home.html

