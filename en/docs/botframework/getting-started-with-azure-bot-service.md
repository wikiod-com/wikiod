---
title: "Getting started with Azure Bot Service"
slug: "getting-started-with-azure-bot-service"
draft: false
images: []
weight: 9981
type: docs
toc: true
---

The **Azure Bot Service** provides an integrated environment that is purpose-built for bot development, enabling you to build, connect, test, deploy and manage intelligent bots, all from one place. You can write your bot in C# or Node.js directly in the browser using the Azure editor, without any need for a tool chain. You can also increase the value of your bots with a few lines of code by plugging into Cognitive Services to enable your bots to see, hear, interpret & interact in more human ways

## Getting started with Azure Bot Service
Create a new bot in Azure following this [documentation](https://docs.botframework.com/en-us/azure-bots/build/first-bot/)

Login into Azure and from Intelligence + Analytics category, select Bot Service and provide required information.

[![enter image description here][1]][1]

Enter the required details for the bot, they are identical to the required details of an App Service,for example App Name, Subscription, Resource Group and Location.  Once entered, click the Create button.

[![enter image description here][2]][2]

Once created/deployed, navigate to the Bot by clicking on the link either from the main page, if you pinned it to the dashboard or open the resource group and click the link.

**Remember** that there may be a slight delay before the splash screen displays indicating that the Bot Service is generating your bot; don’t click Create bot again.

[![enter image description here][3]][3]

After confirming the deployment generate and configure microsoft app ID and app password.

[![enter image description here][4]][4]

Select programming language of your choice (*I selected C#*) and select **Question and Answer template**.

[![enter image description here][5]][5]

This will further give options such as *existing knowledge base already created* or to *generate a new one*. As I had already created a knowledge base with my subscription, I selected it. This made my work much easier, reducing the time required to include all the keys in the Azure bot code related to the Knowledge base.

[![enter image description here][6]][6]

After *clicking* create bot, the Azure editor will contain all files and we can *test* the functional bot in the **chat control**. The **default code** is generated when you create Bot Azure Service. You can change the logic of the code based on your requirements.

[![enter image description here][7]][7]

Create a new repository in the github to configure continuous deployment with Azure and copy the SSH key.


[![enter image description here][8]][8]

Clone the repository in visual studio using the SSH key copied in github.

[![enter image description here][9]][9]

Download files from Azure Bot Service to the repository cloned location.

[![enter image description here][10]][10]

Select configure continuous integration tab to configure the settings.

[![enter image description here][11]][11]

Select the deployment source. I selected Github and the repository to be synced.

[![enter image description here][12]][12]

Configure the project and the branch to the code be pushed.

[![enter image description here][13]][13]

Configure the performance test using Team Services Account.

[![enter image description here][14]][14]

Configure with all subscription, location details etc.

[![enter image description here][15]][15]

Once all the deployment settings are configured, the initial commit is deployed.

[![enter image description here][16]][16]

Create a new html file in visual studio to customize all the configured channels embed codes.

[![enter image description here][17]][17]

Configure it with different channels we want the bot to work with. To configure it with skype, add to contacts where you get the link to be shared to chat with the bot.

[![enter image description here][18]][18]

The test skype preview looks as below.

[![enter image description here][19]][19]

Configure with the email as below.

[![enter image description here][20]][20]

Get the embed code of different channels so that the users can have access to bot through configured channels.

[![enter image description here][21]][21]

Configure the Web Chat by customizing the name of the site.

[![enter image description here][22]][22]

Get the embed code including secret key.

[![enter image description here][23]][23]

Once all the code is updated in visual studio, push it to github and then sync the code in Azure with github.

[![enter image description here][24]][24]

The code sync reflects in both Azure as well as github as below.

[![enter image description here][25]][25]

All deployment and performance details can be visualized in Azure Bot Service.

[![enter image description here][26]][26]

You can *set* the breakpoints in Visual Studio and run locally in the emulator and **debug** following this [documentation](https://docs.botframework.com/en-us/azure-bot-service/manage/debug/#debugging-c-bots-built-using-the-azure-bot-service-on-windows).

You can track the build updates and errors using *Azure Analytics*.

Looking forward to update the Bot and move to next level.


  [1]: https://i.stack.imgur.com/52DP4.png
  [2]: https://i.stack.imgur.com/PogQA.png
  [3]: https://i.stack.imgur.com/sm1VI.png
  [4]: https://i.stack.imgur.com/2fGOa.png
  [5]: https://i.stack.imgur.com/Fv2B2.png
  [6]: https://i.stack.imgur.com/tRyus.png
  [7]: https://i.stack.imgur.com/RbWyt.png
  [8]: https://i.stack.imgur.com/AEWCm.png
  [9]: https://i.stack.imgur.com/gYYfh.png
  [10]: https://i.stack.imgur.com/eroMh.png
  [11]: https://i.stack.imgur.com/stTCn.png
  [12]: https://i.stack.imgur.com/A7LeW.png
  [13]: https://i.stack.imgur.com/xJgiL.png
  [14]: https://i.stack.imgur.com/1dKau.png
  [15]: https://i.stack.imgur.com/ka0zD.png
  [16]: https://i.stack.imgur.com/5fahf.png
  [17]: https://i.stack.imgur.com/AZpi4.png
  [18]: https://i.stack.imgur.com/PZREe.png
  [19]: https://i.stack.imgur.com/t3jwZ.png
  [20]: https://i.stack.imgur.com/jtGy9.png
  [21]: https://i.stack.imgur.com/Fwjur.png
  [22]: https://i.stack.imgur.com/kKXRL.png
  [23]: https://i.stack.imgur.com/h70o1.png
  [24]: https://i.stack.imgur.com/L3Ftv.png
  [25]: https://i.stack.imgur.com/Sq3l4.png
  [26]: https://i.stack.imgur.com/0YZrX.png

