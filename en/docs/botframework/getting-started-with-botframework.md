---
title: "Getting started with botframework"
slug: "getting-started-with-botframework"
draft: false
images: []
weight: 1
type: docs
toc: true
---

## Installation or Setup
-----
**C#** 
 1. **Visual Studio 2015** (latest update) - you can download the community version here for free: [www.VisualStudio.com][1]

 2. Important: **update all VS extensions** to their latest versions 
    Tools->Extensions and Updates->Updates

 3. Download the **Bot Application template** from here: [Template Download][2]  Save the zip file to your Visual Studio 2015 templates directory which is traditionally in "%USERPROFILE%\Documents\Visual Studio 2015\Templates\ProjectTemplates\Visual C#\"  Note: you will need to restart visual studio after this step, in order to use the template.

[![New Bot Application Project][3]][3]

 4. Create a **new C# project** using the new Bot Application template

[![Bot Application Solution][4]][4]

 Once your bot is finished being created, you should have a solution similar to this:

[![Bot Application Browser Window][5]][5]

 5. **Run the application** by hitting F5, or by clicking the green Run button in the tool bar.  Since our new bot is actually a WebAPI project, a browser window will be opened to the default.htm page.  The bot is now running, and exposed locally.  Note the url ... it will be needed to setup the Bot Framework Emulator in the next step.

**Node.js**

1. Create a new node.js project by using `npm init`.
2. Install the botbuilder sdk and restify using the following npm commands:


    npm install --save botbuilder
    npm install --save restify

3. To create your bot, create a new file called index.js, and copy the following code to initialize the bot. 


    var restify = require('restify');
    var builder = require('botbuilder');

    // Setup Restify Server
    var server = restify.createServer();
    server.listen(process.env.port || process.env.PORT || 3978, function () {
       console.log('%s listening to %s', server.name, server.url); 
    });

    // Create chat connector for communicating with the Bot Framework Service
    var connector = new builder.ChatConnector({
        appId: process.env.MICROSOFT_APP_ID,
        appPassword: process.env.MICROSOFT_APP_PASSWORD
    });

    var bot = new builder.UniversalBot(connector);
4. You should now be able to run this file using `node index.js`.

This is a basic setup that will be required for all bots created with bot framework. You can treat this as a blank template project to start with. It initializes a restify server for your bot and creates a connector to connect local machines with your server.

**Downloading Emulator for Debugging (Both for node and C#)**

[![Emulator Image][6]][6]

 1. Download and install the **Bot Framework Emulator** [Emulator Download][7] 

 2. Run the emulator, and enter the url from step 5 (C#) into the **Endpoint URL** text box. Then, click "Connect".

[![Connecting to Bot][8]][8]

 3. You should now be able to communicate with your bot using the chat window in the emulator.  You will see the conversation details logged in the bottom right, and you can click on the Post and Get line items to see the json that has been passed back and forth.

[![enter image description here][9]][9]

Congratulations on creating a Bot using the Microsoft Bot Framework!  
 
  [1]: https://www.visualstudio.com/downloads/
  [2]: http://aka.ms/bf-bc-vstemplate
  [3]: https://i.stack.imgur.com/8xBXo.png
  [4]: https://i.stack.imgur.com/OOMjM.png
  [5]: https://i.stack.imgur.com/bWAXo.png
  [6]: https://i.stack.imgur.com/8tsD2.png
  [7]: https://aka.ms/bf-bc-emulator
  [8]: https://i.stack.imgur.com/Jw4aM.png
  [9]: https://i.stack.imgur.com/6uqSp.png

