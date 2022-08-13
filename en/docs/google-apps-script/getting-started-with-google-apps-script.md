---
title: "Getting started with google-apps-script"
slug: "getting-started-with-google-apps-script"
draft: false
images: []
weight: 1
type: docs
toc: true
---

## Types of Scripts
Google App scripts are of three types.

 - Standalone
 - Bound to Google Apps
 - Web Apps

__Standalone script__

Standalone scripts are not bound to any Google apps i.e _Docs, Sheets or Forms etc._ Standalone script can either be created by visiting _script.google.com_ or by connecting Google app script with Google drive. Standalone script can be used to program Google apps independently, can be used as a Web App or can be set up to run automatically from an installable trigger. See the [documentation][1] for standalone script.

__Bound to Google Apps__

Script bound to Google Apps also known as container-bound script; unlike standalone scripts, are bound to Google apps i.e _Google Docs or Google Sheets etc._ Container bound script can be created by selecting `tools> Script editor` from Google App. Some [features][2] like dialogs, prompts, menus and sidebar are only provided by container-bound scripts. Furthermore, container-bound script is used to create [Google Add-ons][3]. See the [documentation][4] for container-bound scripts.

__Web Apps__

Google App Script can be used as web app as they can be accessed by browser. Web App can provide user interface on the browser and can make use of google apps i.e _docs, sheets etc._ Both standalone scripts and scripts bound to Google Apps can be turned into web apps. For any script to work as a web app, script has to meet two requirements:

 - include a `doGet()` or `doPost()` function.
 - The function returns an HTML service HtmlOutput object or a Content service TextOutput object.

Inshort, `doGet()` and `doPost()` functions works like http get and post request handlers respectively.

For more details on Web Apps, see the official [documentation][5].


  [1]: https://developers.google.com/apps-script/guides/standalone
  [2]: https://developers.google.com/apps-script/guides/dialogs
  [3]: https://developers.google.com/apps-script/add-ons/
  [4]: https://developers.google.com/apps-script/guides/bound
  [5]: https://developers.google.com/apps-script/guides/web

## Installation or Setup
Google Apps Script does not require setup or installation. The only requirement is a Google Account. A Gmail account works as well as a Google Apps for Work/Education/Government account. You can create a new Google account by going to [accounts.google.com][1]
  
Start your first script by going to [script.google.com][2]. You can also access Google Apps Script under the `tools -> Script editor...` of many Google Apps i.e _Docs, Sheets, Forms etc_. Google Apps Script can also be added directly to your Google Drive with the `Connect more apps..` feature.  

Official documentation can be found at [developers.google.com/apps-script/][3]. 

For app-scripts to run they must contain a code.gs file. The code.gs file must contain a function named  doGet (standalone scripts) or an onOpen function (addon scripts). The quick starts in the documentation contain examples. 

If an api is turned on in the app-script it must also be turned on in the developers-console. However, the developers console contains api's that can be turned on but do not appear in the app-script interface. For example, Marketplace SDK needs to be turned on in the developers console before the app can be published to the Google play store or to a G suite domain wide deployment. 

For Google apps for education/work/government there are settings in the domain admin console that can be adjusted to allow or disallow app-scripts to run. 

  [1]: https://accounts.google.com  
  [2]: https://script.google.com  
  [3]: https://developers.google.com/apps-script/  


## Running/Debugging your script
Try to run your code from the tool bar as shown below : 

[![enter image description here][1]][1]

In your code, if you have more than one function then, before running it you should mention the function you want to run with. For example :

[![enter image description here][2]][2]


  [1]: http://i.stack.imgur.com/FENhy.png
  [2]: http://i.stack.imgur.com/9kOsX.png

Alternatively, you can press **ctrl + r** from your keyboard to run the code. It will save the code first, if not saved, and then run it. But, for that to work, you must have selected the function, as seen in the image above.

Also, if your script is called by some external activities, still you will be able to see logs by clicking view->logs if you are logging anything after the code is executed.

## Hello World
We are going to say Hello as a message box.

    function helloWorld() 
    {
      Browser.msgBox("Hello World");
    }

To execute the script, either click ▶ or select the menu item **Run** -> **helloWorld**

## A deeper look at Google Apps Script
Google Apps Script is a JavaScript based platform-as-a-service primarily used to automate and extend Google Apps. Apps Script runs exclusively on Google's infrastructure requiring no server provisioning or configuration. An online IDE serves as the interface to the entire platform connecting all the services that are available to Apps Script. User authentication is baked into the platform via OAuth2 and requires no code or setup by the script author. 

Apps Script runs server-side, but can have user interfaces built with Html, CSS, JavaScript, or any other browser supported technologies. Unlike Nodejs, which is event driven, App Scripts runs in a threaded model. All calls to a script generate a unique instance of that script which runs in isolation of all other instances. When an instance of a script finishes execution it is destroyed.

Functions in Apps Script are blocking so callback and async programming patterns are not needed. Locking is used to prevent critical sections of code, such as file IO,  from being executed simultaneously by different instances. 

In practice writing Apps Scripts are simple. Below is a simple script that creates a new spreadsheet from a template spreadsheet.

    // Create a new spreadsheet from a template
    function createSpreadsheet(){
       var templateFileId = '1Azcz9GwCeHjGl9TXf4aUh6g20Eqmgd1UMSdNVjzIZPk';
       var sheetName = 'Account Log for:' + new Date();
       SpreadsheetApp.openById(templateFileId).copy(sheetName);   
    } 

