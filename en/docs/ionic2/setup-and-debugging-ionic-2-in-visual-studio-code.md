---
title: "Setup and Debugging  Ionic 2 in Visual Studio Code"
slug: "setup-and-debugging--ionic-2-in-visual-studio-code"
draft: false
images: []
weight: 9985
type: docs
toc: true
---

Visual Studio is a open Source IDE which provides intellisense and editing facility for code .This IDE supports many languages like(Ionic ,C, C# ,AngularJs, TypeScript ,Android and so on) . These languages are able to execute there code by adding its Extensions in VSCode. By using VSCode we able to run and debug the code of different different languages. 


 

## Create and Add your Ionic Project in VSCode
VsCode is unable to create the ionic project because it is a code editor.So you can create your ionic project by **CLI** or **cmd**. create your project by below command 

    $ ionic start appName blank
Above command use to create blank template ionic application. Ionic2 provide three type of templates **blank, tabs and sidemenu**. So. you may replace blank template by any other two templates as per your need.

Now, your Ionic project has been created. So, you are able to add your project in VSCode to edit. To add your project follow below points.

1. Go to **File** menu in VScode.
2. Click the **Open Folder** inside File menu.
3. Find and open your project folder.



> You may directly open the folder by using shortcut key **ctrl+O** or
> **ctrl+k**

` 


## Installation of VSCode

Firstly you need to download and install the VSCode. This VSCode latest version is available for download in its  [Official website][1] . After download the VSCode you should install and open it.

  

    

     Introduction of Extensions in VSCode
VSCode is a open editor so it provide editor for all languages but to execute a code you need to add the Extension for that particular language.
For running and editing your ionic code you should add **ionic2-vscode** Extension in yourVSCode. In the left side of VSCode Editor there are 5 icons in which the lowest one icon is use for Extension. The Extensions you may get by using **shortcut key 
(ctrl+shift+X)**.  

    Add Extension for Ionic2 in VsCode
By pressing **ctrl+shift+X** you shown the part of extension where on top **three dots** are shown **...** these dots are known as more icon.On click of it a dialog is open and shows the numbers of options to choose .you may choose the option  as per your need but for getting all extension you should select **Shown Recommended Extension** .iN the list of all eXtension you may install your Extension `(ionic2-vscode),npm`

  [1]: https://code.visualstudio.com/docs/setup/setup-overview

## Run and Debug your Ionic Project
***>     Run and Debug in Chrome***

**To run** the ionic project use below  command in **terminal or cmd or CLI**

  

     $ ionic serve

**For debugging** the ionic project ,Firstly you should add extension **(Debugger for chrome)**  and then configure launch.json file like this.

   

     {
            "version": "0.2.0",
            "configurations": [
               
                {
              "name": "Launch in Chrome",
              "type": "chrome",
              "request": "launch",
              "url": "http://localhost:8100",
              "sourceMaps": true,
              "webRoot": "${workspaceRoot}/src"
            }
    ]
    }

 

 *>***Run and Debug in Android****

**For Run** ionic project in Android you should add android platform by below command in terminal or cmd or CLI:

    $ ionic cordova platform add android 
        
Build Android by this command

    $ ionic cordova build android

Run command for android platform 

    $ ionic cordova run android

Now, Your application run on real Android device.

**For debug** into Android device you need to add **Cordova or Android Extension** in VSCode. and configure launch.json file like this.



    {
        "version": "0.2.0",
        "configurations": [
        {
                    "name": "Run Android on device",
                    "type": "cordova",
                    "request": "launch",
                    "platform": "android",
                    "target": "device",
                    "port": 9222,
                    "sourceMaps": true,
                    "cwd": "${workspaceRoot}",
                    "ionicLiveReload": false
                },
                {
                    "name": "Run iOS on device",
                    "type": "cordova",
                    "request": "launch",
                    "platform": "ios",
                    "target": "device",
                    "port": 9220,
                    "sourceMaps": true,
                    "cwd": "${workspaceRoot}",
                    "ionicLiveReload": false
                },
                {
                    "name": "Attach to running android on device",
                    "type": "cordova",
                    "request": "attach",
                    "platform": "android",
                    "target": "device",
                    "port": 9222,
                    "sourceMaps": true,
                    "cwd": "${workspaceRoot}"
                },
                {
                    "name": "Attach to running iOS on device",
                    "type": "cordova",
                    "request": "attach",
                    "platform": "ios",
                    "target": "device",
                    "port": 9220,
                    "sourceMaps": true,
                    "cwd": "${workspaceRoot}"
                },
                {
                    "name": "Run Android on emulator",
                    "type": "cordova",
                    "request": "launch",
                    "platform": "android",
                    "target": "emulator",
                    "port": 9222,
                    "sourceMaps": true,
                    "cwd": "${workspaceRoot}",
                    "ionicLiveReload": false
                },
                {
                    "name": "Run iOS on simulator",
                    "type": "cordova",
                    "request": "launch",
                    "platform": "ios",
                    "target": "emulator",
                    "port": 9220,
                    "sourceMaps": true,
                    "cwd": "${workspaceRoot}",
                    "ionicLiveReload": false
                },
                {
                    "name": "Attach to running android on emulator",
                    "type": "cordova",
                    "request": "attach",
                    "platform": "android",
                    "target": "emulator",
                    "port": 9222,
                    "sourceMaps": true,
                    "cwd": "${workspaceRoot}"
                },
                {
                    "name": "Attach to running iOS on simulator",
                    "type": "cordova",
                    "request": "attach",
                    "platform": "ios",
                    "target": "emulator",
                    "port": 9220,
                    "sourceMaps": true,
                    "cwd": "${workspaceRoot}"
                },
                {
                    "name": "Serve to the browser (ionic serve)",
                    "type": "cordova",
                    "request": "launch",
                    "platform": "serve",
                    "cwd": "${workspaceRoot}",
                    "devServerAddress": "localhost",
                    "sourceMaps": true,
                    "ionicLiveReload": true
                },
                {
                    "name": "Simulate Android in browser",
                    "type": "cordova",
                    "request": "launch",
                    "platform": "android",
                    "target": "chrome",
                    "simulatePort": 8000,
                    "livereload": true,
                    "sourceMaps": true,
                    "cwd": "${workspaceRoot}"
                },
                {
                    "name": "Simulate iOS in browser",
                    "type": "cordova",
                    "request": "launch",
                    "platform": "ios",
                    "target": "chrome",
                    "simulatePort": 8000,
                    "livereload": true,
                    "sourceMaps": true,
                    "cwd": "${workspaceRoot}"
                }
    ]
    }



> After configuration you follow the following steps or short keys for
> debugging:

 

 1. Go to **debug** menu.  
2. Click **start debugging**.

      or 
  

    Short keys

 - Debugging - F5 
 
 - StepOver - F10

 - Step Into and Step Out - F11

 - Stop Debugging - Shift+F5

 - Restart Debugging -ctrl+shift_F5





 


