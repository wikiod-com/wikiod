---
title: "electron-winstaller"
slug: "electron-winstaller"
draft: false
images: []
weight: 9955
type: docs
toc: true
---

NPM module that builds Windows installers for Electron apps. It will help to create single EXE for Electron windows application

## Syntax
 

 - **Install Globally**
 - npm install -g electron-winstaller
 - **Install Locally**
 - npm install --save-dev electron-winstaller

## Parameters
| Config Name | Description|
| ------ | ------ |
| appDirectory| The authors value for the nuget package metadata. Defaults to the author field from your app's package.json file when unspecified.|
| owners| The owners value for the nuget package metadata. Defaults to the authors field when unspecified.|
| exe| The name of your app's main .exe file. This uses the name field in your app's package.json file with an added .exe extension when unspecified.|
| description| The description value for the nuget package metadata. Defaults to the description field from your app's package.json file when unspecified.|
| version| The version value for the nuget package metadata. Defaults to the version field from your app's package.json file when unspecified.|
| title| The title value for the nuget package metadata. Defaults to the productName field and then the name field from your app's package.json file when unspecified.|
| name| Windows Application Model ID (appId). Defaults to the name field in your app's package.json file.|
| certificateFile| The path to an Authenticode Code Signing Certificate|
| certificatePassword   | The password to decrypt the certificate given in certificateFile|
| signWithParams| Params to pass to signtool. Overrides certificateFile and certificatePassword.|
| iconUrl| A URL to an ICO file to use as the application icon (displayed in Control Panel > Programs and Features). Defaults to the Atom icon.|
| setupIcon| The ICO file to use as the icon for the generated Setup.exe|
| setupExe| The name to use for the generated Setup.exe file|
| setupMsi| The name to use for the generated Setup.msi file|
| noMsi| Should Squirrel.Windows create an MSI installer?|
| remoteReleases| A URL to your existing updates. If given, these will be downloaded to create delta updates|
| remoteToken| Authentication token for remote updates|


## Build JS
   Here Is basic build file to build executable from electron windows app. 

    var electronInstaller = require('electron-winstaller');
    var resultPromise = electronInstaller.createWindowsInstaller({
        appDirectory: 'Your_electron_application_path',
        authors: 'Author Name',
        description: "Description"
    });
    
    resultPromise.then(() => console.log("Build Success!"), (e) => console.log(`No dice: ${e.message}`));

