---
title: "Angular2 and .Net Core"
slug: "angular2-and-net-core"
draft: false
images: []
weight: 9895
type: docs
toc: true
---

## Quick tutorial for an Angular 2 Hello World! App with .Net Core in Visual Studio 2015
**Steps:**
1. Create Empty .Net Core Web App:
[![enter image description here][1]][1]
2. Go to wwwroot, and create a normal html page called Index.html:
[![enter image description here][2]][2]
3. Configure Startup.cs to accept static files (this will require to add "Microsoft.AspNetCore.StaticFiles": "1.0.0" library in the “project.json” file):
[![enter image description here][3]][3]
[![enter image description here][4]][4]
4. Add NPN File:
    - Right click the WebUi project and add NPN Configuration File (package.json):
    [![enter image description here][5]][5]
    - Verify the last versions of the packages:
    [![enter image description here][6]][6]

        Note: If visual studio does not detect the versions of the packages (Check all packages, because some of them does show the version, and some others don't), it might be because the Node version coming in visual studio is not working correctly, so it will probably require to install node js externally and then link that installation with visual studio.

        i. Download and install node js: https://nodejs.org/es/download/

        ii. Link the installation with visual studio: https://ryanhayes.net/synchronize-node-js-install-version-with-visual-studio-2015/:
        [![enter image description here][7]][7]
        iii. (Optional) after saving the package.json it will install the dependencies in the project, if not, run "npm install" using a command prompt from the same location as the package.json file.

        Note: Recommended to install "Open Command Line", an extension that can be added to Visual Studio:
        [![enter image description here][8]][8]
5. Add typescript:
    - Create a TsScript folder inside the WebUi project, just for organization (The TypeScripts won't go to the browser, they will be transpiled into a normal JS file, and this JS file will be the one going to the wwwroot foder using gulp, this will be explained later):
    [![enter image description here][9]][9]
    - Inside that folder add "TypeScript JSON Configuration File" (tsconfig.json):
    [![enter image description here][10]][10]
    And add the next code:
    
        [![enter image description here][11]][11]
    - In the WebUi Project’s root, add a new file called typings.json:
    [![enter image description here][12]][12]
    And add the next code:
    [![enter image description here][13]][13]
    - In the Web Project root open a command line and execute "typings install", this will create a typings folder (This requires “Open Command Line” explained as an optional step in the Note inside Step 4, numeral iii).
    [![enter image description here][14]][14]
    [![enter image description here][15]][15]
    [![enter image description here][16]][16]
6. Add gulp to move files:
    - Add "Gulp Configuration File" (gulpfile.js) at the root of the web project:
    [![enter image description here][17]][17]
    - Add Code:
    [![enter image description here][18]][18]
7. Add Angular 2 bootstrapping files inside the “tsScripts” folder:
    [![enter image description here][19]][19]
    
    app.component.ts
    [![enter image description here][20]][20]

    app.module.ts
    [![enter image description here][21]][21]

    main.ts
[![enter image description here][22]][22]

8. Inside wwwroot, create the next file structure:
    [![enter image description here][23]][23]
9. Inside the scripts folder (but outside app), add the systemjs.config.js:
    [![enter image description here][24]][24]
    And add the next code:
    [![enter image description here][25]][25]

10. Execute Gulp Task to generate the scripts in wwwroot.
    - Right click gulpfile.js
    - Task Runner Explorer
    [![enter image description here][26]][26]
        i. If the tasks are not loaded ("Fail to load. See Output window") Go to output window and take a look at the errors, most of the time are syntax errors in the gulp file.
    - Right Click "default" task and "Run" (It will take a while, and the confirmation messages are not very precise, it shows it finished but the process is still running, keep that in mind):
    [![enter image description here][27]][27]
11. Modify Index.html like:
    [![enter image description here][28]][28]

12. Now run and enjoy.

    Notes:
    - In case there are compilation errors with typescript, for example "TypeScript Virtual Project", it is an indicator that the TypeScript version for Visual Studio is not updated according to the version we selected in the “package.json”, if this happens please install: https://www.microsoft.com/en-us/download/details.aspx?id=48593

**References:**
- Deborah Kurata's "Angular 2: Getting Started" course in Pluralsight:

    https://www.pluralsight.com/courses/angular-2-getting-started-update


- Angular 2 Official Documentation: 

    https://angular.io/


- Articles by Mithun Pattankar:

    http://www.mithunvp.com/angular-2-in-asp-net-5-typescript-visual-studio-2015/

    http://www.mithunvp.com/using-angular-2-asp-net-mvc-5-visual-studio/

  [1]: https://i.stack.imgur.com/68jtc.png
  [2]: https://i.stack.imgur.com/NSUEW.png
  [3]: https://i.stack.imgur.com/4AycC.png
  [4]: https://i.stack.imgur.com/xppBs.png
  [5]: https://i.stack.imgur.com/8GxRU.png
  [6]: https://i.stack.imgur.com/LZ6IK.png
  [7]: https://i.stack.imgur.com/ZgeTz.png
  [8]: https://i.stack.imgur.com/jWRu8.png
  [9]: https://i.stack.imgur.com/HYvED.png
  [10]: https://i.stack.imgur.com/OgRWE.png
  [11]: https://i.stack.imgur.com/DHDCh.png
  [12]: https://i.stack.imgur.com/n5Kmj.png
  [13]: https://i.stack.imgur.com/x4h7U.png
  [14]: https://i.stack.imgur.com/X5fmM.png
  [15]: https://i.stack.imgur.com/OLXiH.png
  [16]: https://i.stack.imgur.com/qerUw.png
  [17]: https://i.stack.imgur.com/zCBu2.png
  [18]: https://i.stack.imgur.com/W4b3z.png
  [19]: https://i.stack.imgur.com/IByD3.png
  [20]: https://i.stack.imgur.com/GOiju.png
  [21]: https://i.stack.imgur.com/QBfTG.png
  [22]: https://i.stack.imgur.com/Aq2Tz.png
  [23]: https://i.stack.imgur.com/7rWYv.png
  [24]: https://i.stack.imgur.com/lVkVh.png
  [25]: https://i.stack.imgur.com/8TX8V.png
  [26]: https://i.stack.imgur.com/mE8Rc.png
  [27]: https://i.stack.imgur.com/fcBes.png
  [28]: https://i.stack.imgur.com/FLOOY.png

## Expected errors when generating Angular 2 components in .NET Core project (version 0.8.3)
When generating new Angular 2 components in a .NET Core project, you may run into the following errors (as of version 0.8.3):

    Error locating module for declaration
            SilentError: No module files found

OR

    No app module found. Please add your new Class to your component. 
            Identical ClientApp/app/app.module.ts

[SOLUTION]

 1. Rename app.module.client.ts to app.client.module.ts
 2. Open app.client.module.ts: prepend the declaration with 3 dots “...” and wrap the declaration in brackets. 

    *For example: `[...sharedConfig.declarations, <MyComponent>]`*
 3. Open  boot-client.ts: update your import to use the new
    app.client.module reference. 

    *For example: `import { AppModule } from './app/app.client.module';`*

 4. Now try to generate the new component: `ng g component my-component`

[EXPLANATION]

Angular CLI looks for a file named app.module.ts in your project, and tries to find a references for the declarations property to import the component. This should be an array (as the sharedConfig.declarations is), but the changes do not get applied

[SOURCES]

 - https://github.com/angular/angular-cli/issues/2962
 - https://www.udemy.com/aspnet-core-angular/learn/v4/t/lecture/6848548 (section 3.33 lecture contributor Bryan Garzon)


