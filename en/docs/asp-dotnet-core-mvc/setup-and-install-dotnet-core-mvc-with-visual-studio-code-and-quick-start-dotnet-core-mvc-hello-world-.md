---
title: "Setup and install .Net Core MVC with Visual studio code and quick start .net core mvc hello world."
slug: "setup-and-install-net-core-mvc-with-visual-studio-code-and-quick-start-net-core-mvc-hello-world"
draft: false
images: []
weight: 9928
type: docs
toc: true
---

This article give idea's about setup and installing Asp.Net core with visual studio code. Also create basic MVC template and debugging.
 
Steps involved below...
 
 Step 1 - installing Visual studio code.

 Step 2 - Configuring .Net core and C#.

 Step 3 - Create Basic MVC Template.

 Step 4 - Execute and Debug the application.


This article is about to setup from scratch with visual studio code open source and create and debug basic .net core mvc applications.

* File location used above is change as per users, No constraint.
* Need internet for downloading setups.


## Step 1 - Visual studio code installation
<br>

*  Download visual studio code from here [Visual studio code][1]. Select your target installer[mac|windows|linux].


[![enter image description here][2]][2]




<br>

* Go to downloaded file in your local.

[![enter image description here][3]][3]


* Below steps in volved for installing

[![enter image description here][4]][4]


[![enter image description here][5]][5]


[![enter image description here][6]][6]


[![enter image description here][7]][7]


[![enter image description here][8]][8]


[![enter image description here][9]][9]


[![enter image description here][10]][10]


[![enter image description here][11]][11]


  [1]: http://code.visualstudio.com/
  [2]: https://i.stack.imgur.com/U8PGy.jpg
  [3]: https://i.stack.imgur.com/S1bqo.jpg
  [4]: https://i.stack.imgur.com/bzNCA.jpg
  [5]: https://i.stack.imgur.com/82V2d.jpg
  [6]: https://i.stack.imgur.com/P6yGh.jpg
  [7]: https://i.stack.imgur.com/LDSOD.jpg
  [8]: https://i.stack.imgur.com/eorRI.jpg
  [9]: https://i.stack.imgur.com/auzX4.jpg
  [10]: https://i.stack.imgur.com/eGJPn.jpg
  [11]: https://i.stack.imgur.com/MWi6V.jpg

Installation finished successfully.

## Step 2 - Configuring .Net core and C#.
After installing Visual studio code configure .net core and C#.

* Configure C# based on market-place.
  Reference Url: [C# market place .net core][1]


[![enter image description here][2]][2]


1. Launch Visual studio code.
2. Press [**ctrl + P**]
3. paste "**ext install csharp**" this and hit.

Once done above steps , C# extension available in VS Code.

[![enter image description here][3]][3]


* Now configure .net core.

Download .net core sdk from [here][4]. Choose Windows=>CommandLine.


[![enter image description here][5]][5]


Install the sdk like below.

[![enter image description here][6]][6]


[![enter image description here][7]][7]


[![enter image description here][8]][8]


  [1]: https://marketplace.visualstudio.com/items?itemName=ms-vscode.csharp
  [2]: https://i.stack.imgur.com/NX4xI.jpg
  [3]: https://i.stack.imgur.com/enwuy.jpg
  [4]: https://www.microsoft.com/net/core#windowscmd
  [5]: https://i.stack.imgur.com/HYcwt.jpg
  [6]: https://i.stack.imgur.com/Gkzk5.jpg
  [7]: https://i.stack.imgur.com/332tI.jpg
  [8]: https://i.stack.imgur.com/PUfdd.jpg

.Net core sdk installation done successfully.

## Step 3 - Create Basic MVC Template.
<br>

* Create your new project folder and open in windows-command prompt with the location of project folder.


[![enter image description here][1]][1]


* Type "**dotnet new -t web**" and hit. This is for creating new mvc template.

[![enter image description here][2]][2]


* Once complete. GO to the project location and see basic mvc project has been created.

[![enter image description here][3]][3]


* Then type "**dotnet restore**" and hit. This is for to restoring all packages from project.json file.

[![enter image description here][4]][4]


[![enter image description here][5]][5]


* Now launch VScode and open the project folder.

[![enter image description here][6]][6]

[![enter image description here][7]][7]


  [1]: https://i.stack.imgur.com/l3acC.jpg
  [2]: https://i.stack.imgur.com/PMX9p.jpg
  [3]: https://i.stack.imgur.com/aaaP0.jpg
  [4]: https://i.stack.imgur.com/vcNnU.jpg
  [5]: https://i.stack.imgur.com/XP2J0.jpg
  [6]: https://i.stack.imgur.com/A0XBn.jpg
  [7]: https://i.stack.imgur.com/S6HRy.jpg

Now you can finally see the mvc project in VS code.

All the basic mvc structure files you can see.[Model-View-Controller]

## Step 4 - Execute and Debug the application.
Open the project folder in VScode.

Sample here i am setting break point in home controller. 

[![enter image description here][1]][1]


Now click the debug option.

[![enter image description here][2]][2]

add debug configuration like below. Make sure .Net core Launch(web) is selected.

[![enter image description here][3]][3]


[![enter image description here][4]][4]


You can see break point will hit , once you start debugging by press run icon.

Then give continue.
Web page will shown in browser like below.
[![enter image description here][5]][5]


Web page is seems broken.

Press "**F12**" or open developer tool.

You can see some errors in console.


[![enter image description here][6]][6]


Few bootstrap and jquery files were not loaded.

[Find a script and css files by **Ctrl+shift+f** in VS code and enter missed file name and search.]

Fix this by adding scripts with cdn or exact file location in layout file.

[![enter image description here][7]][7]


Now refresh the page and watch.

[![enter image description here][8]][8]


  [1]: https://i.stack.imgur.com/lgjvK.jpg
  [2]: https://i.stack.imgur.com/oxUwu.jpg
  [3]: https://i.stack.imgur.com/hROBm.jpg
  [4]: https://i.stack.imgur.com/d8gx5.jpg
  [5]: https://i.stack.imgur.com/kXnzw.jpg
  [6]: https://i.stack.imgur.com/rA1kI.jpg
  [7]: https://i.stack.imgur.com/0hfCA.jpg
  [8]: https://i.stack.imgur.com/FHwSe.jpg

Now site seems fine and no more console error finally.

Happy coding.

