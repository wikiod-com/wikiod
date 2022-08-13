---
title: "Getting started with Xamarin.iOS"
slug: "getting-started-with-xamarinios"
draft: false
images: []
weight: 1
type: docs
toc: true
---

## Get Started in Visual Studio
 1. Browse to **File > New > Project** to bring you up the New Project dialog.
 2. Navigate to Visual C# > iOS > iPhone and select Single View App:
[![enter image description here][1]][1]
 3. Give your app a **Name** and press **OK** to create your project.
 4. Select the Mac Agent icon from the toolbar, as illustrated below:
[![enter image description here][2]][2]
 5. Select the Mac that will build your application from the list (make sure you Mac is set up to receive the connection!), and press **Connect**:
[![enter image description here][3]][3]
 6. To run your application, select the **Debug | iPhoneSimulator** configuration, and press the Play button:[![enter image description here][4]][4]
 7. This will launch the iOS Simulator on the Mac, and will display your empty application: 
[![enter image description here][5]][5]


  [1]: http://i.stack.imgur.com/FLWsn.png
  [2]: http://i.stack.imgur.com/nH2rm.png
  [3]: http://i.stack.imgur.com/EeLxo.png
  [4]: http://i.stack.imgur.com/DPD9M.png
  [5]: http://i.stack.imgur.com/j6cAT.png

## Get Started in Xamarin Studio
 1. Browse to **File > New > Solution** to bring you up the new project
    dialog
 2. Select **Single View App** and press **Next**
 3. Configure your app by setting your app name and organization ID, and press **Next**: [![enter image description here][1]][1]


 4. Set your Project name and Solution name, or leave as the default name. Click **Create** to create your project.
 5. To run your application, select the Debug | iPhone 6s iOS 9.x configuration, and press the **Play** button:
[![enter image description here][2]][2]
 6. This will launch the iOS Simulator, and will display your empty application:
[![enter image description here][3]][3]


  [1]: http://i.stack.imgur.com/fpEWw.png
  [2]: http://i.stack.imgur.com/vACh6.png
  [3]: http://i.stack.imgur.com/g0XIw.png

## Hello, World
 1. Double click on the **Main.Storyboard** file.
 2. Set **View As** to iPhone 6:[![enter image description here][1]][1] 
 3. Drag a label and a button from the Toolbox to the design surface so that it looks like the image below:[![enter image description here][2]][2]
 4. In the Properties pad, give the label and button the following properties:

nothing | Name |   Title    |
------ | ------ | ----- |
Label   | lblClicks   |   [blank]    |
Button   | clickMe   |    Click Me!   |

 5. Add the following code to the **ViewDidLoad** method inside the **ViewController** class:

<!-- language: c# -->   

    clickMe.TouchUpInside += (sender, e) =>
    {
        totalClicks++;
        if (totalClicks == 1)
        {
            lblClicks.Text = totalClicks + " Click";
        }
        
       else {
           lblClicks.Text = totalClicks + " Clicks";
       }
    };

 6. Run the application

  [1]: http://i.stack.imgur.com/70TIT.png
  [2]: http://i.stack.imgur.com/tXE46.png

