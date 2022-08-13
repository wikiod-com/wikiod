---
title: "Getting started with unity3d"
slug: "getting-started-with-unity3d"
draft: false
images: []
weight: 1
type: docs
toc: true
---

## Installation or Setup
# Overview
Unity runs on Windows and Mac. There is also a [Linux alpha version](http://forum.unity3d.com/forums/linux-editor.93/) available.

There are 4 different payment plans for Unity:

 1. **Personal** - Free *(see below)*
 2. **Plus** - $35 USD per month per seat *(see below)*
 3. **Pro** - $125 USD per month per seat - After subscribing to the Pro plan for 24 consecutive months, you have the option to stop subscribing and keep the version you have.
 4. **Enterprise** - [Contact Unity for more information][1]

*According to EULA: Companies or incorporated entities that had a turnover in excess of US$100,000 in their last fiscal year must use **Unity Plus** (or a higher license); in excess of US$200,000 they must use **Unity Pro** (or Enterprise).*

# Installing

1. Download the [Unity download assistant][2].

2. Run the assistant and choose the modules you want to download and install, such as Unity editor, MonoDevelop IDE, documentation, and desired platform build modules.

If you have an older version, you can [update to the latest stable version](https://store.unity.com/download?ref=update).

If you want to install Unity without Unity download assistant, you can get the **component installers** from [Unity 5.5.1 release notes][3].

# Installing Multiple Versions of Unity

It is often necessary to install multiple versions of Unity at the same time. To do so:

  - On Windows, change the default install directory to an empty folder that you have previously created such as `Unity 5.3.1f1`.

  - On Mac, the installer will always install to `/Applications/Unity`. Rename this folder for your existing install (e.g. to `/Applications/Unity5.3.1f1`) before running the installer for the different version.

  - You can hold <kbd>Alt</kbd> when launching Unity to force it to let you choose a project to open. Otherwise the last project loaded will attempt to load (if available) and it may prompt you to update a project you do not want updated.


  [1]: https://store.unity.com/contact?type=sales
  [2]: https://unity3d.com/get-unity/download
  [3]: https://unity3d.com/unity/whats-new/unity-5.5.1

## Basic editor and code
## Layout
Unity basic editor will look like below. Basic functionalities of some default windows/tabs are described in the image.

[![enter image description here][1]][1]

### Linux Layout
There is a little difference in menu layout of linux version, like the screenshot below,
[![enter image description here][2]][2]

## Basic Usage
Create an empty `GameObject` by right clicking in the Hierarchy window and select `Create Empty`. Create a new script by right clicking in the Project window and select `Create` > `C# Script`. Rename it as needed. 

When the empty `GameObject` is selected in the Hierarchy window, drag and drop the newly created script in the Inspector window. Now the script is attached to the object in the Hierarchy window. Open the script with the default MonoDevelop IDE or your preference.

## Basic Scripting
Basic code will look like below except the line `Debug.Log("hello world!!");`.
<!-- language: c# -->

    using UnityEngine;
    using System.Collections;
    
    public class BasicCode : MonoBehaviour {
    
        // Use this for initialization
        void Start () {
            Debug.Log("hello world!!");
        }
        
        // Update is called once per frame
        void Update () {
        
        }
    }

Add the line `Debug.Log("hello world!!");` in the `void Start()` method. Save the script and go back to editor. Run it by pressing **Play** at the top of the editor.

Result should be like below in the Console window:

[![enter image description here][3]][3]


  [1]: http://i.stack.imgur.com/HDm82.png
  [2]: http://i.stack.imgur.com/He5k7.png
  [3]: http://i.stack.imgur.com/A6TWZ.png

## Editor Layouts
You can save the layout of your tabs and windows to standardize your work environment.

The layouts menu can be found in the upper right corner of Unity Editor:

[![layouts][1]][1]

Unity ships with 5 default layouts (2 by 3, 4 Split, Default, Tall, Wide) *(marked with 1)*. In the picture above, aside from default layouts, there is also a custom layout at the top.

You can add your own layouts by clicking **"Save Layout..."** button in the menu *(marked with 2)*:

[![save layout][2]][2]

You can also delete any layout by clicking **"Delete Layout..."** button in the menu *(marked with 2)*:

[![delete layout][3]][3]

The **"Revert Factory Settings..."** button removes all custom layouts and restores default layouts *(marked with 2)*.

  [1]: http://i.stack.imgur.com/k4UO6.png
  [2]: http://i.stack.imgur.com/PyJWn.png
  [3]: http://i.stack.imgur.com/BxrhM.png

## Customizing Your Workspace
You can customize your Layout of Views by click-dragging the Tab of any View to one of several locations. Dropping a Tab in the Tab Area of an existing window will add the Tab beside any existing Tabs. Alternatively, dropping a Tab in any Dock Zone will add the View in a new window.
[![enter image description here][1]][1]



Tabs can also be detached from the Main Editor Window and arranged into their own floating Editor Windows. Floating Windows can contain arrangements of Views and Tabs just like the Main Editor Window.
[![enter image description here][2]][2]

When you’ve created an editor layout, you can save the layout and restore it any time. [Refer to this example for editor layouts][3].
[![enter image description here][4]][4]



At any time, you can right-click the tab of any view to view additional options like Maximize or add a new tab to the same window.  
[![enter image description here][5]][5]


  [1]: http://i.stack.imgur.com/EyW7l.gif
  [2]: http://i.stack.imgur.com/oImrn.png
  [3]: https://www.wikiod.com/unity3d/getting-started-with-unity3d#Editor Layouts
  [4]: http://i.stack.imgur.com/IBpG7.png
  [5]: http://i.stack.imgur.com/sbveP.png

