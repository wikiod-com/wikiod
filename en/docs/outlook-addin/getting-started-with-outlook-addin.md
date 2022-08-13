---
title: "Getting started with outlook-addin"
slug: "getting-started-with-outlook-addin"
draft: false
images: []
weight: 1
type: docs
toc: true
---

## Installation or Setup
Detailed instructions on getting outlook-addin set up or installed.

## Create first outlook-addin - Hello World
1. Open Visual Studio
[![enter image description here][1]][1]


2. Select **File --> New --> Project** menu or push **Ctrl + Shift + N** buttons
[![enter image description here][2]][2]


3. Select Outlook VSTO Add-in template
[![enter image description here][3]][3]

4. Add the following code to the project:


    private void ThisAddIn_Startup(object sender, System.EventArgs e)
    {
         MessageBox.Show("Hello world");
    }

5. Run the program

[![enter image description here][4]][4]


  [1]: https://i.stack.imgur.com/OZ8Rj.png
  [2]: https://i.stack.imgur.com/7eXNx.png
  [3]: https://i.stack.imgur.com/K4l4D.png
  [4]: https://i.stack.imgur.com/p6VPy.png

