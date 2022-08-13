---
title: "Getting started with ActionScript 3"
slug: "getting-started-with-actionscript-3"
draft: false
images: []
weight: 1
type: docs
toc: true
---

## Installation Overview
ActionScript 3 can be used by installing the [Adobe AIR SDK][1] or [Apache Flex SDK][2] or as part Adobe's [Animate CC][3] product *(formerly known as Flash Professional)*.

Adobe Animate CC is a professional software solution that can be used to create AS3 projects using visual tools - once installed, no further steps are necessary to begin creating AS3 projects.

The AIR SDK and Flex SDK can be used with command line tools or with various third party IDEs.

In addition to Adobe Animate CC, there are four other popular IDEs capable of working with AS3. These IDEs have their own instructions on how to get started.

 - [Flash Builder][4]  (By Adobe - based on Eclipse)
 - [IntelliJ IDEA][5] (By Jetbrains)
 - [FlashDevelop][6]
 - [FDT][7] (Eclipse Plugin)


  [1]: http://www.adobe.com/devnet/air/air-sdk-download.html
  [2]: http://flex.apache.org/installer.html
  [3]: http://www.adobe.com/products/animate/features.html
  [4]: http://www.adobe.com/ca/products/flash-builder.html
  [5]: https://www.jetbrains.com/idea/
  [6]: http://www.flashdevelop.org/
  [7]: http://fdt.powerflasher.com/

## Hello World
An example document class that prints “Hello, World” to the debug console when instantiated. 

    import flash.display.Sprite;
    
    public class Main extends Sprite {

        public function Main() {
            super();
    
            trace("Hello, World");
        }

    }


## Flash Develop Installation
[FlashDevelop][1] is a multi-platform open source IDE created in 2005 for Flash developers. With no cost, it's a very popular way to get started developing with AS3.

To Install FlashDevelop:

1. [Download The Installation File][2] and run the installer
2. Once installation is complete, run FlashDevelop. On the first launch, the `App Man` window should appear asking you to pick what SDK's and tools to install.
  
[![enter image description here][3]][3]
*If the AppMan doesn't open automatically, or you want to add something later, open it by choosing 'Install Software' on the 'Tools' menu.*

Check the **AIR SDK+ ACS 2.0** item (in the 'Compiler' section) and the **Flash Player (SA)** item in the 'Runtimes' section (plus anything else you'd like to install).  Click the install button.

3. Once the SDK is installed, let's test is by creating a hello world project. Start by creating a new project (from the *Project* menu)

4. Choose **AIR AS3 Projector** from the list, and give it a name/location.

5. In the project manager panel (choose 'Project Manager' from the view menu if not already visible), expand the **src** folder, and open the `Main.as` file.

6. In the `Main.as` file, you can now create a first example program like https://www.wikiod.com/actionscript-3

7. Run your project by clicking the play icon, or pressing `F5`, or `Ctrl+Enter`. The project will compile and when finished a blank window should appear (this is your application). In the FlashDevelop output window, you should see the words: **Hello World**.

You are now ready to start developing AS3 applications with FlashDevelop!  


  [1]: http://www.flashdevelop.org/
  [2]: http://www.flashdevelop.org/community/viewforum.php?f=11
  [3]: http://i.stack.imgur.com/VySuK.jpg

## Apache Flex Installation
*from http://flex.apache.org/doc-getstarted.html*

1. [Download the SDK installer][2]
2. Run the SDK installer. The first question you will be asked is the installation directory. 
   - on a Mac, use `/Applications/Adobe Flash Builder 4.7/sdks/4.14.0/` 
   - on a PC, use `C:\Program Files(x86)\Adobe Flash Builder 4.7\sdks\4.14.0`

   You will need to create the 4.14.0 folders. Press Next.
Accept SDK Licenses and Install.

IDE Specific Instructions for Apache Flex setup:

 - [Flash Builder][8]
 - [IntelliJ IDEA][9]
 - [FlashDevelop][10]
 - [FDT][11]


  [2]: http://flex.apache.org/installer.html
  [8]: http://flex.apache.org/doc-getstarted.html#setupFlashBuilder
  [9]: http://flex.apache.org/doc-getstarted.html#setupIDEA
  [10]: http://flex.apache.org/doc-getstarted.html#setupFD
  [11]: http://flex.apache.org/doc-getstarted.html#setupFDT

## Building Flex or Flash projects at the command line using mxmlc
The Flex compiler (`mxmlc`) is one of the most important parts of the Flex SDK. You can edit AS3 code in any text editor you like. Create a main class file that extends from `DisplayObject`.

You can trigger builds at the command line as follows:

<!-- language: lang-none -->
    mxmlc -source-path="." -default-size [width in pixels] [height in pixels] -default-frame-rate [fps] -o "outputPath.swf" "mainClass.as"

If you need to compile a Flash project (as opposed to Flex) you can add a reference to the Flash library as follows (you'll need to have the Adobe Animate IDE installed):

<!-- language: lang-none -->
    mxmlc -source-path="." -library-path+="/Applications/Adobe Animate CC 2015.2/Adobe Animate CC 2015.2.app/Contents/Common/Configuration/ActionScript 3.0/libs" -static-link-runtime-shared-libraries=true -default-size [width in pixels] [height in pixels] -default-frame-rate [fps] -o "outputPath.swf" "mainClass.as"

Or on Windows:

<!-- language: lang-none -->
    mxmlc -source-path="." -library-path+="C:\Program Files\Adobe\Adobe Animate CC 2015.2\Common\Configuration\ActionScript 3.0\libs" -static-link-runtime-shared-libraries=true -default-size [width in pixels] [height in pixels] -default-frame-rate [fps] -o "outputPath.swf" "mainClass.as"

## A displayed "Hello World" example
    package {
        import flash.text.TextField;
        import flash.display.Sprite;
    
        public class TextHello extends Sprite {
            public function TextHello() {
                var tf:TextField = new TextField();
                tf.text = "Hello World!"
                tf.x = 50;
                tf.y = 40;
                addChild(tf);
            }
        }
    }

This class uses the `TextField` class to display the text.

