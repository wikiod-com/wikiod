---
title: "Using Processing with Alternative Code Editors like Sublime and Atom"
slug: "using-processing-with-alternative-code-editors-like-sublime-and-atom"
draft: false
images: []
weight: 9955
type: docs
toc: true
---

## Using Processing with Sublime Text
To add a language to Sublime Text, you use [Package Control][1]. You can do this by pasting the correct Python code (available on the Package Control site linked above), or by downloading the .sublime-package file (also available for download on the site). Once you set up Package Control, restart Sublime Text.

To install the Processing package from Package Control, follow the following steps:
* Execute Tools | Install processing-java in order to install the processing-java tool. This tool is needed to build Processing sketch in command line and editors such as Sublime Text and Atom.
* Open the Command Palette (Ctrl+Shift+P or Cmd+Shift+P)
* Search for 'Package Control: Install Package'
* Search for 'Processing' and install the package
* Restart Sublime Text

After following these steps, you should be able to select Processing as a language. This will facilitate coding in Processing in Sublime Text.

  [1]: https://packagecontrol.io/installation

## Using Processing with Atom editor
There are several packages which can run Processing sketches in the Atom editor. These instructions use the [Script](https://atom.io/packages/script) package. There are also available packages for [syntax highlighting](https://atom.io/packages/processing-language) and [autocomplete](https://atom.io/packages/processing-autocomplete), which are required for Script to identify Processing filetypes.

Install the Script plugin. Either by running `apm install script` from the command line or search for the package 'script' by *rgbkrk* in the `Install` tab in Atom settings (shortcut `command + ,` or `ctrl + ,`).

Once Script is installed, you will need to install `processing-java`. This tool comes with the main Processing software and is needed to build Processing sketches on the command line and in editors:

+ *MacOS:* Run `Tools > Install "processing-java"`.
+ *Linux:* Add the Processing directory to your `PATH` environment variable (replace `/path/to/processing` with the path where Processing is installed):

      sudo ln -s /path/to/processing/processing-java /usr/local/bin/

+ *Windows:* Add the Processing directory to your `PATH` environment variable:
  + Open Advanced System Settings either by running `sysdm.cpl` or searching in Control Panel.
  + Click the Environment Variable button on the Advanced tab.
  + Edit the `PATH` variable to include the Processing directory in either the User variables (for just your account) or System variables (for all users).

Now, you can run Processing sketches by running `Packages > Script > Run Script`. The default shortcut is `command + shift + b` or `ctrl + shift + b`, but to further decrease the pain of transition, you can bind the Processing IDE shortcut to run the sketch. In order to do that:
+ Open up the Atom Keymap file by running `File > Keymap`
+ Paste following lines at the end of the file (feel free to change the binding to whatever you want).

      'atom-text-editor':
        'ctrl-r': 'script:run'

## Using Processing with Eclipse
To use Processing in Eclipse, start by creating a new Java project. Then, select `File > Import` and then choose `General > File System` to locate the `core.jar` file. It can be found in `PATH_TO_PROCESSING/core/library/` for Windows or `/Applications/Processing 3.app/Contents/Java/core/library/` for Mac. Once this is completed, right-click on `core.jar` and add it to the build path.

The boilerplate for using Processing in Eclipse is as follows:

    import processing.core.PApplet;
    
    public class UsingProcessing extends PApplet {
    
        // The argument passed to main must match the class name
        public static void main(String[] args) {
            PApplet.main("UsingProcessing");
        }
    
        // method used only for setting the size of the window
        public void settings(){
            
        }
    
        // identical use to setup in Processing IDE except for size()
        public void setup(){
            
        }
    
        // identical use to draw in Prcessing IDE
        public void draw(){
    
        }
    }

The `settings()` method is used to set the size of the window. For example, to create a 400x400 window, write the following:
   

     public void settings(){
        size(400,400);   
     }

Everything else as outlined in the [Hello World documentation][1] in terms of the use of `setup()` and `draw()` applies here.

As a final example, here is the code from the [Drawing a Line example][2] were it to be written in Eclipse:

    import processing.core.PApplet;
    
    public class UsingProcessing extends PApplet {
    
        // The argument passed to main must match the class name
        public static void main(String[] args) {
            PApplet.main("UsingProcessing");
        }
    
        // method for setting the size of the window
        public void settings(){
            size(500, 500);
        }
    
        // identical use to setup in Processing IDE except for size()
        public void setup(){
            background(0);
            stroke(255);
            strokeWeight(10);
        }
    
        // identical use to draw in Prcessing IDE
        public void draw(){
            line(0, 0, 500, 500);
        }
    }

  [1]: https://www.wikiod.com/processing/getting-started-with-processing#Hello World
  [2]: https://www.wikiod.com/processing/drawing-basic-shapes#Drawing a Line


