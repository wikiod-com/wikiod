---
title: "Getting started with AutoHotkey"
slug: "getting-started-with-autohotkey"
draft: false
images: []
weight: 1
type: docs
toc: true
---

## Hello World
Show a "Hello World!" in message box.

    MsgBox, Hello World!
Show a "Hello World!" in tooltip.

    #Persistent
    Tooltip, Hello World!
Show a "Hello World!" message in the traybar edit.

    #Persistent
    TrayTip,, Hello World!

Prints "Hello, World" to Standard Output (stdout).

    FileAppend, % "Hello, World", *

## Installation or Setup
From [Autohotkey Site Documentation][1]

 1. Go to the [AutoHotkey Homepage][2].
 1. Click [Download][3], once downloaded run the executable.  
 1. During installation of AutoHotkey, you will be asked to choose from UNICODE or ANSI. In short, you would probably want to choose UNICODE. It has support for non-English letters and numbers (characters). 
 1. Keep going until you see an Install button. 
 1. Once done, great!

Use as [portable software][4]

 1. Go to the AutoHotkey's [download page][5].
 2. Find the Portable section, choose from UNICODE 32, 64 or ANSI and downloaded.
 3. When choosing the destination folder, pick any correct storedevice external or not.
 4. Now you can opt to associate .ahk files with Autohotkey.exe
 5. Create a plain text file and give it the .ahk extension
 6. Then Right-click on the .ahk file in explorer and click Properties.
 7. In the file Properties, click the Change button next to the "Opens with" option.
    - After clicking Change, you'll be given a list of programs to open
    the    file, select the program you want to use and then click OK or
    Apply.
    - If the program you want to select is not listed, click the
    Browse button and find the Autohotkey executable (.exe) file and
    click OK to    select that program.
 9. Now .ahk files will run as if autohotkey was installed, great!

If you have chocolatey installed, run the following command as an admin user

> choco install autohotkey

Alternatively, it can be built from the source code. See here for details:  
https://github.com/Lexikos/AutoHotkey_L/


  [1]: https://autohotkey.com/docs/Tutorial.htm#s11
  [2]: https://autohotkey.com
  [3]: https://autohotkey.com/download/ahk-install.exe
  [4]: http://portableapps.com/about/what_is_a_portable_app
  [5]: https://autohotkey.com/download/

## Show "Hello World" in a GUI
    Gui, Add, Text,, Hello World!
    Gui, Show, w200 h200
    return
    
    GuiClose:
    ExitApp

## Achieve an effect similar to SplashTextOn
    Gui, +AlwaysOnTop +Disabled -SysMenu +Owner  ; +Owner avoids a taskbar button.
    Gui, Add, Text,, Some text to display.
    Gui, Show, NoActivate, Title of Window  ; NoActivate avoids deactivating the currently active window.

## How to create a Script
Once you have AutoHotkey installed, you will probably want it to do stuff. AutoHotkey is not magic, we all wish it was, but it is not. So we will need to tell it what to do. This process is called "Scripting".

1. Right-Click on your desktop.
2. Find "New" in the menu.
3. Click "AutoHotkey Script" inside the "New" menu.
4. Give the script a new name. Note: It must end with a .ahk extension. Ex. MyScript.ahk
5. Find the newly created file on your desktop and Right-Click it.
6. Click "Edit Script".
7. A window should have popped up, probably Notepad. If so, SUCCESS!

So now that you have created a script, we need to add stuff into the file. For a list of all built-in commands, function and variables, see section 5.
Here is a very basic script containing a Hotkey which types text using the Send command when the hotkey is pressed.

    ^j::
       Send, My First Script
    Return

We will get more in-depth later on. Until then, here's an explanation of the above code.
- The first line. `^j::` is the Hotkey. `^` means `CTRL`, `j` is the letter j. Anything to the left of `::` are the keys you need to press.
- The second line. `Send, My First Script` is how you `SEND` keystrokes. `SEND` is the command, anything after the comma (,) will be typed.
- The third line. `Return`. Return will become your best friend. It literally STOPS code from going any further, to the lines below. This will prevent many issues when you start having a lot of stuff in your scripts.

8. Save the File.
9. Double-Click the file/icon in the desktop to run it. Open notepad or (anything you can type in) and press Ctrl and J.
10. Hip Hip Hooray! Your first script is done. Go get some reward snacks then return to reading the rest of this tutorial.

