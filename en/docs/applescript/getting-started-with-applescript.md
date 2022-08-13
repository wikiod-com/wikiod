---
title: "Getting started with applescript"
slug: "getting-started-with-applescript"
draft: false
images: []
weight: 1
type: docs
toc: true
---

## Your first AppleScript
1. Open Script Editor.

   <!-- if version [lt 2.1] [gte 2.4] -->

   With Mac OS X Leopard and earlier, and OS X Yosemite and later, Script Editor is located at

    /Applications/Utilities/Script Editor.app

   <!-- end version if -->

   <!-- if version [gte 2.1] [lt 2.4] -->

   Between Mac OS X Snow Leopard and OS X Mavericks inclusive, Script Editor is AppleScript Editor.

    /Applications/Utilities/AppleScript Editor.app

   <!-- end version if -->

2. Enter the following line of code:

       display dialog "Hello World"

3. Click the run button.

   <img src="http://i.stack.imgur.com/yoxfk.png" width="234">

   Two things happen: Script Editor compiles your script (if there are any errors, it'll let you know what's wrong and where), and then runs it.

4. You will now be shown a dialog saying "Hello World".

   <img src="http://i.stack.imgur.com/uCWfF.png" width="420">

You've completed your first script!

## What the code does:

`display dialog` is one command, though it uses two words. This is common in AppleScript, unlike other languages which commonly require joining words together with no spaces. The `display dialog` command tells AppleScript to display a pop-up dialog.

This last part in "double quotes" tells the script what text to display.

## Exploring scripting dictionaries
The power of AppleScript lies in being able to automate many Mac applications. To find out what you can automate, you need to read an app's scripting dictionary.

To do so, launch Script Editor, and select File > Open Dictionary…

[![Open Dictionary window][1]][1]

Once you choose an app, its dictionary will open up in a new window. At the top of the window, you'll see a column view (like Finder's). The first column contains various "suites" of AppleScript terminology. The "Standard Suite" contains items that apply to most apps (e.g. `open`, `close`, `save`, `print`), and the other suites define custom behavior.

[![Dictionary window][2]][2]


  [1]: http://i.stack.imgur.com/e7MJF.png
  [2]: http://i.stack.imgur.com/sEQCo.png

