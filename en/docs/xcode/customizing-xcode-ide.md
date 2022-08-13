---
title: "Customizing Xcode IDE"
slug: "customizing-xcode-ide"
draft: false
images: []
weight: 9990
type: docs
toc: true
---

This is collection of different tips and tricks, to customize and improve your Xcode IDE

## Open Terminal in current Xcode project folder
Xcode have ability to run any script with hotkey.

Here is example how to assign hotkey `⌘+⌥+⌃+⇧+T` to open Terminal app in current project folder.

 1. Create bash script and save it in some folder


    #!/bin/bash
    
    # Project Name:  $XcodeProject
    # Project Dir:   $XcodeProjectPath
    # Workspace Dir: $XcodeWorkspacePath
    
    open -a Terminal "$(dirname $XcodeProjectPath)"  

 2. Make script executable: open Terminal at script folder and run `chmod +x your_script_name.sh`

 3. Open Xcode Preferences at Behaviors tab
 4. Add new custom behavior by tapping `+` in the bottom left corner
 5. Check `Run` action at the right
[![enter image description here][1]][1]
 6. Choose script, which you create previously by clicking at the `Choose Script...` twice.

*If your script is grayed, be sure, that you run `chmod +x` on your script file*
[![enter image description here][2]][2]
 7. Assign hotkey (for example `⌘+⌥+⌃+⇧+T`) to your behavior and rename it
[![enter image description here][3]][3]


Now you can open terminal in project folder with one hotkey.

This is only one example of using Xcode behaviors, but you can create any script and launch any app with it.

Bash script author: http://mattorb.com/xcode-behaviors-for-fun-and-profit/


  [1]: https://i.stack.imgur.com/3whQR.png
  [2]: https://i.stack.imgur.com/FozZ7.png
  [3]: https://i.stack.imgur.com/c6TjC.png

## Clear derived data with hotkey
In the same way as in `Open Terminal in current Xcode project folder` example, you can add clear of derived data folder with hotkey.

Create custom behavior (follow the steps in [Open Terminal in current Xcode project folder][1]). But use another script.

Script text:

    #!/bin/bash
    
    rm -rf $HOME"/Library/Developer/Xcode/DerivedData/"


  [1]: https://www.wikiod.com/xcode/customizing-xcode-ide#Open Terminal in current Xcode project folder

