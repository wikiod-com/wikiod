---
title: "Android Studio Tips and Tricks"
slug: "android-studio-tips-and-tricks"
draft: false
images: []
weight: 9917
type: docs
toc: true
---

## Emulator for testing
If there are no specific needs, choose `x86_64` system images with Google APIs to create the emulator.

 It works way faster than `armeabi-v7a` on intel x86 x64 based computers. 


There are some SDK libraries compiled and designed with ARM architecture. 

If you try to install them on Intel based emulators you'll get an error message saying 

> "INSTALL_FAILED_NO_MATCHING_ABIS"

You can go for a 3rd party emulator like [Genymotion] [1] or [Visual Studio's standalone Android Emulator][2] on such occasions.   


  [1]: https://www.genymotion.com/
  [2]: https://www.visualstudio.com/en-us/features/msft-android-emulator-vs.aspx

## Custom Live Template
Example :
------------

[![live template example][1]][1]


To achieve this purpose , you should add a custom Live Template:
-------------

1. Open Settings [<kbd>Ctrl</kbd> + <kbd> Alt </kbd> + <kbd> S </kbd>]
2. Type "Live Templates" in the Top-Left search bar.
3. Click the <kbd> + </kbd> to add a "Template Group" and type a group name(eg: MyTemplate) to continue

[![add template group][2]][2]
4. Choose your custom template group(eg: MyTemplate), then click <kbd> + </kbd> and choose "Live Template".
5. I will take "say" for example:
    >`say` -> `startActivity(new Intent($from$.this,$to$.class));`
6. Type the key that you want to simplify in "Abbreviation" (eg: say),and type the statement in the "Template Text" (eg :`startActivity(new Intent($from$.this,$to$.class));`)

[![enter image description here][3]][3]
7. Click the "define" in the Left-Bottom and choose the situation(eg: Java)
8. Click the "Edit variables",define the expression.(see detail:[Edit Template Variables Dialog][4])

[![expression][5]][5]
9. Click "OK" and "Apply". And try to type "say" in your editor.


  [1]: http://i.stack.imgur.com/PflV8.gif
  [2]: http://i.stack.imgur.com/9GoRf.png
  [3]: http://i.stack.imgur.com/TPYZt.png
  [4]: https://www.jetbrains.com/help/idea/2016.2/edit-template-variables-dialog.html
  [5]: http://i.stack.imgur.com/Rzu3P.png

## Use Custom Code Styles, Share with other Team Members  and Auto Format with Shortcut
It's possible to have your own custom code styles, share them with other team members and use a shortcut to auto format the code in a file.

To create your own custom code style, go to:
Preferences -> Editor -> Code Style

There are some general code style settings here.  You can also select the language (Java for Android) and set the parameters as you see fit.  There's a *lot* of settings.

Once you've done this, save your code style.  This is just for safe keeping.

[![Manage code style schemes][1]][1]

[![Save code style scheme][2]][2]

Once you've saved it, select "Copy to Project" from the "Manage" dialog.  You will be asked whether you want to switch to this created scheme.  Answer yes.

[![Copy scheme to project][3]][3]

[![enter image description here][4]][4]

Once this is done, close the Code Style preferences dialog.

Now, verify that your code style settings have been saved in:
.idea/codeStyleSettings.xml

Ensure that this file is not ignored in your version control system so that you can share it with your teammates.

Once your teammates have this file, they should also have the same settings.

Now, when editing a file, you can format the code by selecting:

Code -> Reformat Code

Shortcuts for Reformat Code (taken from [this][5] answer - see answer for details on resolving issues with shortcut in Ubuntu):

Win

> <kbd>Ctrl</kbd> + <kbd>Alt</kbd> + <kbd>L</kbd>

Linux:

> <kbd>Ctrl</kbd> + <kbd>Shift</kbd> + <kbd>Alt</kbd> + <kbd>L</kbd>


Mac:
> <kbd>Option</kbd> + <kbd>Command</kbd> + <kbd>L</kbd>

When you perform a code reformat, a small dialog should popup informing you of the changes made.  You can click "Show" in this dialog to bring up the "Reformat File Dialog".

[![code format info][6]][6]

You can also bring up this dialog from the Code menu and its corresponding shortcut.

[![enter image description here][7]][7]

Be warned that "Only VCS changed text" doesn't always work depending on how the code has been edited (it may ignore a rule if part of the code has not been edited).

You can also select text and show the reformat code dialog to format only the select text.



  [1]: https://i.stack.imgur.com/UJnHL.png
  [2]: https://i.stack.imgur.com/7CbtZ.png
  [3]: https://i.stack.imgur.com/Vtci9.png
  [4]: https://i.stack.imgur.com/EUQh7.png
  [5]: http://stackoverflow.com/a/16580200/487812
  [6]: https://i.stack.imgur.com/op8Gj.png
  [7]: https://i.stack.imgur.com/PsoZQ.png

