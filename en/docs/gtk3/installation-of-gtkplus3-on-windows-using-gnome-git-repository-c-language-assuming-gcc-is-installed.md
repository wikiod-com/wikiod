---
title: "Installation of GTK+3 On Windows (using GNOME GIT Repository)( C Language-Assuming GCC is Installed)"
slug: "installation-of-gtk+3-on-windows-using-gnome-git-repository-c-language-assuming-gcc-is-installed"
draft: false
images: []
weight: 9993
type: docs
toc: true
---

## Downloading GTK+3 (also suitable for other versions) and Setting Up
**Downloading the Bundle:**

The easiest way to download the required GTK Bundle is to search and download it using this link: https://git.gnome.org/browse/gtk+ (GNOME GIT Repository)

*GNOME GIT Repository* provides the bundles for different versions and you can easily find the desired version by scrolling through the list. These are uploaded by other users or authors and you will often find new bundles coming up.
[![Preview of the List][1]][1]

*Alternatives:*

The other option or the alternate method to acquire the bundle is to use MSYS2. This method is a bit complicated and is not well explained or documented. The best way is to directly download the bundle from an external file hosting website. 

MSYS2 Method- 

 1. http://www.gtk.org/download/windows.php (Official Website Resource on acquiring the Bundle)

 2. https://msys2.github.io/ (download MSYS2)

> ***Note : Rest Of This Documentation Is Based On The Assumption That The GTK Bundle Is Downloaded From The GNOME GIT Repository. Not Tested With MSYS2 Method. Since The Bundle Remains Universal, The Method Used Shouldn't Be An Issue. Based On Ease, The Highly Recommended Way To Obtain The Bundle is T0 Use GNOME GIT Repository.***

**Extracting and Storing the Bundle in a Desired Location (Explained using the C: Drive).**

 1. Create a Folder Named `gtk` in the `C: Drive`. You could choose other names for the folder but this name helps us to identify the content pretty easily and remembering the name becomes easier. Let us term the Gtk folder location as `%GD%` for ease and in this case it is `C:\gtk` (could be different in your case).
 [![After Extracting][2]][2]
 2. > Setting the Path for Gtk binary in Windows Environment Variables.This is important in order to access and obtain the required `Gtk` libraries while using `Cmd` to Compile and run the Programs. To set up the Path Environment Variable:

> - `Windows XP` : `right-click on "My Computer" -> "Properties"`. 
> - `Windows Vista/7/8/10` : `right-click on "Computer" -> "Properties" -> "Advanced system settings".  Click on "Advanced tab" -> "Environment
> variables".` 

> - Add new `Gtk bin file Path (%GD%\bin)` to the Path variable (Under System Variables or add a variable named Path to the User Variables Section and add the value in it) available in the dialogue box which opens. You should also add a new variable named `PKG_CONFIG_PATH` and give it the following value- `C:\gtk\lib\pkgconfig` (since my gtk folder is located in C: drive).

| Variable Name| Value |
| ------       | ------ |
| Path        | %GD%\bin  |
| PKG_CONFIG_PATH   | %GD%\lib\pkgconfig   |

> %GD% is the location of the Gtk Folder
[![After Adding Path Variables][3]][3]
Now we are done with Extracting and setting up the Environment Variables. You will find that I have added the value to the Path variable in System variables sections as well as User Variables section. Adding to any one of these sections is more than enough.

3.***Testing the set-up (You must have GCC installed on your system already-MinGW preferable (http://www.mingw.org/):***
  Run the following commands on the cmd:

> pango-querymodules:`%GTKDIR%\etc\pango\pango.modules`

> gdk-pixbuf-query-loaders:`%GTKDIR%\lib\gdk-pixbuf-2.0\2.10.0\loaders.cache`

> gtk-query-immodules-3.0: `%GTKDIR%\lib\gtk-3.0\3.0.0\immodules.cache`

  - In the console, verify that "pkg-config" prints out something reasonable by typing : `pkg-config --cflags --libs gtk+-3.0` It should print out something similar to what is shown in the following image.[![Command Result][4]][4]

**That's It. You have Downloaded and Set UP GTK On Your Windows System.**


 


  [1]: http://i.stack.imgur.com/SRhIN.png
  [2]: http://i.stack.imgur.com/HTHLP.png
  [3]: http://i.stack.imgur.com/f7fwq.png
  [4]: http://i.stack.imgur.com/psVNQ.png

