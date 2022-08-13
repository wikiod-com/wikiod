---
title: "Getting started with sublimetext3"
slug: "getting-started-with-sublimetext3"
draft: false
images: []
weight: 1
type: docs
toc: true
---

## Installation or Setup
Please note that continued use of Sublime Text requires that you [purchase a license][1] and you are asked to note the [terms and conditions][2].

The process of **installing** Sublime Text is different for each platform, but in each case you need to visit the [download page][4].

After installing ST3, it is common to install the package manager, [Package Control][5].

----
# Mac
**For Mac**, there is only one version of Sublime Text for OS X.

 1. Download .dmg file 
 2. Open .dmg file 
 3. Drag the Sublime Text 3 bundle into the Applications folde
 4. To create a symbolic link to use at the command line issue the
    following command at the terminal:

    ln -s  "/Applications/Sublime Text.app/Contents/SharedSupport/bin/subl" /usr/local/bin/subl

---
# Windows
**For Windows**, both 64-bit and 32-bit versions are available, portable and non-portable.
You should be able to run the 64-bit version if you are using a modern version of Windows. If you are having trouble running the 64-bit version, try the 32-bit version.

**Portable or Not Portable?**
Sublime Text comes in two flavors for Windows: normal, and portable. Most users should be better served by a normal installation. Use the portable version *only if you know you need it.*

Normal installations separate data between two folders: the installation folder proper, and the data directory (user-specific directory for data). Normal installations also integrate Sublime Text with Windows Explorer's context menu.

Portable installations keep all files needed by Sublime Text in a single folder. This folder can be moved around and the editor will still work.

**How to Install the Normal Version of Sublime Text**

 1. Download the installer
 2. Double click on the installer

**How to Install the Portable Version of Sublime Text**

 1. Download the compressed files

 2. Unzip them to a folder of your choice

You will find the sublime_text.exe executable inside that folder.

----
# Linux
**For Linux**, run this command in your terminal to check your operating system’s type:

    uname -m

You can download the package and uncompress it manually. Alternatively, you can use the command line.

**Ubuntu**
- For i386

      cd ~
      wget http://c758482.r82.cf2.rackcdn.com/sublime-text_build-3083_i386.deb

- For x64

      cd ~
      wget http://c758482.r82.cf2.rackcdn.com/sublime-text_build-3083_amd64.deb

Other Linux Distributions
- For i386

      cd ~
      wget http://c758482.r82.cf2.rackcdn.com/sublime_text_3_build_3083_x32.tar.bz2
      tar vxjf sublime_text_3_build_3083_x32.tar.bz2

- For x64

      cd ~
      wget http://c758482.r82.cf2.rackcdn.com/sublime_text_3_build_3083_x64.tar.bz2
      tar vxjf sublime_text_3_build_3083_x64.tar.bz2

Now we should move the uncompressed files to an appropriate location.

    sudo mv Sublime\ Text\ 3 /opt/
    Lastly, we create a symbolic link to use at the command line.
    
    sudo ln -s /opt/Sublime\ Text\ 3/sublime_text /usr/bin/sublime

In Ubuntu, if you also want to add Sublime Text to the Unity launcher, do the following:

First, create a new file.

    sudo sublime /usr/share/applications/sublime.desktop

Copy the following into it.

    [Desktop Entry]
    Version=1.0
    Name=Sublime Text 3
    # Only KDE 4 seems to use GenericName, so we reuse the KDE strings.
    # From Ubuntu's language-pack-kde-XX-base packages, version 9.04-20090413.
    GenericName=Text Editor
    
    Exec=sublime
    Terminal=false
    Icon=/opt/Sublime Text 3/Icon/48x48/sublime_text.png
    Type=Application
    Categories=TextEditor;IDE;Development
    X-Ayatana-Desktop-Shortcuts=NewWindow
    
    [NewWindow Shortcut Group]
    Name=New Window
    Exec=sublime -n
    TargetEnvironment=Unity

If you’ve registered your copy of Sublime Text, but every time you open it you’re asked to enter your license, you should try running this command.

    sudo chown -R username:username /home/username/.config /sublime-text-3

Replace username with your account’s username. This should fix the permission error in the case that you opened up Sublime Text as root when you first entered the license.

[Reference][3]


  [1]: http://www.sublimetext.com/buy
  [2]: http://www.sublimetext.com/eula
  [3]: http://docs.sublimetext.info/en/latest/getting_started/install.html
  [4]: http://www.sublimetext.com/3
  [5]: https://packagecontrol.io/installation


## Start with the Sublime Tutor
This tutorial is inspired from classic vimtutor. You will get to learn some handy shortcuts to work with Sublime Text 3. By the end of this tutorial, you would be familiar with ST's most important and frequently used shortcuts and features.

Installation
-------------

Via [Package Control](https://Sublime.wbond.net/):

1. Install [Package Control](https://Sublime.wbond.net/) if already not installed:
   https://packagecontrol.io/installation#st3
2. Press <kbd>Cmd</kbd>+<kbd>Shift</kbd>+<kbd>P</kbd> to bring command palette
   in front
3. Type `Install Package` and press enter.
4. Search for `Sublime Tutor` and press enter to install the plugin.


### Manual Installation:

1. Make sure you have [git][1] already installed.
2. `cd` into the `Packages` directory of Sublime Text 3. On Mac, it usually 
   resides at the following path: `~/Library/Application Support/Sublime Text 3
   /Packages/`. Alternatively you can open in via a menu item:
   `Preferences > Browse Packages...`
3. Once you are inside Packages directory, clone this repository:
   `git clone git@github.org:jai/sublimetutor.git`. Alternatively download and
   extract the latest release for your platform here:
   https://github.com/jaipandya/SublimeTutor/releases
4. Restart Sublime Text


Getting Started
----------------

If you haven't already, install Sublime Tutor using the installation steps
given above.

Once Sublime Tutor is installed, press <kbd>Ctrl</kbd>+<kbd>Option</kbd>+<kbd>K</kbd>
keyboard shortcut to open this file in Sublime Text. Another option is to go to 
`Help > Sublime Tutor` menu option to open this.

Via Command Palette:

1. <kbd>Cmd</kbd>+<kbd>Shift</kbd>+<kbd>P</kbd> to get the command palette in
   front.
2. Type `Sublime Tutor`, select the first command that comes up to start the
   interactive guide.

Source:

1. https://sublimetutor.com/
1. https://github.com/jaipandya/SublimeTutor
1. https://packagecontrol.io/packages/Sublime%20Tutor

