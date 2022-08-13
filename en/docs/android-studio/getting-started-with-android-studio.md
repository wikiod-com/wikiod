---
title: "Getting started with android-studio"
slug: "getting-started-with-android-studio"
draft: false
images: []
weight: 1
type: docs
toc: true
---

## Installation or Setup



# Windows
 1. Verify that you have the correct JDK. You can check it by opening command prompt (press <kbd>windows key</kbd> and write `cmd`). In the command prompt type `javac -version`, this will show the current version of JDK installed on your matching or an error* if Java is missing. If the JDK is not available or the version is lower than 1.8, download the [Java SE Development Kit 8][2].

 1. Download the latest [Android Studio][1].
 1. Launch the downloaded `.exe` file.
 1. Follow the wizard to install Android Studio
 1. After installation completes, open Android Studio from the shortcut that has been made on Desktop during the installation process.
 1. When you open Android Studio for first time, it may ask you to copy your previous settings, but as it is your first time you can simply choose not to copy anything.
 1. Then Android Studio ask to download the required API's to develop Android apps. Download those, after downloading the IDE will open and you will be able to write your first app.  
  
# Mac
 1. Verify that you have the correct JDK. You can check it by opening terminal (press command + space and write terminal). In the command line type javac -version, this will show the current version of JDK installed on your matching or an error* if Java is missing. If the JDK is not available or the version is lower than 1.8, download the [Java SE Development Kit 8.][2] **

 1. Download the latest [Android Studio][1].
 1. Launch the downloaded `.dmg` file.
 1. Drag and drop Android Studio into the Applications folder, then launch Android Studio.
 1. Open Android Studio.
 1. When you open Android Studio for first time, it may ask you to copy your previous settings, but as it is your first time you can simply choose not to copy anything.
 1. Then Android Studio ask to download the required API's to develop Android apps. Download those, after downloading the IDE will open and you will be able to write your first app.

# Linux

 1. Verify that you have the correct JDK. You can check it by opening terminal (press <kbd>command + space</kbd> and write `terminal`). In the command line type `javac -version`, this will show the current version of JDK installed on your matching or an error* if Java is missing. If the JDK is not available or the version is lower than 1.8, download the [Java SE Development Kit 8][2]. **

 2. Download the latest [Android Studio][1].
 3. Unzip/extract Android Studio in a specific folder.
 4. Open terminal and go the path where you have extracted the Android Studio.
(Then, use cd command to go inside the Android Studio folder.) After going in we need to go inside `bin` folder so again,use command `cd bin` and enter.
 5. Now we need to change the mod of our required file i.e studio.sh to do so enter command `sudo chmod 777 -R studio.sh` , press enter and write your password(if any) and enter.
(Also you can see the list of files present inside bin by command  `ls`.).
 6. After changing mod we just have to run the `.studio.sh ` file to do so enter command `./studio.sh`
1. When you run above command Android Studio launch for first time, it may ask you to copy your previous settings, but as it is your first time you can simply choose not to copy anything.
 1. Then Android Studio ask to download the required API's to develop Android apps. Download those, after downloading the IDE will open and you will be able to write your first app.

> You may encounter "unable to run mksdcard SDK tool" when you are installing android studio in 64 bit ubuntu os because studio requires some 32 binaries.
To overcome this error finish and close Android Studio & go to terminal and run `sudo apt-get install lib32z1 lib32ncurses5 lib32bz2-1.0 lib32stdc++6 `.
Once installation is completed for these binaries again go back to step 6 and resume installation process.

### Notes  
 - If you have already installed JDK and still getting then make sure you have set `JAVA_HOME` in your `System Variables`. You can check this [answer][3] on how to setup one.     
 - There are, however, known stability issues in Android Studio on Mac when using JDK 1.8. Until these issues are resolved, you can improve stability by downgrading your JDK to an older version (but no lower than JDK 1.6).

 - While the Android Studio download completes, verify which version of the **JDK** you have: open a command line and type `javac -version`. If the JDK is *not* available or the version is lower than *1.8*, download the [Java SE Development Kit 8](http://www.oracle.com/technetwork/java/javase/downloads/jdk8-downloads-2133151.html).

 - If you are running a 64-bit version of **Ubuntu**, you need to install some 32-bit libraries with the following commands:

    `sudo dpkg --add-architecture i386`  
    `sudo apt-get update`  
    `sudo apt-get install lib32z1 lib32ncurses5 libbz2-1.0:i386 lib32stdc++6`


 - If you are running 64-bit **Fedora**, the command is:

    `sudo yum install zlib.i686 ncurses-libs.i686 bzip2-libs.i686`

### Problem with downloading

 - If you would face any issue like message that your system is not compatible with Android Studio (it may happen when you're using web browser different than Chrome) download `Android Studio` from: http://tools.android.com/

That's it!

  [1]: https://developer.android.com/studio/index.html
  [2]: http://www.oracle.com/technetwork/java/javase/downloads/jdk8-downloads-2133151.html
  [3]: http://stackoverflow.com/a/6521412/1320532

## Useful shortcuts
Navigation Shortcuts
-------------

     Go to class                       Ctrl+N
     Go to file                        Ctrl + Shift + N
     Navigate open tabs                ALT + Left-Arrow; ALT + Right-Arrow
     Lookup recent files               CTRL + E
     Go to line                        CTRL + G
     Navigate to last edit location    CTRL + SHIFT + BACKSPACE
     Go to declaration                 CTRL + B
     Go to implementation              CTRL + ALT + B
     Go to source                      F4
     Go to super Class                 CTRL + U
     Show Call hierarchy               Ctrl + Alt + H
     Search in path/project            CTRL + SHIFT + F

   Navigation Shortcuts - Mac OS X
-----------

     Go to line number                 CMD + L
     

Programming Shortcuts
------

     Reformat code                       CTRL + ALT + L
     Optimize imports                    CTRL + ALT + O
     Code Completion                     CTRL + SPACE
     Issue quick fix                     ALT + ENTER
     Surround code block                 CTRL + ALT + T
     Rename and refactor                 Shift + F6
     Line Comment or Uncomment           CTRL + /
     Block Comment or Uncomment          CTRL + SHIFT + /
     Go to previous/next method          ALT + UP/DOWN
     Show parameters for method          CTRL + P
     Quick documentation lookup          CTRL + Q
     Project                             Alt+1     
     Version Control                     Alt+9    
     Run                                 Shift+F10     
     Debug                               Shift+F9     
     Android Monitor                     Alt+6     
     Return to Editor                    Esc     
     Hide All Tool Windows               CTRL +Shift+F12
     Auto generate code(constructor,    
          getter/setter etc)             Alt+insert
     Code completion                     CTRL+Space
     Smart code completion
        (by expected type)               CTRL+Shift+Space
     Show quick fix                      CTRL+Enter
     Duplicate Line                      Ctrl+D
     Delete Line                         Ctrl+Y

## Preview Different Screen Size (Devices) and Orientations

<!-- if version [lte 2.1.3] -->

# 1. Preview Different Devices

There is a preview panel at the right of the android studio. In thispanel there is a button with device name with which you are previewing the `UI` of your app like this .  

[![enter image description here][1]][1]

Click on small dropdown indicator of this and a floating panel will appear with all the predefined devices. You can select any of them to preview your App `UI` with different devices mentioning their screen sizes.

Check this image   

[![enter image description here][2]][2]

# 2. Switching Orientation 

And Next to this button there is another button like 

[![enter image description here] [3]][3]

Clicking to its dropdown a floating panel will appear with some options like `portrait` , `landscape` etc. select one of them to preview in different orientations.  
Check the this image

[![enter image description here][4]][4]

<!-- end version if -->

<!-- if version [gt 2.2.0] -->

The option to preview all screen sizes is not available from Android Studio 2.2 Beta 2. However, you can resize the preview screen to see how your layout would look on different screen sizes, as shown in the attached.

[![Android Studio 2.2 Beta 3 Preview Screen resize example][1]][1]

Check: http://stackoverflow.com/questions/39165166/android-studio-2-2-preview-all-screen-sizes-missing

<!-- end version if -->


  [1]: http://i.stack.imgur.com/um1y0.gif
  [2]: http://i.stack.imgur.com/VXFCW.png
  [3]: http://i.stack.imgur.com/JD43d.png
  [4]: http://i.stack.imgur.com/VGqle.png

## Use your favorite tool shortcuts in Android Studio
Go to File > Settings > Keymap and select the Keymaps option from:

 - Mac OS X 
 - Emacs
 - Visual Studio
 - Eclise
 - Netbeans
 - Jbuilder

and others, to map the shortcuts to the wanted tool ones.

