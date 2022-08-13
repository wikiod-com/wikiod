---
title: "How to install Sublime 3 on CentOS 7  RHEL 7?"
slug: "how-to-install-sublime-3-on-centos-7--rhel-7"
draft: false
images: []
weight: 9972
type: docs
toc: true
---

## Here are the steps:
**Step 1: Download Sublime:**
You can either download from their website or use the wget as I did:

    $ cd ~/Downloads
    ## On 32bit
    $ wget https://download.sublimetext.com/sublime_text_3_build_3126_x32.tar.bz2
    
    ## On 64bit
    $ wget https://download.sublimetext.com/sublime_text_3_build_3126_x64.tar.bz2

**Step 2. Extract Sublime package (example to /opt directory)**
You can store Sublime at any place you want. Here, I saved  under /opt directory. 

    ## On 32bit
    $ sudo tar -vxjf sublime_text_3_build_3126_x32.tar.bz2 -C /opt
    ## On 64bit
    $ sudo tar -vxjf sublime_text_3_build_3126_x64.tar.bz2 -C /opt

**Step 3. Now, let’s make a symbolic link to the installed Sublime3 so that we can run the same from command line**

    # sudo ln -s /opt/sublime_text_3/sublime_text /usr/bin/sublime3

**Step 4. Now, let’s test whether the Sublime3 is installed correctly or not. Type sublime3 in command line and it will open up the sublime window.**

    $ sublime3

**Step 5. Create Gnome desktop launcher**
You can run Sublime3 on desktop by clicking a icon.

    $ sudo sublime3 /usr/share/applications/sublime3.desktop

**Step 6. Append this and close file.**

    [Desktop Entry]
    Name=Sublime3
    Exec=sublime3
    Terminal=false
    Icon=/opt/sublime_text_3/Icon/48x48/sublime-text.png
    Type=Application
    Categories=TextEditor;IDE;Development
    X-Ayatana-Desktop-Shortcuts=NewWindow
     
    [NewWindow Shortcut Group]
    Name=New Window
    Exec=sublime -n
    TargetEnvironment=Unity

Now, you can see the Sublime3 icon on under Applications → Programming.
You can run Sublime3 on desktop by clicking this icon. Enjoy!!


