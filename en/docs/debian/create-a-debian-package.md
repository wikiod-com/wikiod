---
title: "Create a debian package"
slug: "create-a-debian-package"
draft: false
images: []
weight: 9993
type: docs
toc: true
---

This topic show how to create a debian package for software deployment.

## RegExpTestor
This example show how I have created a package for my project [RegExpTestor](https://github.com/thibDev/RegExpTestor)  
So first of all, you have to create a tree. In my case I did this:  

    regexp_testor
    ├── DEBIAN
    │   ├── control
    │   └── postinst
    ├── opt
    │   └── regexp_testor
    │       ├── Icon
    │       │   └── 48x48
    │       │       └── regexp_testor_icon.png
    │       └── regexp_testor
    └── usr
        ├── bin
        └── share
            ├── applications
            │   └── regexp_testor.desktop
            └── icons
                └── hicolor
                    └── 48x48
                        └── apps
                            └── regexp_testor_icon.png
* ```control``` and ```postinst``` are script files without extension with 755 right
* ```regexp_testor``` is my binary
* ```regexp_testor.desktop``` is to add a shortcut in the menu

Now the content of files:  

```control``` file:

    Package: regexp-testor
    Version: 1.0
    Section: devel
    Priority: optional
    Architecture: amd64
    Depends: qt5-default
    Maintainer: thibDev <your@email.com>
    Homepage: https://github.com/thibDev
    Description: RegExp testor
     RegExp testor is a regular expression testor like the rubular.com website.

```postinst``` file:

    #!/bin/bash
    echo "Create a symbolic link towards the binary"
    sudo ln -s /opt/regexp_testor/regexp_testor /usr/bin
    echo "Enjoy ;)"

```regexp_testor.desktop``` file:

    [Desktop Entry]
    Version=1.0
    Type=Application
    Name=RegExp testor
    GenericName=RegExp testor
    Comment=A regular expression testor like the rubular.com website
    Exec=/opt/regexp_testor/regexp_testor
    Terminal=false
    MimeType=text/plain;
    Icon=regexp_testor_icon
    Categories=Development;
    StartupNotify=true
Now you have everything you need to create your package ! Follow these steps
1. move into the folder that contain your package
2. run ```sudo dpkg-deb --build package_name```

You have your package !


