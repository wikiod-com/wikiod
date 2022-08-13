---
title: "Getting started with windows-10"
slug: "getting-started-with-windows-10"
draft: false
images: []
weight: 1
type: docs
toc: true
---

## Installation or Setup
Getting installed on Windows 10 is as easy as it's ever been, and despite (true) rumours flying around regarding Windows 10 being the last 'normal' version of Windows, it has and always will stay the same process.

<h2>Installing a new version</h2>
<ol>
<li>Get the windows image (Go straight to step 4 if you have a shop bought disk)</li>
Go to the Windows technet site and download the appropriate version of Windows 10. You will need to enter your purchased product key.
<li>Download a USB imager</li>
Search and download Rufus USB imaging tool. Run this.
<li>Load up the image onto your USB</li>
Click the dropdown at the top and select your USB. Then click the second dropdown and select ISO image option. Finally, click the picture next to the dropdown and find your ISO file. Select it then click 'Start'. When this has done, eject your USB.
<li>Installing</li>
Turn your laptop off and back on. Watch very closely for any text that pops up saying 'Press <button> to enter Boot menu'. When you see this, make a mental note of the button name and find it on your keyboard. Then insert your installation media and restart again, and when the text pops up, press the button. If all goes well, you should have the option to run the installation media. Once that's happened, just follow the on screen prompts to install Windows 10.
</ol>
<h2>Upgrading</h2>
<ol>
<li>Downloading the iso</li>
Follow step 1 in the above guide
<li>Mount the ISO file</li>
Right click this file and click the 'Mount' option
<li>Run the installer</li>
Go to 'This PC' and open up the Disc Image (should show up as a disk). Then run the 'setup.exe' file in there. Finally, just follow the on screen prompts. This needs admin rights, and takes a while, but once this has happened, all your files will be right where they were before, it'll just be Windows 10!

## OneGet and packet managers
OneGet was originally a product from the Open Source Technology Center at Microsoft. Not only is it inspired by open-source Linux package managers, OneGet itself is also open source. It's now part of PowerShell

As opposed to Unix based package managers (such as `apt-get`, `yum`, or `dpkg`), Windows allows usage of Oneget through PowerShell. To do this, you first need to open a Powershell as Admin and run `Set-ExecutionPolicy RemoteSigned`. This is to ensure that the Powershell scripts available in Oneget are allowed to run. Then (using the same PowerShell instance), run `Import-Module –Name OneGet`. This imports the PoerShell module. Then restart your powershell instance, and run `Get-Command –Module OneGet` to see all available commands. To install a package, run the command `Find-Package | <Package_Name`. When prompted to install Nuget, click yes.

# Adding Chocolatey as provider to OneGet

Unfortunately, OneGet doesn't have the [chocolatey](https://chocolatey.org/) package provider installed by default, but it is possible to add it.

Open a powershell window and run `get-packageprovider -name chocolatey`. You should then see the below message:

> The provider 'chocolatey v2.8.5.130' is not installed. chocolatey may
> be manually downloaded from https://oneget.org/ChocolateyPrototype-2.8.5.130.exe and
> installed. Would you like PackageManagement to automatically download
> and install 'chocolatey'?
> 
> [Y] Yes  [N] No  [S] Suspend  [?] Help (default is "Y"):
> 
> 
Type 'Y' then press enter.

    Name                     Version          
    ----                     -------           
    Chocolatey               2.8.5.130

     

# Finding packages

For most well known programs, the name of the app usually works (for example, `Chrome`). but, just as chocolatey can be hard to navigate, so can OneGet. Luckily, OneGet has an implementation of `choco search`. This command is now `find-package <Package Name>`. This will give you a list of packages as seen below:

    Name                           Version          Source                         Summary
    ----                           -------          ------                         -------
    thunderbird                    45.2.0           chocolatey                     A free email client from Mozilla
    Google Chrome                  47.5.8           OneGet                         A high end web browser


# Installing packages
To install a package, you need to run a PowerShell as admin, and run the command `install-package <PAckage Name>`

> The package(s) come from a package source that is not marked as
> trusted. Are you sure you want to install software from 'chocolatey'?
> [Y] Yes  [A] Yes to All  [N] No  [L] No to All  [S] Suspend  [?] Help
> (default is "N"):

To this you can just press y and/or enter. You will then see a display similar to the one below

    
    Name                           Version          Source           Summary
    ----                           -------          ------           -------
    <Package Name>                 <Most recent>    <Server name>    <Description>

# Uninstalling

To uninstall a package, you just use the below command

    uninstall-package vlc

