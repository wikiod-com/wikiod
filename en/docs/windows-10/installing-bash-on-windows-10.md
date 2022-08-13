---
title: "Installing Bash on Windows 10"
slug: "installing-bash-on-windows-10"
draft: false
images: []
weight: 9989
type: docs
toc: true
---

## Install Bash under Windows 10
This Documentation is a small summary of the [offical one](https://msdn.microsoft.com/en-us/commandline/wsl/install_guide)

# Prerequisites #

 1. Your PC must be running a 64-bit version of Windows 10 Anniversary Update build 14393 or later
    > To find your PC's CPU architecture and Windows version/build number, open Settings>System>About. Look for the OS Build and System Type fields. 
2. Turn-on Developer Mode
    - Open Settings -> Update and Security -> For developers
    - Select the Developer Mode radio button
3. Enable the Windows Subsystem for Linux feature (GUI or command-line
   - From Start, search for "Turn Windows features on or off"
      - Select Windows Subsystem for Linux (beta)
      - Click OK
   - **OR** Open a PowerShell prompt as administrator and run:
    `Enable-WindowsOptionalFeature -Online -FeatureName Microsoft-Windows-Subsystem-Linux`

4. Restart your computer


# Install Bash on Windows #

Open a command prompt and run `bash`

After you have accepted the License, the Ubuntu user-mode image will be downloaded and a "Bash on Ubuntu on Windows" shortcut will be added to your start menu.

After installation your Linux distribution will be located at: %localappdata%\lxss\

# Run Bash on Windows # 
To run bash just like in the other step

> Open a command prompt and run `bash`

Or use the "Bash on Ubuntu on Windows" shortcut

The first time you install Bash on Windows, you will be prompted to create a UNIX username and password.

Credits to the [official link](https://msdn.microsoft.com/en-us/commandline/wsl/install_guide)!

## First Step
Hello everyone,

 This first article help some people to install bash on windows 10.

 In first step you need uninstall all antivirus that you have, for example, I use Avast, I needed to uninstall this to install the bash.
 If you use windows protection, unable this to forward.

## Installing
Open the prompt on Windows.<br>
Than you will run: **lxrun /install**<br>
If this is the first time, you will inform a user to Bash console.<br>
Or you can just use **lxrun /install /y** <br>
To do this step automatically.<br>

## Uninstalling
You just need run on prompt:<br>
**lxrun /uninstall /full**

