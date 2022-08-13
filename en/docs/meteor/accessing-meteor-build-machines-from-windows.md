---
title: "Accessing Meteor build machines from Windows"
slug: "accessing-meteor-build-machines-from-windows"
draft: false
images: []
weight: 9975
type: docs
toc: true
---

On Mac and Linux, the `meteor` command line tool assumes that the `ssh` command line tool, used to make secure connections to other computers, is always present. On Windows, this tool needs to be installed. Below are listed two options for setting it up and using it.


## Using PuTTY (Advanced)
If you don't want to add Unix commands to your PATH on Windows, you can download a standalone SSH client like PuTTY. [Download PuTTY here](http://www.putty.org/), then follow the instructions below to get a build machine.

1. Call `meteor admin get-machine <os-architecture> --json`
2. Copy and save the private key from the returned JSON data
3. Follow the directions [here](http://meinit.nl/using-your-openssh-private-key-in-putty) to convert the private key into a format that PuTTY accepts
4. Enter the hostname, username, and private key into PuTTY, and you're good to go!

## Using Cygwin (Unix tools on Windows)
The easiest way to get up and running is to install Git for Windows from [this download page](http://git-scm.com/downloads), and select "Use Git and optional Unix tools from the Windows Command Prompt" as in the screenshot below.

![Screenshot of Git for Windows installation step.](http://i.imgur.com/eSavpFN.png)

After this, `meteor admin get-machine <os-architecture>` will work exactly as it does on Linux and Mac. Keep in mind that you might need to restart your terminal to get the new commands.


