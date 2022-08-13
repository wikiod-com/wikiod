---
title: "Installation"
slug: "installation"
draft: false
images: []
weight: 9995
type: docs
toc: true
---

# Downloading Go

Visit the [Downloads List][1] and find the right archive for your operating system. The names of these downloads can be a bit cryptic to new users.

The names are in the format go[version].[operating system]-[architecture].[archive]

For the version, you want to choose the newest available. These should be the first options you see.

For the operating system, this is fairly self-explanatory except for Mac users, where the operating system is named "darwin". This is named after the [open-source part of the operating system used by Mac computers][2].

If you are running a 64-bit machine (which is the most common in modern computers), the "architecture" part of the file name should be "amd64". For 32-bit machines, it will be "386". If you're on an ARM device like a Raspberry Pi, you'll want "armv6l".

For the "archive" part, Mac and Windows users have two options because Go provides installers for those platforms. For Mac, you probably want "pkg". For Windows, you probably want "msi".

So, for instance, if I'm on a 64-bit Windows machine and I want to download Go 1.6.3, the download I want will be named:

`go1.6.3.windows-amd64.msi`

# Extracting the download files

Now that we have a Go archive downloaded, we need to extract it somewhere.

## Mac and Windows

Since installers are provided for these platforms, installation is easy. Just run the installer and accept the defaults.

## Linux

There is no installer for Linux, so some more work is required. You should have downloaded a file with the suffix ".tar.gz". This is an archive file, similar to a ".zip" file. We need to extract it. We will be extracting the Go files to `/usr/local` because it is the recommended location.

Open up a terminal and change directories to the place where you downloaded the archive. This is probably in `Downloads`. If not, replace the directory in the following command appropriately.

`cd Downloads`

Now, run the following to extract the archive into `/usr/local`, replacing `[filename]` with the name of the file you downloaded.

`tar -C /usr/local -xzf [filename].tar.gz`

# Setting Environment Variables

There's one more step to go before you're ready to start developing. We need to set environment variables, which is information that users can change to give programs a better idea of the user's setup.

## Windows

You need to set the `GOPATH`, which is the folder that you will be doing Go work in.

You can set environment variables through the "Environment Variables" button on the "Advanced" tab of the "System" control panel. Some versions of Windows provide this control panel through the "Advanced System Settings" option inside the "System" control panel.

The name of your new environment variable should be "GOPATH". The value should be the full path to a directory you'll be developing Go code in. A folder called "go" in your user directory is a good choice.

## Mac

You need to set the `GOPATH`, which is the folder that you will be doing Go work in.

Edit a text file named ".bash_profile", which should be in your user directory, and add the following new line to the end, replacing `[work area]` with a full path to a directory you would like to do Go work in. If ".bash_profile" does not exist, create it. A folder called "go" in your user directory is a good choice.

`export GOPATH=[work area]`

## Linux

Because Linux doesn't have an installer, it requires a bit more work. We need to show the terminal where the Go compiler and other tools are, and we need to set the `GOPATH`, which is a folder that you will be doing Go work in.

Edit a text file named ".profile", which should be in your user directory, and add the following line to the end, replacing `[work area]` with a full path tto a directory you would like to do Go work in. If ".profile" does not exist, create it. A folder called "go" in your user directory is a good choice.

Then, on another new line, add the following to your ".profile" file.

`export PATH=$PATH:/usr/local/go/bin`

# Finished!

If the Go tools are still not available to you in the terminal, try closing that window and opening a fresh terminal window.


  [1]: https://golang.org/dl/
  [2]: https://en.wikipedia.org/wiki/Darwin_(operating_system)

## Example .profile or .bash_profile
```
# This is an example of a .profile or .bash_profile for Linux and Mac systems
export GOPATH=/home/user/go
export PATH=$PATH:/usr/local/go/bin
```

