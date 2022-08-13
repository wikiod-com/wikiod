---
title: "Compiling the Linux kernel"
slug: "compiling-the-linux-kernel"
draft: false
images: []
weight: 9859
type: docs
toc: true
---

## Compilation of Linux Kernel on Ubuntu
> **Warning:** be sure you have at least 15 GB of free disk space.

Compilation in Ubuntu >=13.04
---

**Option A) Use Git**

Use git if you want to stay in sync with the latest Ubuntu kernel source. Detailed instructions can be found in the Kernel Git Guide. 
The git repository does not include necessary control files, so you must build them by:

    fakeroot debian/rules clean

**Option B) Download the source archive**

Download the source archive - This is for users who want to rebuild the standard Ubuntu packages with additional patches. 
Use a follow command to install the build dependencies and extract the source (to the current directory):

1. Install the following packages:

       sudo apt-get build-dep linux-image-`uname -r`

**Option C) Download the source package and build**

  This is for users who want to modify, or play around with, the Ubuntu-patched kernel source. 


1. Retrieve the latest kernel source from [kernel.org](https://www.kernel.org/).

2. Extract the archive to a directory and `cd` into it:

       tar xf linux-*.tar.xz
       cd linux-*

3. Build the ncurses configuration interface:

       make menuconfig

4. To accept the default configuration, press <kbd>→</kbd> to highlight `< Exit >` and then <kbd>Return</kbd>.

5. Press <kbd>Return</kbd> again to save the configuration.

6. Use `make` to build the kernel:

       make

   Note that you can use the <code>-j<em>n</em></code> flag to compile files in parallel and take advantage of multiple cores.

The compressed kernel image can be found at <code>arch/[arch]/boot/bzImage</code>, where `[arch]` is equal to `uname -a`.

