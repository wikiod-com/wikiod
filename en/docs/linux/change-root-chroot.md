---
title: "Change root (chroot)"
slug: "change-root-chroot"
draft: false
images: []
weight: 9990
type: docs
toc: true
---

Change root (chroot) is an operation that changes the apparent root directory for the current running process and their children. A program that is run in such a modified environment cannot access files and commands outside that environmental directory tree.

## Syntax
- chroot [destination path] [shell or command]

## Manually changing root in a directory
 1. Ensure you met all requirements, as per [Requirements]()
 
 2. Mount the temporary API filesystems:

        cd /location/of/new/root
        mount -t proc proc proc/
        mount --rbind /sys sys/
        mount --rbind /dev dev/
        mount --rbind /run run/ (optionally)

 
3. If you need to use an internet connection in the chroot environment, copy over the DNS details:

       cp /etc/resolv.conf etc/resolv.conf

4. Change root into /location/of/new/root, specifying the shell (`/bin/bash` in this example):

       chroot /location/of/new/root /bin/bash

5. After chrooting it may be necessary to load the local bash configuration:

       source /etc/profile
       source ~/.bashrc

6. Optionally, create a unique prompt to be able to differentiate your chroot environment:

       export PS1="(chroot) $PS1"

7. When finished with the chroot, you can exit it via:

       exit

8. Unmount the temporary file systems:

       cd /
       umount --recursive /location/of/new/root

## Requirements
- root privileges
- another working Linux environment,such as Live CD boot or an existing distribution
- matching environment architectures of `chroot` source and destination (check current environment architecture with `uname -m`)
- kernel modules which you may need in `chroot` environment must be loaded (for example, with `modprobe`)

## Reasons to use chroot
Changing root is commonly done for performing system maintenance on systems where booting and/or logging in is no longer possible.

Common examples are:

- reinstalling the bootloader
- rebuilding the initramfs image
- upgrading or downgrading packages
- resetting a forgotten password
- building software in a clean root environment

