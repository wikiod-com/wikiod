---
title: "Getting started with operating-system"
slug: "getting-started-with-operating-system"
draft: false
images: []
weight: 1
type: docs
toc: true
---

## Installation or Setup
Detailed instructions on getting operating-system set up or installed.

## install the popular Clonezilla to clone a Mac OS hard drive (basic usage)
Naturally, being able to install hard drive cloning utilities can be an important aspect of installing and maintaining your operating system.

Getting set up with Clonezilla is surprisingly less straightforward than I'd have expected. The wealth of options, while valuable, also makes each part of identifying and loading the software overly convoluted. However, once it is set up, it's very easy to use.

Steps to download for Mac from http://clonezilla.org/downloads/download.php?branch=stable and make the following selections:
1. Select CPU architecture: `amd64`
1. Select file type: `iso`
1. Select repository: `auto`

Steps to format a bootable usb thumb drive
1. Open Applications > Utilities > Disk Utility
1. Insert thumb drive at least 1.5 GB in size
1. Provide a disk name and select format options `OS X Extended (Journaled)` and `GUID Partition Map`

Steps to install Clonezilla on the flash drive.

    $ cd ~/
    $ mv ~/Download/<clonezilla.iso> ~/  # being in the $HOME dir may or may not have mattered
    $ hdiutil convert -format UDRW -o ~/clonezilla.dmg ~/<clonezilla.iso>
    $ diskutil list  # node device node assigned to flash media (e.g. if /dev/disk2, diskN is disk2)
    $ diskutil unmountDisk /dev/<diskN>
    $ sudo dd if=/path/to/downloaded.img of=/dev/diskN  # didn't succeed with rdiskN or with bs=1m as other instructions stipulate
    $ diskutil eject /dev/diskN

Reboot and hold alt key, then follow the on-screen instructions.

Useful references (though keep in mind that many of the details did not work out, where the above did):
- http://drbl.org/faq/fine-print.php?path=./2_System/121_pxe_boot_mac_machine.faq#121_pxe_boot_mac_machine.faq
- http://osxdaily.com/2015/06/05/copy-iso-to-usb-drive-mac-os-x-command/#comment-2000664

Worth noting that once backup is complete, on-screen instructions continue to be useful. It's also easy to overlook the instruction for shutting down: `$ sudo shutdown -h now`

