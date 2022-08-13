---
title: "Basic Terminal commands"
slug: "basic-terminal-commands"
draft: false
images: []
weight: 9928
type: docs
toc: true
---

**How to exract tar.gz/bz2/tbz files :** 

**If Your File Extension is .tar.gz (or .tgz) use this command**
    
     tar xvzf file.tar.gz

  * x: This tells tar to extract the files.
   
  *  v: This option will list all of the files one by one in the archive.
   The “v” stands for “verbose.”
   
   * z: The z option is very important and tells the tar command to
   uncompress the file (gzip).
   
   * f: This options tells tar that you are going to give it a file name
   to work with.

** If Your File Extension is .tar.bz2 (or .tbz)**

The major difference between these two is that the z option has been replaced by the j option.

   * j: This will decompress a bzip2 file.


## !!
(read as bangbang) is a shortcut to repeat the last command entered in console. It is especially useful to run previous command with some changes

    adduser tom

> adduser: Only root may add a user or group to the system.

Oh snap, what now? 
Well you could retype the command with sudo in front or you could try 

    sudo !!

> equivalent to sudo "previous command entered"

This is especially useful if the command you just typed is especially long. 

It can also be used to change a part of previous commands

    cd path/to/wrong/directory
With

    !!:s/wrong/right/
Will do

    cd path/to/right/directory



## Changing password of current user
To change password of current user just type:
 
    sudo passwd

It will ask you to enter your current password:
 
    [sudo] password for <user>:

And then you will be asked to enter new password:

    Enter new UNIX password:

And finally you will be asked to re-enter your password:
 
    Retype new UNIX password:

Note: [By default][1], the keys you press at a command-line password prompt are not displayed at all. They are, however, still registered.


  [1]: https://coderwall.com/p/3vywiw/make-password-asterisks-visible-in-your-terminal

## List files and folders
To list files and folders inside current directory, we use `ls` command:

    user@host:/$ ls
    bin  boot  cdrom  dev  etc  home  initrd.img  lib  lib64  lost+found  
    media  mnt  opt  proc  root  run  sbin  srv  sys  tmp  usr  var  vmlinuz

`ls` prints folder structure in simple view, color coded by type. The Ubuntu default colors for `ls` are:

**blue** for directories, **green** for executable files, **sky blue** for linked files, **yellow with a black background** for devices, **pink** for image files, and **red** for archive files.

`ls -la` will print folder structure with additional info:

    user@host:/$ ls -la
    total 104
    drwxr-xr-x  23 root root  4096 јул 25 12:40 .
    drwxr-xr-x  23 root root  4096 јул 25 12:40 ..
    drwxr-xr-x   2 root root  4096 јул 25 12:42 bin
    drwxr-xr-x   4 root root  4096 јул 25 12:42 boot
    drwxrwxr-x   2 root root  4096 јул 25 12:38 cdrom
    drwxr-xr-x  16 root root  4300 јул 30 12:18 dev
    drwxr-xr-x 134 root root 12288 јул 30 12:18 etc
    drwxr-xr-x   5 root root  4096 јул 25 12:50 home
    lrwxrwxrwx   1 root root    33 јул 25 12:40 initrd.img -> boot/initrd.img-3.19.0-39-generic
    drwxr-xr-x  23 root root  4096 јул 25 12:42 lib
    drwxr-xr-x   2 root root  4096 дец  9  2015 lib64
    drwx------   2 root root 16384 јул 25 12:32 lost+found
    drwxr-xr-x   3 root root  4096 јул 25 14:56 media
    drwxr-xr-x   2 root root  4096 апр 11  2014 mnt
    drwxr-xr-x   3 root root  4096 јул 25 13:37 opt
    dr-xr-xr-x 227 root root     0 јул 30 12:18 proc
    drwx------   2 root root  4096 јул 25 13:06 root
    drwxr-xr-x  23 root root   780 јул 31 14:30 run
    drwxr-xr-x   2 root root 12288 јул 25 12:46 sbin
    drwxr-xr-x   2 root root  4096 дец  8  2015 srv
    dr-xr-xr-x  13 root root     0 јул 30 12:18 sys
    drwxrwxrwt   8 root root  4096 јул 31 16:05 tmp
    drwxr-xr-x  10 root root  4096 дец  8  2015 usr
    drwxr-xr-x  13 root root  4096 дец  9  2015 var
    lrwxrwxrwx   1 root root    30 јул 25 12:40 vmlinuz -> boot/vmlinuz-3.19.0-39-generic

 

Another shortcut for `ls -la` is `ll`. However, this is not a builtin command.  Rather its an `alias` common in ubuntu systems, in full its `ls -laF`. The alias will give you the same output as  `ls -la`, but with additional slash (`/`) at the end of each folder, to help you with easier folder identification.

The `ll` alias can be viewed in full by typing `alias ll`. As illustrated below. If the alias is not set then the command will give an error.
    
    vagrant@host ~ ->>
    08:05 AM Mon Sep 12$ alias ll
     alias ll='ls -alF'

## Adding new user
`adduser` command adds a user to the system. In order to add a new user type:

    sudo adduser <user_name>

example: 

    sudo adduser tom

After typing the above command, you will be prompted to enter details about the new user, such as new password, user Full name, etc.

Below is the information that user will be asked to fill in order to add a new user:

    Enter new UNIX password:
    Retype new UNIX password:
    passwd: password updated successfully
    Changing the user information for tom
    Enter the new value, or press ENTER for the default
        Full Name []: Test User
        Room Number []:
        Work Phone []:
        Home Phone []:
        Other []:
    Is the information correct? [Y/n] y

## Restart Ubuntu
You can restart ubuntu from command line. Below is example of restarting ubuntu immediately.

     sudo reboot

You need to have sudo privilege in order to use this command.

Another commands with same results are `sudo shutdown -r now` and `sudo init 6`.

## Install new software
## APT and APT-GET ##

Easiest and fastest way is with `apt-get` command. This command may be considered as lower-level and "back-end", and support other APT-based tools. There are no fancy loaders, only basic progress info. This is fastest way for installing apps.

Usage:

> sudo apt-get install deluge openssh-server

This command will install two new apps: deluge and openssh-server. You can install as many apps as you want in only one line of commands.

"Fancy" way of the same process is wits `apt`:
> sudo apt install deluge openssh-server

Result is the same, but interaction with users is different from previous command. `apt` is designed for end-users (human) and it's output may be changed between versions.

Both commands will handle dependencies automatically.

    user@host:~$ sudo apt install vlc
    Reading package lists... Done
    Building dependency tree       
    Reading state information... Done
    The following packages were automatically installed and are no longer required:
      libtimezonemap1 sbsigntool
    Use 'apt-get autoremove' to remove them.
    The following extra packages will be installed:
      libbasicusageenvironment0 libcddb2 libcrystalhd3 libdvbpsi8 libebml4
      libfreerdp1 libgnutls28 libgroupsock1 libhogweed2 libiso9660-8
      liblivemedia23 libmatroska6 libproxy-tools libresid-builder0c2a libsidplay2
      libssh2-1 libtar0 libupnp6 libusageenvironment1 libva-x11-1 libvcdinfo0
      libvlc5 libvlccore7 libxcb-composite0 libxcb-keysyms1 libxcb-randr0
      libxcb-xv0 vlc-data vlc-nox vlc-plugin-notify vlc-plugin-pulse
    Suggested packages:
      firmware-crystalhd freerdp-x11 gnutls-bin videolan-doc
    Recommended packages:
      libdvdcss2
    The following NEW packages will be installed:
      libbasicusageenvironment0 libcddb2 libcrystalhd3 libdvbpsi8 libebml4
      libfreerdp1 libgnutls28 libgroupsock1 libhogweed2 libiso9660-8
      liblivemedia23 libmatroska6 libproxy-tools libresid-builder0c2a libsidplay2
      libssh2-1 libtar0 libupnp6 libusageenvironment1 libva-x11-1 libvcdinfo0
      libvlc5 libvlccore7 libxcb-composite0 libxcb-keysyms1 libxcb-randr0
      libxcb-xv0 vlc vlc-data vlc-nox vlc-plugin-notify vlc-plugin-pulse
    0 upgraded, 32 newly installed, 0 to remove and 308 not upgraded.
    Need to get 10,5 MB of archives.
    After this operation, 51,7 MB of additional disk space will be used.
    Do you want to continue? [Y/n] 

## DPKG ##

`dpkg` stands for Debian Package. This is basic, low-level package installer for Debian and other Debian derivatives. `dpkg` have many options, but we are interested in `-i`, which stands for `install`.

> sudo dpkg -i google-chrome-stable.deb

You can install `.deb` packages with `dpkg`.

If you get an error while installing `.deb` package, in most cases you don't have some dependencies. To get rid of this error and install app correctly, run `sudo apt-get -f install` without any other parameter. This will search for dependencies and install them before `.deb`. Installation will continue and app will be installed. 

## Reading a text file
Using Ubuntu you have different ways to read a text file, all similar but useful in different context.

**cat**

This is the simplest way to read a text file; it simply output the file content inside the terminal. Be careful: if the file is huge, it could take some time to complete the printing process! If you need to stop it, you can always press `CTRL+C`. Note that if you need to navigate through the document, you need to scroll the terminal output.

    cat file_name.txt

**more**

An improved version of `cat`. If your file is longer than the display of the terminal, you can simply type

    more file_name.txt

and you'll have a downwards scrolling display of text, in which you can move down by pressing `ENTER`. 

**less**

This is the `more` command with some enhancements, and is typically a better choice than `cat` for reading medium to big documents. It opens file showing them from the beginning, allowing to scroll up/down/right/left using arrows.

    less file_name.txt

Once the document is open, you can type some commands to enable some useful features, such as:

 - `q`: close immediately the opened file.
 - `/word`: search 'word' inside the document. Pressing `n` you can go to the following occurrence of 'word'.
 - `ENTER`: scrolls down of a single line.
 - `r`: repaints the file content, if it's changing while reading.

This is the best choice for reading medium to big documents.

**tail**

This software shows only the last part of the file. It's useful if you need to read just a few lines in the end of a very big document.

    tail file_name.txt
The above command will show last 10 lines(default) of the file. To read last 3 lines, we need to write:

    tail -3 file_name.txt

There's another use case where this command is extremely useful. Imagine to have a empty document, that is filled while you are watching it; if you want to see new lines in real time while they are written to the file without reopening it, just open the file with the `-f` option. It's really useful if you are watching some logs, for example.

    tail -f file_name.txt

This is the best choice for reading growing documents.

**head**

This command does the opposite task of `tail`. For example the following command will show the first 15 lines of the file `file_name.txt`.

    head -15 file_name.txt

**tailf**

This is an alternative for `tail -f filename` .It follows the file changes as they occur and shows you the output.

**vim**

Some of us like vi, some others like vim. This is not just for reading files, you can also edit them! Now let's see only some features that regards reading documents. Please note that vim offers syntax highlighting.

    vim file_name.txt

Once the file is opened, be careful! Don't start typing, or you will mess everything up! In fact, even if you can see the cursor, you have to press `i` to start typing and `ESC` after you finished typing. By the way, now I'm going to showing you some useful commands that concern reading (not writing):

 - `:` : you need to type colon before inserting each of the following commands!
 - `q!` : exit from the file without asking a confirm. It's the same as `q` if you didn't edit the text.
 - `/word` : search for 'word' inside the document.
 - `230` : goes to line '230'.

Tip: a shortcut to insert a colon and then type `wq!` for writing edits to file and quit without asking a confirm, you can hold down `SHIFT` and press twice `z`.

This is the best choice for reading code files.

## Search the exact name of a packet for apt-get
If you know you need a packet \<packet>, but you don't know the exact name \<packet-exact-name>, instead of searching with Google try with the following:

    sudo apt-get update
    apt-cache search <packet>

This will return you a list of packets name with a description. Once you have identified the exact packet name \<packet-exact-name> install it normally:

    sudo apt-get install <packet-exact-name>

## Limit output to lines with the desired string
If you are running a command that returns hundreds of lines, but your interest is just on the lines that contains the word \<word>, you should definitely use grep! For example try running:

    ifconfig -a

Check the output, that could be only a few lines or quite long, if you have a server with multiple network interfaces. To show (for example purpose) only the lines that contain 'HWaddr', try using grep:

    ifconfig -a | grep HWaddr

Now the output should be much shorter! Generally speaking, you could use grep in the following way:

    <command-with-output> | grep <word>

## Compress files and folders using the tar command
Use the tar (tape archive) command to compress your files and folders. It is similar to creating .ZIP files in Windows environment.

    Syntax: tar -zcvf <output tar file> <source file> 

    Example: tar -zcvf outputfile.tar.gz source file

Here’s what those switches actually mean:

**-c**: Create an archive.<br>
**-z**: Compress the archive with gzip. <br>
**-v**: Display progress in the terminal while creating the archive, also known as “verbose” mode. The v is always optional in these commands, but it’s helpful.<br>
**-f**: Allows you to specify the filename of the archive.

