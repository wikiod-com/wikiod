---
title: "Working with ssh keys"
slug: "working-with-ssh-keys"
draft: false
images: []
weight: 9995
type: docs
toc: true
---

## Convert PPK (PuTTY key) to OpenSSH format
You might receive from your peer private key in PPK format, which seems it does not work in OpenSSH (command-line `ssh`). The client will be asking for the passphrase, because of [OpenSSH bug](https://bugzilla.mindrot.org/show_bug.cgi?id=2522).

    $ ssh -i mykey.ppk example.com
    Enter passphrase for mykey.ppk:

You need to convert the key to OpenSSH format using PuTTYgen (command-line version):

    puttygen mykey.ppk -o mykey.key -O private-openssh

Or in GUI version:

 * Open PuttyGen    
 * Click Load
 * Load your private key
 * Go to **Conversions**->**Export OpenSSH** and export your private key
 * Copy your private key to `~/.ssh/id_rsa`

Source: [SO answer](http://stackoverflow.com/a/2224204/2196426), [Unix SE answer](http://unix.stackexchange.com/a/323000/121504)

