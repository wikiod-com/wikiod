---
title: "Setup Perforce login details in Windows cmd"
slug: "setup-perforce-login-details-in-windows-cmd"
draft: false
images: []
weight: 9997
type: docs
toc: true
---

After installing perforce and setup your workspace through p4v, you could set up your workspace in Windows cmd.



## Setup Perforce login details in Windows cmd


After installing perforce and setup your workspace through p4v, you could set up your workspace in Windows cmd.

The command you need is:

    p4 set

<b><i>Set your server</i></b>

    p4 set P4PORT= xx.xxx.xx.xxx:xxxxx

<b><i>Set your user name</i></b>

    p4 set P4USER=username

<b><i>Set your password</i></b>

In terms of setting your password, you could just set your password in plain text by the following command:

    p4 set P4PASSWD=yourpasswd

If you want to secure your password, you could download the MD5 encryptor to encrypt your password.

    p4 set P4PASSWD=hashedpasswd

<b><i>Set you client (workspace)</i></b>

In order to select your workspace, you could use the following command:

    p4 set P4CLIENT=nameofworkspace
The name of your workspace could look like: username_hostmachinename_numbers.

<b><i>Set your ignore file</i></b>

You can set up your ignore file so that when adding your project to perforce, some files (building results) could be automatically ignored.

    p4 set P4IGNORE= filepath



