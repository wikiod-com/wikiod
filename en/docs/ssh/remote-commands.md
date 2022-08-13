---
title: "Remote commands"
slug: "remote-commands"
draft: false
images: []
weight: 9991
type: docs
toc: true
---

## Interactive and screen-based commands
Many commands and programs in the remote side are screen-based (e.g. `mc`) or they need to ask password (e.g. `sudo`), to be able to run these kind of programs you can use option `-t`.

    ssh -t alice@example.com sudo ls /

> [sudo] password for alice:
>
> bin root dev etc home lib mnt opt proc root run usr var



## Hello World
To send a remote command via SSH (the SSH server needs to be running on the remote host), you can simply write the command after _user@machine_.

    ssh alice@example.com echo 'Hello World'

> Hello World

It returns back the output to the sender, executed on the remote host.

