---
title: "Managing remote files through ssh"
slug: "managing-remote-files-through-ssh"
draft: false
images: []
weight: 9997
type: docs
toc: true
---

this topic should describe various ways of using ssh as a secure way to manage files on remote machines

## Mounting remote directory through ssh
You can mount remote directory through ssh by using sshfs. Sshfs does not come as a default on Ubuntu, so you need to install it first by using `sudo     apt-get install sshfs`.
You can then mount the remote directory to your local machine like this

    sshfs user@xxx.xxx.xxx.xxx:/remotedir /localdir

Note that the localdir needs to be present before trying to mount the remotedir. In case that the localdir is not empty, sshfs will complain and abort. You can force it to mount in the non empty dir by using nonempty option like this

    sshfs -o nonempty user@xxx.xxx.xxx.xxx:/remotedir /localdir

Once you umount the localdir, local files will become visible again.





## Moving files between servers through ssh
One way to move files between servers is by using the `scp` command. Secure copy command utilizes ssh to transfer data.
The simples example for copying a file from local to remote server is

    scp /localdir/localfile user@xxx.xxx.xxx.xxx:/remotedir/remotefile

Similarily, to copy file from a remote to local server would be

    scp user@xxx.xxx.xxx.xxx:/remotedir/remotefile /localdir/localfile 

