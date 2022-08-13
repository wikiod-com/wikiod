---
title: "Handling NFS Mount"
slug: "handling-nfs-mount"
draft: false
images: []
weight: 9976
type: docs
toc: true
---

NFS is the most common way to share disk between computers in linux. It allows user on a client computer to access files over a network much like local storage is accessed. Here we see how to configure Puppet to manage mounting and serving NFS drives.

## Parameters
| Parameter | Details |
| ------ | ------ |
| name   | The path to local directory in which the remote drive should be mounted. |
| device | Remote server address and directory path on remote server, separated by `:` |
| atboot | Whether this drive should be mounted while booting. Enabling makes drives available sooner, but may cause delayed boot in case of network or mounting problem. |
| pass | Fsck order is to tell fsck what order to check the file systems, if set to "0" file system is ignored. Usually NFS drives need not be checked in clients, so "0" is a suitable option. |

* Mount target directory should exists on the client.
* [Mount resource type documentation](https://docs.puppet.com/puppet/latest/reference/type.html#mount)
* [fstab description and option details](https://help.ubuntu.com/community/Fstab)

## Mounting a remote NFS drive
    mount { '/path/to/local/folder':
      ensure  => 'mounted',
      atboot  => false,
      device  => 'server-ip-or-domain:/path/to/server/folder',
      fstype  => 'nfs',
      options => 'defaults',
      pass    => 0,
    }

