---
title: "Permissions"
slug: "permissions"
draft: false
images: []
weight: 9982
type: docs
toc: true
---

In unix each files has certain permissions like read, write and execute. A user can manipulate the permissions of a file using 'chmod' command.

In UNIX, there are three permissions used to grant a certain level of access to a file or folder.

For files:

* **Read**: Allow the user/group/others to read a file.
* **Write**: Allow the user/group/others to modify a file.
* **Execute**: Allow the user/group/others to execute (or run) a file.

These are slightly changed for directories:

* **Read**: Allow the user/group/others to list the names of files in a directory.
* **Write**: Allow the user/group/others to create, delete and rename files in a directory.
* **Execute**: Allow the user/group/others to access file metadata and contents for a directory.

These permissions can be represented using the letters "r" for read, "w" for write, and "x" for execute. They can also be represented numerically: 4 for read, 2 for write and 1 for execute.

## Change a file's permissions
    > chmod 644 example.txt
    > ls -l example.txt
    -rw-r--r--  1 owner ogroup 57 Jul  3 10:13  example.txt


The above command changes the file permissions to allow the file owner to read and write to a file. It also allows users in the owner's group and other users in the system to read the file.

## Understanding Permissions
Let's say there is a file we would like to execute, a bash script named `add.sh`, for example. Typing `./add.sh` however, yields a permission error. Getting the permissions is a simple process. 

To determine the permissions a file has, type:

`ls -l filename`, or, in our case, `ls -l ./add.sh`

This prints the following to the console:

`-r--r--r-- 1 username groupname 0 Jan  4 12:00 add.sh`

Let's stop and understand what this means. There are three different types for permissions: owner, group, others. Distinct permissions apply to each permission type.

There are also three permission actions, which more broadly also describe what exactly a user can do to a file. These are: (read: r, write: w, execute: x).

So, back up to that string of dashes and r's. Each permission group has three potential abilities. The groups are listed in the order `owner-group-others` and the actions as `read-write-execute`.

But wait, that means there's an extra character at the beginning of the string. This is actually the file descriptor character. We can see there is a `-` there, but other characters exist for things like directories`(d)`, sockets`(s)`, symbolic link`(l)` etc.

This leaves us with essentially this information: `a file where owner, group, and others have read permissions. No other permissions granted.`

Let's alter this to allow the owner to also write and execute the file. Note: Depending on the permissions, it may be necessary to prepend `sudo` to this command.

```
chmod 744 add.sh
ls -l add.sh
```
Prints out
```
-rwxr--r-- 1 username groupname 0 Month time add.sh
```

Now, the owner of the file can execute the file by typing
```
./add.sh
```

## CHMOD calculation
CHMOD Calculation

CHMOD is binary.
_ / _ _ _ / _ _ _ / _ _ _
= _/4+2+1/4+2+1/4+2+1 = 777
= _/rwx/rwx/rwx = 777
Therefore _rwx = _/4+2+1 = 7 

D / _ _ _ / _ _ _ / _ _ _ (‘D’ = directory, another use is L = Link)

So e.g.
_rwxr_xr_x = _/rwx/rx/r_x = 755



## CHOWN
To change own:group you use command chown user:group

e.g. chown owner:group or if owner and group are same you can use chown owner: (because linus assumes owner:group are same). 


