---
title: "Administering SVN"
slug: "administering-svn"
draft: false
images: []
weight: 9991
type: docs
toc: true
---

## Create new user
To add user, use following command

`htpasswd /etc/subversion/passwd user_name`

Specify `user_name` with the username you wish to add in above command. It will prompt to provide password for the user. 

If you are creating very first user, you need to add `–c` switch in above command, which will create the file.

`htpasswd -c /etc/subversion/passwd user_name`

You can check existence of the file or list of configured users using following command
`cat /etc/subversion/passwd`

You might need to execute above commands as super user.

## Create user groups
Groups can be defined in `/etc/subversion/svn_access_control` file.

Create/edit the file using following command

`nano /etc/subversion/svn_access_control`

Use syntax specified as below to define groups and assign members.

```
[groups]
groupname = <list of users, comma separated>
```

e.g.
```
[groups]
myproject-dev = john, peter
myproject-support = maria, cristine
```

Above example will create two groups named `myproject-dev` and `myproject-support`. It will add users `john` and `peter` to group `myproject-dev` and users `maria` and `cristine` to group `myproject-support`.

Groups can then be used to manage repository access

## Managing repository permissions
Access specifications for subversion repositories is specified `etc/subversion/svn_access_control` file

Create/edit the file using following command

`nano /etc/subversion/svn_access_control`

Use following syntax to configure access permissions for repositories to group/members

```
[Repository:<Path>]
@groupname = r/rw
User = r
```

e.g. 

```
[myproject:/]
@myproject-dev = rw
@myproject-support = r
jack = r
            
[myproject:/branches/support]
@myproject-support = rw
patrick = r
```

Above example configuration will grant read-write access to entire `myproject` repository to users belonging to group `myproject-dev`, while read-only access is granted to users belonging to group `myproject-support` and specific user `jack`. 
**Note that, group names are preceded by `@`.**

Similarly, it will assign read-write access to `support` branch of `myproject` repository to all users belonging to `myproject-support` and read-only access to `patrick`.

## Creating A New Repo
New repository can be created with two different options:

# 1. Using command line

Execute following command. It will create a directory for the repository, but parent path has to be present. i.e. in the following example, `/var/svn` should already be there, while it will create `my_repository` directory.

```bash
svnadmin create /var/svn/my_repository
```

If you are using [TortoiseSVN](https://tortoisesvn.net/), you can use GUI to create repo.

 1. Open the directory where you want to create a new repository.
 2. Right click on the folder and select `TortoiseSVN -> Create Repository here...`
 3. A repository is then created inside the selected folder. Don't edit those files yourself! If you get any errors make sure that the folder is empty and not write protected.

