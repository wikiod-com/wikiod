---
title: "Getting started with Secure Shell"
slug: "getting-started-with-secure-shell"
draft: false
images: []
weight: 1
type: docs
toc: true
---

## Creating your SSH key
You can create your ssh key using ssh-keygen, it's a program that is part of the ssh installation. To do so just run it and follow the instructions on screen.

Here's an example:

    $ ssh-keygen
    Generating public/private rsa key pair.

The default directory where you ssh key pair will be saved is inside the `.ssh` folder in your home directory (you can change this by specifying a valid path) and the default name for the keypair is `id_rsa` for the private key and `id_rsa.pub` for the public key:

    Enter file in which to save the key (/home/nasreddine/.ssh/id_rsa): /home/my_folder/my_ssh_key

You can protect your SSH key from unauthorized use by entering a password. This is optional but it's recommended that you use a passphrase. Note that, like with any other command program, when entering your passphrase it will not show anything on screen but it is being recorded:

    Enter passphrase (empty for no passphrase):
    Enter same passphrase again:

Once you enter your passphrase ssh-keygen will generate a key and save it to the path you chose:


    Your identification has been saved in /home/my_folder/my_ssh_key.
    Your public key has been saved in /home/my_folder/my_ssh_key.pub.

We're done. Now our ssh key is ready for use.

[![enter image description here][1]][1]


  [1]: http://i.stack.imgur.com/aj4ci.png

## How to SSH into a machine
In order to login to a user's account on machine with SSH you can use the command `ssh username@ip_address`. It will ask for a password. If you type the correct password, you will be connected to the shell of that user on that machine. Otherwise it will prompt for the password again.

For example

    root@dev10:~# ssh root@10.11.50.3
    root@10.11.50.3's password: 
    Welcome to Ubuntu 16.04 LTS (GNU/Linux 4.4.0-31-generic x86_64)
        
    Last login: Fri Jul 22 18:33:27 2016 from 10.11.50.10
    root@dev2:~# 

If you want to use a specific ssh key to connect to a machine, use `ssh -i /path/to/ssh_secret_key username@host`

When you are connecting to a machine for the very first time, it will ask you to verify the [fingerprint][1] of the target machine. This is a security mechanism for avoiding a [man-in-the-middle attack][2]. You can see the fingerprint of the target machine by issuing this command in the target machine.

    ssh-keygen -l -E md5 -f /etc/ssh/ssh_host_ecdsa_key.pub

   You can type "yes" if both are same. It will proceed to password prompt.

Example:

    root@dev10:~# ssh root@10.11.50.3
    The authenticity of host '10.11.50.3 (10.11.50.3)' can't be established.
    ECDSA key fingerprint is dd:a3:de:cd:5b:01:cd:0b:b6:bc:b3:09:c2:c8:1a:68.
    Are you sure you want to continue connecting (yes/no)? yes
    Warning: Permanently added '10.11.50.3' (ECDSA) to the list of known hosts.
    root@10.11.50.3's password: 
    
    Last login: Fri Jul 22 17:45:09 2016 from 10.11.1.71
    root@dev2:~# 


  [1]: https://en.wikipedia.org/wiki/Public_key_fingerprint
  [2]: https://en.wikipedia.org/wiki/Man-in-the-middle_attack

## Installation or Setup
Free version of SSH protocol implementation, OpenSSH is available in all the Linux distributions. It consists of the server and client packages.

# Installation

## On Debian-based Linux, you can install `openssh` using

    # apt-get install openssh-server openssh-client

## On RHEL/CentOS you need to use `yum`:

    # yum install openssh-server openssh-clients

Current Fedora is using `dnf` instead of `yum`.

## On Arch Linux, use pacman:

    # pacman -S openssh

## On OSX, the `openssh` should be already installed.

If you want to use more recent version, you need to install the `openssh` from brew:

    # brew install openssh --with-brewed-openssl --with-keychain-support

## TODO Instructions for Windows

# Setup

The `openssh` client does not need any special setup and is ready to use just after installation. You can try that running `ssh remote`, where the `remote` is the remote host running `ssh` server.

The `openssh` server is usually started after the installation and default setup is applied. If not, you can start it on `systemd` based systems using

On Debian-based Linux with systemd:

    # systemctl start ssh

On RHEL/CentOS/Fedora and Arch Linux:

    # systemctl start sshd

Or on upstart systems using

    # service sshd start

## Configuration

The `openssh` have configuration files under `/etc/ssh/`. The client is also reading client configuration in `~/.ssh/config`. The server is using a file `sshd_config`, which contains most of the default values and contains simple key-value pairs. Example:

    Protocol 2
    PasswordAuthentication yes
    ChallengeResponseAuthentication no
    UsePAM yes
    AcceptEnv LANG LC_CTYPE LC_NUMERIC LC_TIME LC_COLLATE LC_MONETARY
    X11Forwarding yes
    Subsystem    sftp    /usr/libexec/openssh/sftp-server



## Adding your public key to the list of server user's authorized keys
In order to `ssh` into a server your identity's public key has to be added to the list of trusted keys. Most commonly this is done per-user:

    ssh-copy-id -i ~/.ssh/<identity>.pub <user>@<hostname>

Which can be also done manually:

    cat ~/.ssh/<identity>.pub | ssh <user>@<hostname> 'cat >> ~/.ssh/authorized_keys'

After doing that you should be able to log in without need to provide user's password when passing the identity file to the `ssh` call.

## Config File
OpenSSH config files are used for configuration that should be applied every time the ssh client is run. Most command line options are possible to put in the config files.

OpenSSH uses configuration from the following sources in order:

 1. Command line options
 2. User's configuration file `~/.ssh/config`
 3. System wide configuration file `/etc/ssh/ssh_config`

Configuration options are listed one by one in the config files.
    
    # This is a comment.

    # Parameter can be specified like this, separated with white space.
    StrictHostKeyChecking ask

    # Or parameter key and value may be separated with white space and =.  
    ForwardX11 = yes

    # The parameter value can be quoted if it contains white space.    
    IdentityFile "/file system/path with/white space"

The full list of possible config parameters is available [here][1].

One of the most useful features of the config file is that it can be sectioned based on host name or address. In this way you can have different configurations for different hosts.
    
    # Based on host name.    

    Host host1.domain.com
        User user1

    Host host2.domain.com
        User user2
    
    # Or wildcard matching name or ip.

    Host *elastic-cloud.com 10.201.4.?
        User user3

  [1]: http://man.openbsd.org/ssh_config

## Connecting from script using password
When you really need to script `ssh` connection, piping the password into the `ssh` command does not work (`echo passw0rd | ssh host`). It is because the password is not read from standard input, but directly from TTY (teleprinter, teletypewriter, Teletype for historical reasons).

But there is `sshpass` tool which works around this problem. It can read the password from parameter, file or environment variable. But note that none of these options does not satisfy the security requirements for a passwords!

    $ sshpass -p passw0rd ssh host
    $ sshpass -f /secret/filename ssh host
    $ SSHPASS=passw0rd sshpass -e ssh host

The command line options can be seen by other users in `ps` (during runtime it is masked, but not during start time and you can't rely on it):

    ... 23624  6216 pts/5    Ss   Aug30   0:00  \_ /bin/bash
    ... 12812  1988 pts/5    S+   08:50   0:00  |   \_ sshpass -p passw0rd ssh host
    ... 45008  5796 pts/15   Ss+  08:50   0:00  |       \_ ssh host

Note, that environemnet variables of a process are also accessible by other processes on the system using `/proc/PID/environ` file.

Finally, storing the password in the file might look like the best possible idea, but still using keys as described in the other examples is preferred way to use `ssh`.

## SSH-Agent
If you have set a long passphrase and do not wish to keep entering it every time you want to connect to the server, you can use SSH-Agent to store your passphrase while you are logged in on your computer.

Start the ssh-agent in the background:

    eval "$(ssh-agent -s)"
    # Agent pid 59566

And add your key to the ssh-agent, you will be prompted to enter your passphrase:

    ssh-add ~/.ssh/matrix.ac
    # Enter passphrase for /home/osaka/.ssh/matrix.ac: 
    # Identity added: /home/osaka/.ssh/matrix.ac (/home/osaka/.ssh/matrix.ac)

Done. Now connect with ```ssh user@matrix.ac``` and you should not have to enter your passphrase. You can extend this by using programs like gnome-keyring/seahorse, keychain and other key managers.

