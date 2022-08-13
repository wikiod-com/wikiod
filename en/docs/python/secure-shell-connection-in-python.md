---
title: "Secure Shell Connection in Python"
slug: "secure-shell-connection-in-python"
draft: false
images: []
weight: 9978
type: docs
toc: true
---

## Parameters
| Parameter| Usage|
| ------ | ------ |
| hostname| This parameter tells the host to which the connection needs to be established|
| username | username required to access the host|
|port|host port
|password|password for the account|

## ssh connection


    from paramiko import client
    ssh = client.SSHClient() # create a new SSHClient object
    ssh.set_missing_host_key_policy(paramiko.AutoAddPolicy()) #auto-accept unknown host keys
    ssh.connect(hostname, username=username, port=port, password=password) #connect with a host
    stdin, stdout, stderr = ssh.exec_command(command) # submit a command to ssh
    print stdout.channel.recv_exit_status() #tells the status  1 - job failed


