---
title: "Inventory"
slug: "inventory"
draft: false
images: []
weight: 9925
type: docs
toc: true
---

## Parameters
| Parameter | Explanation |
|-----------|-------------|
| ansible_connection | Connection type to the host. This can be the name of any of ansible’s connection plugins. SSH protocol types are `smart`, `ssh` or `paramiko`. The default is smart. Non-SSH based types are described in the next section. |
| ansible_host | The name of the host to connect to, if different from the alias you wish to give to it. |
| ansible_port | The ssh port number, if not 22 |
| ansible_user | The default ssh user name to use. |
| ansible_ssh_pass | The ssh password to use (this is insecure, we strongly recommend using `--ask-pass` or SSH keys) |
| ansible_ssh_private_key_file | Private key file used by ssh. Useful if using multiple keys and you don’t want to use SSH agent. |
| ansible_ssh_common_args | This setting is always appended to the default command line for **sftp**, **scp**, and **ssh**. Useful to configure a `ProxyCommand` for a certain host (or group). |
| ansible_sftp_extra_args | This setting is always appended to the default **sftp** command line. |
| ansible_scp_extra_args | This setting is always appended to the default **scp** command line. |
| ansible_ssh_extra_args | This setting is always appended to the default **ssh** command line. |
| ansible_ssh_pipelining | Determines whether or not to use SSH pipelining. This can override the `pipelining` setting in `ansible.cfg`. |
| ansible_become | Equivalent to `ansible_sudo` or `ansible_su`, allows to force privilege escalation |
| ansible_become_method | Allows to set privilege escalation method |
| ansible_become_user | Equivalent to `ansible_sudo_user` or `ansible_su_user`, allows to set the user you become through privilege escalation |
| ansible_become_pass | Equivalent to `ansible_sudo_pass` or `ansible_su_pass`, allows you to set the privilege escalation password |
| ansible_shell_type | The shell type of the target system. You should not use this setting unless you have set the `ansible_shell_executable` to a non-Bourne (sh) compatible shell. By default commands are formatted using `sh`-style syntax. Setting this to `csh` or `fish` will cause commands executed on target systems to follow those shell’s syntax instead. |
| ansible_python_interpreter | The target host python path. This is useful for systems with more than one Python or not located at **/usr/bin/python** such as *BSD, or where **/usr/bin/python** is not a 2.X series Python. We do not use the **/usr/bin/env** mechanism as that requires the remote user’s path to be set right and also assumes the **python** executable is named python, where the executable might be named something like **python2.6**. |
| ansible_*_interpreter | Works for anything such as ruby or perl and works just like `ansible_python_interpreter`. This replaces shebang of modules which will run on that host. |
| ansible_shell_executable | This sets the shell the ansible controller will use on the target machine, overrides `executable` in `ansible.cfg` which defaults to **/bin/sh**. You should really only change it if is not possible to use **/bin/sh** (i.e. **/bin/sh** is not installed on the target machine or cannot be run from sudo.). New in version 2.1. |


## Inventory with custom private key
    [targethost]
    192.168.1.1 ansible_user=mrtuovinen ssh_private_key_file=~/.ssh/custom_key

## Inventory, Group Vars, and You
project structure (ansible best practice).

    project/
      group_vars/
         development
      inventory.development
      playbook.yaml

it all starts with inventory.development

    [development]
    dev.fakename.io
    
    [development:vars]
    ansible_host: 192.168.0.1
    ansible_user: dev
    ansible_pass: pass
    ansible_port: 2232

    [api:children]
    development

which lets you link to group_vars. Hold data 'specific' to that environment ...

    ---
    app_name: NewApp_Dev
    app_url: https://dev.fakename.io
    app_key: f2390f23f01233f23f

that lets one run the following playbook AGAINST the inventory file:

    ---
    - name: Install api.
      hosts: api
      gather_facts: true
      sudo: true
      tags:
        - api
      roles:
        - { role: api,         tags: ["api"]         }

with the following runline:

    ansible-playbook playbook.yaml -i  inventory.development

## Inventory with username and password
Inventory is the Ansible way to track all the systems in your infrastructure. Here is a simple inventory file containing a single system and the login credentials for Ansible.

    [targethost]
    192.168.1.1 ansible_user=mrtuovinen ansible_ssh_pass=PassW0rd

## Inventory with custom SSH port
    [targethost]
    192.168.1.1 ansible_user=mrtuovinen ansible_port=2222

## Pass static inventory to ansible-playbook
    ansible-playbook -i path/to/static-inventory-file -l myhost myplaybook.yml

## Pass dynamic inventory to ansible-playbook
    ansible-playbook -i path/to/dynamic-inventory-script.py -l myhost myplaybook.yml

See [dynamic inventory][1] for more details.

[1]: https://www.wikiod.com/ansible/dynamic-inventory

## Hosts file
The host file is used to store connections for Anisble playbooks. There are options to define connection parameters:

`ansible_host` is the hostname or IP address

`ansible_port` is the port the machine uses for SSH

`ansible_user` is the remote user to connect as

`ansible_ssh_pass` if using a password to SSH

`ansible_ssh_private_key_file` if you need to use multiple keys that are specific to hosts

These are the most commonly used options. More can be found in the [Ansible official documentation](http://docs.ansible.com/ansible/intro_inventory.html#list-of-behavioral-inventory-parameters).

Here is an example `hosts` file:

```
# Consolidation of all groups
[hosts:children]
web-servers
offsite
onsite
backup-servers

[web-servers]
server1 ansible_host=192.168.0.1 ansible_port=1600
server2 ansible_host=192.168.0.2 ansible_port=1800

[offsite]
server3 ansible_host=10.160.40.1 ansible_port=22 ansible_user=root
server4 ansible_host=10.160.40.2 ansible_port=4300 ansible_user=root

# You can make groups of groups
[offsite:children]
backup-servers

[onsite]
server5 ansible_host=10.150.70.1 ansible_ssh_pass=password

[backup-servers]
server6 ansible_host=10.160.40.3 ansible_port=77
```

