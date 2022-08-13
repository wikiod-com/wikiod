---
title: "Ansible Architecture"
slug: "ansible-architecture"
draft: false
images: []
weight: 9990
type: docs
toc: true
---

## Understanding Ansible Architecture
[![Ansible Architecture][1]][1]


  [1]: https://i.stack.imgur.com/Mm0ci.png

The idea is to have one or more control machines from where you can issue ad-hoc commands to remote machines (via `ansible` tool) or run a sequenced instruction set via playbooks (via `ansible-playbook` tool). 

Basically, we use Ansible control machine, this will typically be your desktop, laptop or server. Then from there, you use Ansible to push configuration changes out, via ssh.

The host inventory file determines the target machines where these plays will be executed. The Ansible configuration file can be customized to reflect the settings in your environment.



