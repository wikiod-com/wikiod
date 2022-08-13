---
title: "Become (Privilege Escalation)"
slug: "become-privilege-escalation"
draft: false
images: []
weight: 9995
type: docs
toc: true
---

Often you need to execute commands under a different user or get *root* privileges. Those options allow you to **become** another user in the guest system.

## Syntax
- `become`: can be set to true or yes and triggers the user escalation settings.
- `become_user`: set to the desired user in the remote host.
- `become_method`: specify the command used to make login and change user.
- `become_flags`: change login parameters. Mostly used when you want to change to a system user without shell privileges.

## Only in a task
    - name: Run script as foo user
      command: bash.sh
      become: true
      become_user: foo

## Run all role tasks as root
    - hosts: all
      become: true
    
    - name: Start apache
      service: apache2
      state: started

## Run a role as root
    - hosts: all
      roles:
        - { role: myrole, become: yes }
        - myrole2

