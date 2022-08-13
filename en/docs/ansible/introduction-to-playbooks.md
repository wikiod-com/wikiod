---
title: "Introduction to playbooks"
slug: "introduction-to-playbooks"
draft: false
images: []
weight: 9946
type: docs
toc: true
---

## Overview
In Ansible, a playbook is is a YAML file containing the definition of how a server should look. In a playbook you define what actions Ansible should take to get the server in the state you want. Only what you define gets done.

This is a basic Ansible playbook that installs git on every host belonging to the `web` group:

    ---
    - name: Git installation
      hosts: web
      remote_user: root
      tasks: 
        - name: Install Git
          apt: name=git state=present

## Playbook's structure
The format of a playbook is quite straightforward, but strict in terms of spacing and layout. A playbook consists of plays. A play is a combination of targets hosts and the tasks we want to apply on these hosts, so a drawing of a playbook is this:

[![Pictorial representation of an Ansible playbook][1]][1]


  [1]: http://i.stack.imgur.com/5nHuP.jpg

To execute this playbook, we simply run:

    ansible-playbook -i hosts my_playbook.yml


## Play's structure
Here’s a simple play:

    - name: Configure webserver with git
      hosts: webserver
      become: true
      vars:
        package: git
      tasks:
        - name: install git
          apt: name={{ package }} state=present

As we said earlier, every play must contain:

• A set of hosts to configure

• A list of tasks to be executed on those hosts

Think of a play as the thing that connects hosts to tasks.
In addition to specifying hosts and tasks, plays also support a number of optional settings. Two common ones are:

- `name`: a comment that describes what the play is about. Ansible will print this out when
the play starts to run
- `vars`: a list of variables and values

## Tags
Play contains several tasks, which can be tagged:


    - name: Install applications
      hosts: all
      become: true
      tasks:
        - name: Install vim
          apt: name=vim state=present
          tags:
            - vim
        - name: Install screen 
          apt: name=screen state=present
          tags:
            - screen

Task with tag 'vim' will run when 'vim' is specified in tags. You can specify as many tags as you want. It is useful to use tags like 'install' or 'config'. Then you can run playbook with specifying tags or skip-tags. For 

    ansible-playbook my_playbook.yml --tags "tag1,tag2"
    ansible-playbook my_playbook.yml --tags "tag2"
    ansible-playbook my_playbook.yml --skip-tags "tag1"

By default Ansible run all tags

