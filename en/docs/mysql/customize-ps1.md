---
title: "Customize PS1"
slug: "customize-ps1"
draft: false
images: []
weight: 9980
type: docs
toc: true
---

## Customize the MySQL PS1 with current database
In the .bashrc or .bash_profile, adding:

    export MYSQL_PS1="\u@\h [\d]>"

make the MySQL client PROMPT show current user@host [database].

[![enter image description here][1]][1] 


  [1]: http://i.stack.imgur.com/lHXU6.png

## Custom PS1 via MySQL configuration file
In `mysqld.cnf` or equivalent:

    [mysql]
    prompt = '\u@\h [\d]> '

This achieves a similar effect, without having to deal with `.bashrc`'s.

