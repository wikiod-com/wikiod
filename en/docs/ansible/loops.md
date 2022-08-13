---
title: "Loops"
slug: "loops"
draft: false
images: []
weight: 9995
type: docs
toc: true
---

## Copy multiple files in a single task
    - name: copy ssl key/cert/ssl_include files
      copy: src=files/ssl/{{ item }} dest=/etc/apache2/ssl/
      with_items:
        - g_chain.crt
        - server.crt
        - server.key
        - ssl_vhost.inc

## Install multiple packages in a single task
    - name: Installing Oracle Java and support libs
      apt: pkg={{ item }}
      with_items:
        - python-software-properties
        - oracle-java8-installer
        - oracle-java8-set-default
        - libjna-java


