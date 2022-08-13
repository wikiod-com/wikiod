---
title: "Ansible group variables"
slug: "ansible-group-variables"
draft: false
images: []
weight: 9995
type: docs
toc: true
---

## Group variables with static inventory
It is suggested that you define groups based on purpose of the host (roles) and also geography or datacenter location (if applicable):

File `inventory/production`

    [rogue-server]
    192.168.1.1
    
    [atlanta-webservers]
    www-atl-1.example.com
    www-atl-2.example.com
    
    [boston-webservers]
    www-bos-1.example.com
    www-bos-2.example.com
    
    [atlanta-dbservers]
    db-atl-1.example.com
    db-atl-2.example.com
    
    [boston-dbservers]
    db-bos-1.example.com
    
    # webservers in all geos
    [webservers:children]
    atlanta-webservers
    boston-webservers
    
    # dbservers in all geos
    [dbservers:children]
    atlanta-dbservers
    boston-dbservers
    
    # everything in the atlanta geo
    [atlanta:children]
    atlanta-webservers
    atlanta-dbservers
    
    # everything in the boston geo
    [boston:children]
    boston-webservers
    boston-dbservers

File `group_vars/all`

    ---
    apache_port: 80

File `group_vars/atlanta-webservers`

    ---
    apache_port: 1080

File `group_vars/boston-webservers`

    ---
    apache_port: 8080

File `host_vars/www-bos-2.example.com`

    ---
    apache_port: 8111

After running `ansible-playbook -i inventory/hosts install-apache.yml` (hosts in the playbook would be `hosts: all`)

The ports would be

| Address | Port |
| ------ | ------ |
| `192.168.1.1`   | 80   |
| `www-atl-1.example.com`   | 1080   |
| `www-atl-2.example.com`   | 1080   |
| `www-bos-1.example.com`   | 8080   |
| `www-bos-2.example.com`   | 8111   |

