---
title: "Ansible Group Vars"
slug: "ansible-group-vars"
draft: false
images: []
weight: 9995
type: docs
toc: true
---

## Example group_vars/development, and why
Project structure

    project/
      group_vars/
         development
      inventory.development
      playbook.yaml

These variables will be applied to hosts under the development group due to the filename.

    ---
    ## Application
    app_name: app
    app_url: app.io
    web_url: cdn.io
    app_friendly: New App
    env_type: production
    app_debug: false

    ## SSL
    ssl: true
    ev_ssl: false

    ## Database
    database_host: 127.0.0.1
    database_name: app
    database_user: sql

    ## Elasticsearch
    elasticsearch_host: 127.0.0.1


