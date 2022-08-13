---
title: "How To Create A DreamHost Cloud Server From An Ansible Playbook"
slug: "how-to-create-a-dreamhost-cloud-server-from-an-ansible-playbook"
draft: false
images: []
weight: 9991
type: docs
toc: true
---

## Install Shade library
Shade is a library developed by OpenStack to simplify interactions with OpenStack clouds, like DreamHost. 
 
$ pip install shade

## Write a Playbook to Launch a Server
Create a file named ```launch-server.yaml```, that will be our playbook.

The first part of the playbook is a list of hosts that your playbook will run on, we only have one, localhost.

    - hosts: localhost

Then we need to define a list of tasks to perform in this playbook. We will only have one that launches an Ubuntu Xenial server on DreamCompute.

    tasks:
      - name: launch an Ubuntu server

Next part of the playbook uses the ```os_server``` (OpenStack Server) module. This defines what the server has to look like in DreamCompute.

    os_server:

First step is to authenticate to DreamCompute; substitute ```{username}``` with your DreamCompute username, ```{password}``` with your DreamCompute password, and ```{project}``` with your DreamCompute project. You'll find those in the [OpenStack RC][1] file.

      auth:
        auth_url: https://iad2.dream.io:5000
        username: {username}
        password: {password}
        project_name: {project}

Next lines define some elements of the new server.

      state: present
      name: ansible-vm1
      image: Ubuntu-16.04
      key_name: {keyname}
      flavor: 50
      network: public
      wait: yes

Lets break down the previous few lines:

 - ```state``` is the state of the server, possible values are ```present``` or ```absent```
 - ```name``` is the name of the server to create; can be any value
 - ```image``` is the image to boot the server from; possible values are visible on [DreamHost Cloud web panel][2]; the variable accepts either image name or UUID 
 - ```key_name``` is the name of the public key to add to the server once it is created;  this can be any key has already been added to DreamCompute.
 - ```flavor``` is the flavor of server to boot; this defines how much RAM and CPU your server will have; the variable accepts either the name of a flavor (gp1.semisonic) or the ID (50, 100, 200, etc)
 - ```network``` is the network to put your server on. In DreamHost Cloud case it is the ```public``` network.
 - ```wait``` set to yes forces the playbook to wait for the server to be created before continuing.

  [1]: https://iad2.dreamcompute.com/project/access_and_security/api_access/openrc/
  [2]: https://iad2.dreamcompute.com/project/images/

## Running the Playbook
Run the Ansible playbook:

    $ ansible-playbook launch-server.yaml

You should see output like

    PLAY [localhost]
    ***************************************************************
    
    TASK [setup]
    *******************************************************************
    ok: [localhost]
    
    TASK [launch an Ubuntu server]
    ***********************************************
    changed: [localhost]
    
    PLAY RECAP
    *********************************************************************
    localhost                  : ok=2    changed=1    unreachable=0    failed=0

Now if you check the [DreamHost Cloud dashboard][1] you should see a new instance named “ansible-vm1”


  [1]: https://iad2.dreamcompute.com/project/instances/

