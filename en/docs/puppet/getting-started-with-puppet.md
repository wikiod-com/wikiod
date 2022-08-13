---
title: "Getting started with puppet"
slug: "getting-started-with-puppet"
draft: false
images: []
weight: 1
type: docs
toc: true
---

## What is puppet and why should I care?
Puppet is a configuration management solution. Users describe the desired state of a server or software and configuration management achieves this state. This brings following advantages:

- Configurations can be reproduced exactly the same every time, as many times as necessary
- Configurations for all software and servers are stored in a central location. This makes backup and version control of configurations easily achievable
- Changes to all servers propagate through the entire infrastructure within a couple of minutes, without having to log in to any machine directly
- Everything is described in the same language, making it easy to configure new software
- Modules are similar to libraries and allow configurations to be consolidated. Modules for all major software packages already exist, making installing them extremely easy
- Servers can share information between each other, influencing the configuration of other servers. For example a new server can automatically register itself with the load balancer and monitoring solution

Puppet uses Ruby to describe the desired state of a server, called a **node**. It does so with the use of primitives called **resource types**. By default, every 30 minutes, the puppet **agent** authenticates itself against the puppet **server**. It then sends a list of properties of itself called **facts**. The server looks at the facts and the configuration files called **manifests** and compiles the desired state for the node. It then sends that **configuration** back to the node, where the agent applies it.

To give an idea of how powerful this can be, here are a couple examples of increasing complexity showcasing what puppet can do for you.

**Example: User**

This example creates the user *username* on node *myserver* and adds it to the group *wheel*.

    node 'myserver' {
        user { 'username':
          ensure => 'present',
          groups => ['wheel'],
        }
    }

This file that would be stored on the puppet *server* is the *manifest*. The *resource type* in this example is *user*. Every resource type has optional and required properties. In this example, *ensure* is required and *groups* is optional. This specific configuration would only be applied to *myserver*. You can apply configurations to all nodes by placing it outside of a node definition.

It is possible to take a couple of resource definitions and store them as **modules**. A module is similar to a library. These modules can be shared online, and you usually find one for every major software package. The official way to share modules is through the puppet forge: https://forge.puppet.com/

**Example: Postgres**

This example installs a postgres server on node *myserver*, and creates a database *db*, owned by *username*, identified by *password*. It does so using the postgresql module.

    node 'myserver' {
        class { 'postgresql::server': }
        
        postgresql::server::db { 'db':
            user     => 'username',
            password => 'password',
        }
    }

In this case *postgresql* is a module. The module itself takes care of identifying the operating system, downloading and installing the program, and then configuring it according to the manifest. This is a basic example but the module allows a great deal of customization.

Note that it is not necessary to know SQL or how to actually install a postgres server to do so. Official modules are well maintained and provide a sane and secure base configuration.

It is also possible to use *facts* in manifests. Facts act like variables.

**Example: Conditions using facts**

This example uses the rsyslog module to configure rsyslog on all non-windows machines.

    if $osfamily != 'windows' {
      class { 'rsyslog::client': }
    }

*$osfamily* is a fact. These facts are gathered every time the puppet agent runs. Note that because this definition is outside of a node definition, it gets applied to all nodes. However, *rsyslog::client* will only be executed on nodes that do not run windows.

Since puppet uses ruby, programmatic elements like control flows and variables can be used in manifests.

With the addition of **PuppetDB** you can share information between multiple nodes. This allows one node to influence configuration on a different node. Classic examples include load balancers or monitoring solutions.

**Example: Registering a host with monitoring using exported resources**

This example creates an **exported resource** on a node, and then imports that resource on the monitoring server, adding the host to the monitoring. It is using the Icinga2 puppet module.

    @@icinga2::object::host { $::fqdn:
      display_name     => $::fqdn,
      ipv4_address     => $::ipaddress_eth0,
    }

    node 'icinga2' {
        Icinga2::Object::Host <<| |>> { }
    }

*@@icinga2::object::host* creates a host definition object. This gets created by every node that executes this code. The *@@* marks it as an *exported resource*. Usually, nodes do not share information in puppet. Exported resources allow to do that.

Note that all the property values in the host definition are facts. This means they will be different for every node which executes it.

Finally, the exported resource gets *imported* by the *icinga2* node. The Icinga2 module is responsible for making sure that the correct configuration files are created and reloaded. 

## Is it for you?
If you do deployments, configure your applications on multiple servers and required to login to your servers and make some changes in infrastructure, applications, pre-requisits etc. then puppet can definitely help you.

Except all this if you handle a big infrastructure and want a centralized management you can also have a look.

## Before you startup
Before you decide to work on puppet there are few things that you need to know.

 1. puppet work in both client-server architecture (widely used) as well
    single machine (specially for testing purpose)
    
 2. puppet master can only be configured on a linux machine (master
    machine/node), windows can be used only as client (managed
    machine/node)

 3. if configuring master, you must be aware of using linux machine and basic commands

 4. puppet provides it's own configuration language that looks like json

## Official Documentation
Puppet provide official documention for both open-source and enterprise versions. you can find it [here][1]


  [1]: https://docs.puppet.com/ "Official Doc"

## Installation
System Requirements
===================

However, the Puppet master service is fairly resource intensive, and should be installed on a robust dedicated server.

 - At a minimum, your Puppet master server should have two processor cores and at least 1 GB of RAM.
 - To comfortably serve at least 1,000 nodes, it should have 2-4 processor cores and at least 4 GB of RAM.


Check your network configuration:
=================================

In an agent/master deployment, you must prepare your network for Puppet’s traffic.

 - **Firewalls:** The Puppet master server must allow incoming connections on port 8140, and agent nodes must be able to connect to the master on that port.
 - **Name resolution:** Every node must have a unique hostname. Forward and reverse DNS must both be configured correctly.

> ***Note:*** The default Puppet master hostname is puppet. Your agent nodes can be ready sooner if this hostname resolves to your Puppet
> master.
> 
> The time must be set accurately on the Puppet master server that will
> be acting as the certificate authority. You should probably use NTP.


----------

Installing Puppet Server
========================

Puppet provides official packages that install Puppet Server 2.4 and all of its prerequisites on the following platforms.

**Red Hat Enterprise Linux**

 - Enterprise Linux 6
 - Enterprise Linux 7

**Debian**

 - Debian 7 (Wheezy)
 - Debian 8 (Jessie)

**Ubuntu**

 - Ubuntu 12.04 (Precise)
 - Ubuntu 14.04 (Trusty)
 - Ubuntu 15.10 (Wily)
 - Ubuntu 16.04 (Xenial)

Enable the Puppet package repositories
======================================

**Enterprise Linux 7**

    sudo rpm -Uvh https://yum.puppetlabs.com/puppetlabs-release-pc1-el-7.noarch.rpm

[For other versions look here][2]


**Installing puppet master**

    yum install puppetserver

or 

    apt-get install puppetserver

> Puppet Server is configured to use 2 GB of RAM by default. To change [look here][1]

**Start the Puppet Server service:**

    systemctl start puppetserver

or

    service puppetserver start


  [1]: https://docs.puppet.com/puppetserver/2.4/install_from_packages.html#memory-allocation
  [2]: https://docs.puppet.com/puppet/latest/reference/puppet_collections.html#enterprise-linux-7

