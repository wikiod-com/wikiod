---
title: "Provision"
slug: "provision"
draft: false
images: []
weight: 9985
type: docs
toc: true
---

## Syntax
 - config.vm.provision "shell", inline: COMMANDS
 - config.vm.provision "shell", path: "relativePath/script.sh"

## Parameters
| Parameter | Details |  
| --------- | ------- |  
| COMMANDS | The shell commands to run. Can be a string (e.g. `"echo \"Hello, World!\""`) or a string variable (e.g. `$setup`). |  


Provisioning is used to automatically configure a virtual machine. It is performed automatically when a virtual machine is first created (using `vagrant up`). It can also be re-run later using `vagrant provision`.

## Minimal setup


## Launch and provision the box


## Launch the box without provisioning


## Provision a running box


## Shell provisioner
The shell provisioner runs a shell script when provisioning.

    $setup = <<SETUP
    # You can write your shell script between here ...
    sudo echo "Hello, World!" > /etc/motd.tail
    # ... and here.
    SETUP
    
    Vagrant.configure("2") do |config|
      config.vm.box = "ubuntu/trusty64"
      config.vm.provision "shell", inline: $setup
    end

## Run shell script from file (not using inlining)
    # provision/bootstrap-controller.sh : path and shell filename from vagrantfile location
    config.vm.define "configcontroller" do |controller|
    ...
    controller.vm.provision :shell do |shell|
        shell.path = "provision/bootstrap-controller.sh"
    end
    ...

