---
title: "Providers"
slug: "providers"
draft: false
images: []
weight: 9987
type: docs
toc: true
---

## Set default provider with environment variable
Introduce environment variable `VAGRANT_DEFAULT_PROVIDER`

    export VAGRANT_DEFAULT_PROVIDER=vmware_fusion

## Set default provider in Vagrantfile
    Vagrant.configure("2") do |config|
      # ... other config up here

      config.vm.provider "vmware_fusion"
    end

## Launch box in Hyper-V
    vagrant up --provider hyperv

## Launch box in Docker
    vagrant up --provider docker

## Launch box in VMWare Fusion
    vagrant up --provider vmware_fusion

## Launch box in VMWare Workstation
    vagrant up --provider vmware_workstation

## Launch box in VirtualBox
    vagrant up --provider virtualbox

VirtualBox is the default provider for a vanilla Vagrant setup.

