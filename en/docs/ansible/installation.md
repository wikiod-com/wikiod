---
title: "Installation"
slug: "installation"
draft: false
images: []
weight: 9933
type: docs
toc: true
---

Installing Ansible in any OS, including Windows using Virtual Box and Vagrant. An alternate solution is also available if you just want to practice ansible ad-hoc commands and playbooks and do not wish to set up the local environment.

## Installing Ansible on Ubuntu
Ansible maintains a PPA repository that can be used to install the Ansible binaries:

    sudo apt-add-repository ppa:ansible/ansible -y
    sudo apt-get update && sudo apt-get install ansible -y

To install a specific version, use `pip`. The PPA may be out of date.

## Installing from source
Ansible is **best used** from a checkout.

It runs as you (not root) and it has minimal python dependencies. 

Python pip dependency install with pip:

    sudo pip install paramiko PyYAML Jinja2 httplib2 six

Next, clone the [Ansible repo][1] from GitHub:
 
    cd ~/Documents
    git clone git://github.com/ansible/ansible.git --recursive 
    cd ansible

Finally, add the ansible initialization script line to your ~/.bashrc or ~/.zshrc :

    source ~/Documents/ansible/hacking/env-setup

Restart your terminal session, and test with 
    
    ansible --version

  [1]: https://github.com/ansible/ansible

## Installing Ansible on MacOS
There are two main ways way to install Ansible on OS X, either using the [Homebrew][1] or Pip package manager. 

If you have homebrew, the latest Ansible can be installed using the following command:

    brew install ansible

To install Ansible 1.9.X branch use following command:

    brew install homebrew/versions/ansible19

To install Ansible 2.0.X branch use following command:

    brew install homebrew/versions/ansible20

To install using pip, use the following command: `pip install ansible`.

To install a specific version, use `pip install ansible=<required version>`.

  [1]: http://brew.sh

## Installation on Red Hat based systems
Ansible can be installed on CentOS or other Red Hat based systems. Firstly you should install the prerequisites:

    sudo yum -y update
    sudo yum -y install gcc libffi-devel openssl-devel python-pip python-devel

then install Ansible with pip:

    sudo pip install ansible

I can recommend for you to upgrade the setuptools after the installation:

    sudo pip install --upgrade setuptools

You can also use the local Package Manager as well:
    
    yum install ansible

## Installation on Amazon Linux from git repo
Amazon Linux is a RHEL variant, so the Red Hat instructions should work for the most part.  There is, however, at least one discrepancy.

There was an instance where the **python27-devel** package, as opposed to **python-devel**, was explicitly necessary.

Here, we will install from source.

    sudo yum -y update
    sudo yum -y install python27 python27-devel openssl-devel libffi-devel gcc git
    
    git clone https://github.com/ansible/ansible/<search the github for a preferable branch>
    
    cd ansible
    sudo python setup.py build
    sudo python setup.py install



## Installing Ansible On Any OS(windows) Machine Using Virtual Box+Vagrant
My laptop is having Windows 10. Here i am giving steps that you can follow to test and learn Ansible. 

**SOME THEORY**

For Ansible you need a Control Machine and a host(or hosts) to run the Playbook. 

- **Control Machine** should be Linux based or MacOS(windows not allowed) and need Python (2.6 or higher version). Here Ansible will be installed.
- **Target machine** (host/node) can be Linux/MacOS/windows. This needs only Python to be installed. No agent software required.

**SETUP**

> Step 1: **Install [Virtual Box][1]**

 Virtual box is a software to create virtual computers of different OS. It is like having multiple computers each or different OS and different versions. 

Download [Virtual Box][1] according to the OS in your system and install it. 
> Step 2: **Install [Vagrant][2]**

Vagrant is Command Line Interface to create virtual machines in virtual box. This makes things easy. You need to learn basic Vagrant commands. 
> Step 3: **Create a folder where you want your virtual machine**

>Step 4: **Create Virtual Machine using Vagrant**

Open terminal and go to the path where you created folder, and run the following two commands.

You need to select [**Virtual Box**][3]. I am installing Ubuntu for example. You can choose anything from the list. You need to run these two commands under "**virtual box**" category: `vagrant init ubuntu/trusty64` and `vagrant up --provider virtualbox`. Other categories might be: hyperv, vmware_desktop etc. (this will take some time, as it will download the necessary files)

>Step 4: Install Ansible

For UbuntuOS: `sudo apt-get install ansible`


----------

**Alternative solution**: 
-----
You can use [**Katacoda**][4] to practice ansible. No need to install or setup anything. Run two commands given in step 2 and after that, you are good to go.  


  [1]: https://www.virtualbox.org/wiki/Downloads
  [2]: https://www.vagrantup.com/downloads.html
  [3]: http://vagrantcloud.com
  [4]: https://www.katacoda.com/jonatanblue/scenarios/1

