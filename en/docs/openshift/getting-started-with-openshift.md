---
title: "Getting started with openshift"
slug: "getting-started-with-openshift"
draft: false
images: []
weight: 1
type: docs
toc: true
---

## Getting started with Minishift
This is similar to [Docker-machine][1] or [minikube][2] which are used to learn Docker and Kubernetes.     It will run a virtual machine containing everything you need to test openshift-origin.

Here is the documentation from openshift-origin and how to install it on any OS [here][3]

I'll write doc for Debian-like OS with virtualbox driver since I did it on my laptop but it's kind of a copy/paste from openshift website

Install Virtualbox
==================

    # apt-get install linux-headers-$(uname -r|sed 's,[^-]*-[^-]*-,,') virtualbox

Enable and start virtualbox
===========================

    # systemctl enable virtualbox && \
      systemctl start virtualbox
Running `systemctl status virtualbox` should show you an enabled & active service.

Download minishift & install binary
===================================

> You can find all minishift releases [here][4] in case this one doesn't exist anymore when you try the `wget`

    # mkdir $HOME/minishift && \
      wget -O $HOME/minishift/minishift.tar.gz https://github.com/minishift/minishift/releases/download/v1.0.0/minishift-1.0.0-linux-amd64.tgz && \ 
      tar -xf $HOME/minishift/minishift.tar.gz -C $HOME/minishift

Add minishift command to $PATH

    # echo "export PATH=\$PATH:$HOME/minishift" >> $HOME/.bashrc && \
      source $HOME/.bashrc

Run minishift
=============

> `minishift --help` show all available options

    # minishift start --vm-driver=virtualbox

Now you should have an openshift up and running

Export openshift client command to $PATH
================================
This command should be installed when you run `minishift start` if it is not present on the system.
Add it to $PATH for more comfort:

    # echo "export PATH=\$PATH:$(find $HOME/.minishift -name oc -type f)" >> $HOME/.bashrc && \
      source $HOME/.bashrc
    

Login to minishift
=======

> Note: You'll have to accept unsecured ssl certificates

To connect with bash client :

    # oc login -u system:admin https://$(minishift ip):8443
To connect with your default browser (login with developer:developer):

    # minishift console

*Finally your Openshift-origin-standalone for test/dev purposes is ready and accessible.*

Some more things you maybe need to know
=======

 - When you are done and want to recover some RAM run `minishift stop`
   
 - If you want to destroy the all VM containing openshift, run
   `minishift delete`

 - To see your Openshift master logs, run `minishift logs`
   
 - To run a shell into the VM containing Openshift, run
   `minishift ssh`

 - If you are new to openshift and want to try a deployment, check the
   following [doc][5]
 


  [1]: https://docs.docker.com/machine/
  [2]: https://github.com/kubernetes/minikube
  [3]: https://docs.openshift.org/latest/minishift/getting-started/installing.html
  [4]: https://github.com/minishift/minishift/releases
  [5]: https://docs.openshift.org/latest/minishift/getting-started/quickstart.html#deploy-sample-app

## Getting started with an all-in-one container
To install openshift follow installation steps on https://install.openshift.com

