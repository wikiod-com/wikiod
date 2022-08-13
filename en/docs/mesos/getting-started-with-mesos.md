---
title: "Getting started with mesos"
slug: "getting-started-with-mesos"
draft: false
images: []
weight: 1
type: docs
toc: true
---

## Mesos Description and Installation from Mespsheres package
Mesos is a cluster manager aiming for improved resource utilization by dynamically
sharing resources among multiple frameworks. It was started at the University of
California, Berkeley in 2009 and is in production use in many companies, including
Twitter and Airbnb. It became an Apache top-level project in July 2013 after nearly
two years in incubation.
Mesos shares the available capacity of machines (or nodes) among jobs of different
natures, as shown in the following figure. Mesos can be thought of as a kernel
for the data center that provides a unified view of resources on all nodes and
seamless access to these resources in a manner similar to what an operating system
kernel does for a single computer. Mesos provides a core for building data center
applications and its main component is a scalable two-phased scheduler. The Mesos
API allows you to express a wide range of applications without bringing the domainspecific
information into the Mesos core. By remaining focused on core, Mesos
avoids problems that are seen with monolithic schedulers.

# Ubuntu
    
    # Setup
    sudo apt-key adv --keyserver hkp://keyserver.ubuntu.com:80 --recv E56151BF
    DISTRO=$(lsb_release -is | tr '[:upper:]' '[:lower:]')
    CODENAME=$(lsb_release -cs)
    
    # Add the repository
    echo "deb http://repos.mesosphere.com/${DISTRO} ${CODENAME} main" | \
    sudo tee /etc/apt/sources.list.d/mesosphere.list
    sudo apt-get -y update
    
    # Install
    sudo apt-get -y install mesos

# RedHat/CentOs

    # Install Zookeeper
    sudo rpm -Uvh http://archive.cloudera.com/cdh4/one-click-install/redhat/6/x86_64/cloudera-cdh-4-0.x86_64.rpm
    sudo yum -y install zookeeper

    # Install Mesos
    sudo rpm -Uvh http://repos.mesosphere.com/el/7/noarch/RPMS/mesosphere-el-repo-7-1.noarch.rpm
    sudo yum -y install mesos
    
[More](https://open.mesosphere.com/getting-started/install/)


