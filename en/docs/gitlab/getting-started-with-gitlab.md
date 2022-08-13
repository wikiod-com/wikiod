---
title: "Getting started with gitlab"
slug: "getting-started-with-gitlab"
draft: false
images: []
weight: 1
type: docs
toc: true
---

## Installation or Setup
This is a short summary of the GitLab guide on [Install a GitLab CE Omnibus package][1].

**Requirements**

In order to install the GitLab Community Edition on your server, you should read the [requirements page][2]. To make it brief, the recommended requirements are:

 - **OS:** Ubuntu, Debian, CentOS, RHEL
 - **Ruby version:** Ruby (MRI) 2.1.x, currently does not work with versions 2.2 or 2.3.
 - **CPU:** 2 cores (supports up to 500 users)
 - **Memory:** 2 GB (supports up to 100 users)
 - **Database:** PostgreSQL

---

**Installation**

The recommended method is to install the Omnibus package, which is fast to install. It contains GitLab and all its dependencies (Ruby, PostgreSQL, Redis, Nginx, Unicorn, etc.). For other methods check out the [GitLab's installation options][3]

With Ubuntu 16.04 as the recommended OS, this guide describes the installation steps on Debian based distributions. For CentOS, RHEL, Oracle Linux and Scientific Linux, please refer to the original guides:

 - [CentOS 6 (and RedHat/Oracle/Scientific Linux 6)][4]
 - [CentOS 7 (and RedHat/Oracle/Scientific Linux 7)][5]

**Ubuntu, Debian, Raspberrian**

Install necessary dependencies. If you use Postfix select 'Internet Site' during setup

    sudo apt-get install curl openssh-server ca-certificates postfix apt-transport-https
    curl https://packages.gitlab.com/gpg.key | sudo apt-key add -

Add the Gitlab package server and install the package

    sudo curl -sS https://packages.gitlab.com/install/repositories/gitlab/raspberry-pi2/script.deb.sh | sudo bash
    sudo apt-get install gitlab-ce

If you do not want to install the repository through a piped script, [download the package manually][6] and install it using

    dpkg -i gitlab-ce_<version>.deb

Now configure and start GitLab

    sudo gitlab-ctl reconfigure

Finally browse to the hostname and login. At first you will be redirected to provide  a password for the initial administrator account. After that you're able to login in. The **default administrator account username** is **root**. 


  [1]: https://about.gitlab.com/downloads/#ubuntu1604
  [2]: http://docs.gitlab.com/ce/install/requirements.html
  [3]: https://about.gitlab.com/installation/
  [4]: https://about.gitlab.com/downloads/#centos6
  [5]: https://about.gitlab.com/downloads/#centos7
  [6]: https://packages.gitlab.com/gitlab/gitlab-ce

