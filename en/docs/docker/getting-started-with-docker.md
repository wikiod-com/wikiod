---
title: "Getting started with Docker"
slug: "getting-started-with-docker"
draft: false
images: []
weight: 1
type: docs
toc: true
---

## Installing Docker on Windows
**Requirements:** 64-bit version of Windows 7 or higher on a machine which supports  Hardware Virtualization Technology, and it is enabled.

While the docker binary can run natively on Windows, to build and host containers you need to run a Linux virtual machine on the box. 

<!-- if version [gte 1.12.0] -->

Since version 1.12 you don't need to have a separate VM to be installed, as Docker can use the native Hyper-V functionality of Windows to start up a small Linux machine to act as backend.

To install docker follow the following steps:

1. Go to [Docker for Windows][1]
2. Download and run the installer.
3. Continue through installer with default options and enter your account credentials when requested. 

[Check here][1] for more information on the installation.

<!-- end version if -->

<!-- if version [lte 1.11.2] -->

Until version 1.11 the best way to run this Linux VM is to install Docker Toolbox, that installs Docker, VirtualBox and the Linux guest machine.

To install docker toolbox follow the following steps:

 1. Go to [Docker Toolbox](https://www.docker.com/toolbox)
 2. Click the link for Windows and run the installer.
 3. Continue through installer with default options and enter your account credentials when requested. 

This will install the Docker binaries in Program Files and update any existing Virtual Box installation. [Check here](https://docs.docker.com/v1.11/windows/step_one/) for more information on the installation.

<!-- end version if -->

**To Verify Installation:**

<!-- if version [gte 1.12.0] -->
  
1. Start `Docker` from the Start menu if it hasn't been started yet, and make sure it is running. Next upen up any terminal (either `cmd` or PowerShell)

<!-- end version if -->

<!-- if version [lte 1.11.2] -->

1. On your Desktop, find the Docker Toolbox icon. Click the icon to launch a Docker Toolbox terminal.

<!-- end version if -->

2. Once the terminal is open type

       docker run hello-world

3. If all is well then this should print a welcome message verifying that the installation was successful.


  [1]: https://docs.docker.com/docker-for-windows/

## Installing Docker on Mac OS X
**Requirements:** OS X 10.8 “Mountain Lion” or newer required to run Docker.


While the docker binary can run natively on Mac OS X, to build and host containers you need to run a Linux virtual machine on the box. 

<!-- if version [gte 1.12.0] -->

Since version 1.12 you don't need to have a separate VM to be installed, as Docker can use the native `Hypervisor.framework` functionality of OSX to start up a small Linux machine to act as backend.

To install docker follow the following steps:

1. Go to [Docker for Mac][1]
2. Download and run the installer.
3. Continue through installer with default options and enter your account credentials when requested. 

[Check here][1] for more information on the installation.

<!-- end version if -->

<!-- if version [lte 1.11.2] -->

Until version 1.11 the best way to run this Linux VM is to install Docker Toolbox, that installs Docker, VirtualBox and the Linux guest machine.

To install docker toolbox follow the following steps:

 1. Go to [Docker Toolbox](https://www.docker.com/toolbox)
 2. Click the link for Mac and run the installer.
 3. Continue through installer with default options and enter your account credentials when requested. 

This will install the Docker binaries in `/usr/local/bin` and update any existing Virtual Box installation. [Check here](https://docs.docker.com/v1.11/mac/step_one/) for more information on the installation.

<!-- end version if -->

**To Verify Installation:**

<!-- if version [gte 1.12.0] -->
  
1. Start `Docker.app` from the Applications folder, and make sure it is running. Next open up Terminal.

<!-- end version if -->

<!-- if version [lte 1.11.2] -->

1. Open the `Docker Quickstart Terminal`, which will open a terminal and prepare it for use for Docker commands.

<!-- end version if -->

2. Once the terminal is open type

       $ docker run hello-world

3. If all is well then this should print a welcome message verifying that the installation was successful.


  [1]: https://docs.docker.com/docker-for-mac/




## Installing docker on Ubuntu Linux
Docker is supported on the following *64-bit* versions of Ubuntu Linux:

* Ubuntu Xenial 16.04 (LTS)
* Ubuntu Wily 15.10
* Ubuntu Trusty 14.04 (LTS)
* Ubuntu Precise 12.04 (LTS)

A couple of notes:

> The following instructions involve installation using **Docker** packages only, and this ensures obtaining the latest official release of **Docker**. If you need to  install only using `Ubuntu-managed` packages, consult the Ubuntu documentation (Not recommended otherwise for obvious reasons).

> Ubuntu Utopic 14.10 and 15.04 exist in Docker’s APT repository but are no longer officially supported due to known security issues.

**Prerequisites**
* Docker only works on a 64-bit installation of Linux.
* Docker requires Linux kernel version 3.10 or higher (Except for `Ubuntu Precise 12.04`, which requires version 3.13 or higher). Kernels older than 3.10 lack some of the features required to run Docker containers and contain known bugs which cause data loss and frequently panic under certain conditions. Check current kernel version with the command `uname -r`. Check this post if you need to update your `Ubuntu Precise (12.04 LTS)` kernel by scrolling further down. Refer to this [WikiHow][1]
post to obtain the latest version for other Ubuntu installations.

**Update APT sources**

This needs to be done so as to access packages from Docker repository.

1) Log into your machine as a user with `sudo` or `root` privileges.
2) Open a terminal window.
3) Update package information, ensure that APT works with the https method, and that CA certificates are installed.
 

    $ sudo apt-get update
    $ sudo apt-get install \
        apt-transport-https \
        ca-certificates \
        curl \
        software-properties-common

4) Add Docker’s official GPG key: 

        $ curl -fsSL https://download.docker.com/linux/ubuntu/gpg | sudo apt-key add -

   Verify that the key fingerprint is **9DC8 5822 9FC7 DD38 854A E2D8 8D81 803C 0EBF CD88**.

        $ sudo apt-key fingerprint 0EBFCD88

            
            pub   4096R/0EBFCD88 2017-02-22
                  Key fingerprint = 9DC8 5822 9FC7 DD38 854A  E2D8 8D81 803C 0EBF CD88
            uid                  Docker Release (CE deb) <docker@docker.com>
            sub   4096R/F273FCD8 2017-02-22



5) Find the entry in the table below which corresponds to your Ubuntu version. This determines where APT will search for Docker packages. When possible, run a long-term support (LTS) edition of Ubuntu.

| Ubuntu Version| Repository|
| ------ | ------ |
| Precise 12.04 (LTS)   | `deb https://apt.dockerproject.org/repo ubuntu-precise main`   |
| Trusty 14.04 (LTS)   | `deb https://apt.dockerproject.org/repo ubuntu-trusty main`   |
|Wily 15.10        |`deb https://apt.dockerproject.org/repo ubuntu-wily main`| 
|Xenial 16.04 (LTS)|`deb https://apt.dockerproject.org/repo ubuntu-xenial main`|

> **Note:** Docker does not provide packages for all architectures. Binary artifacts are built nightly, and you can download them from `https://master.dockerproject.org`. To install docker on a multi-architecture system, add an `[arch=...]` clause to the entry. Refer to [Debian Multiarch wiki][2] for details.

6) Run the following command, substituting the entry for your operating system for the placeholder `<REPO>`.

    $ echo "<REPO>" | sudo tee /etc/apt/sources.list.d/docker.list

7) Update the `APT` package index by executing `sudo apt-get update`.

8) Verify that `APT` is pulling from the right repository.

When you run the following command, an entry is returned for each version of Docker that is available for you to install. Each entry should have the URL `https://apt.dockerproject.org/repo/`. The version currently installed is marked with `***`.See the below example's output.

    $ apt-cache policy docker-engine

      docker-engine:
        Installed: 1.12.2-0~trusty
        Candidate: 1.12.2-0~trusty
        Version table:
       *** 1.12.2-0~trusty 0
              500 https://apt.dockerproject.org/repo/ ubuntu-trusty/main amd64 Packages
              100 /var/lib/dpkg/status
           1.12.1-0~trusty 0
              500 https://apt.dockerproject.org/repo/ ubuntu-trusty/main amd64 Packages
           1.12.0-0~trusty 0
              500 https://apt.dockerproject.org/repo/ ubuntu-trusty/main amd64 Packages
    
From now on when you run `apt-get upgrade`, `APT` pulls from the new repository.

**Prerequisites by Ubuntu Version**

For Ubuntu Trusty (14.04) , Wily (15.10) , and Xenial (16.04) , install the `linux-image-extra-*` kernel packages, which allows you use the `aufs` storage driver.

To install the `linux-image-extra-*` packages:

1) Open a terminal on your Ubuntu host.

2) Update your package manager with the command `sudo apt-get update`.

3) Install the recommended packages.

       $ sudo apt-get install linux-image-extra-$(uname -r) linux-image-extra-virtual
4)   Proceed to Docker installation

For Ubuntu Precise (12.04 LTS), Docker requires the 3.13 kernel version. If your kernel version is older than 3.13, you must upgrade it. Refer to this table to see which packages are required for your environment:

| Package| Description|
| ------ | ------ |
| `linux-image-generic-lts-trusty`| Generic Linux kernel image. This kernel has `AUFS` built in. This is required to run Docker.   |
| `linux-headers-generic-lts-trusty`   | Allows packages such as `ZFS` and `VirtualBox guest additions` which depend on them. If you didn’t install the headers for your existing kernel, then you can skip these headers for the `trusty` kernel. If you’re unsure, you should include this package for safety.   |
| `xserver-xorg-lts-trusty`| Optional in non-graphical environments without Unity/Xorg. **Required** when running Docker on machine with a graphical environment.   |
| `ligbl1-mesa-glx-lts-trusty`   | To learn more about the reasons for these packages, read the installation instructions for backported kernels, specifically the [LTS Enablement Stack][3]. Refer to note 5 under each version.   |

To upgrade your kernel and install the additional packages, do the following:

1. Open a terminal on your Ubuntu host.
2. Update your package manager with the command `sudo apt-get update`.
3. Install both the required and optional packages.

       $ sudo apt-get install linux-image-generic-lts-trusty
4. Repeat this step for other packages you need to install.
5. Reboot your host to use the updated kernel using the command `sudo reboot`.
6. After reboot, go ahead and install Docker.

**Install the latest version**

Make sure you satisfy the prerequisites, only then follow the below steps.

> **Note:** For production systems, it is recommended that you [install a specific version][4] so that you do not accidentally update Docker. You should plan upgrades for production systems carefully.

1) Log into your Ubuntu installation as a user with `sudo` privileges. (Possibly running `sudo -su`).

2) Update your APT package index by running `sudo apt-get update`.

3) Install Docker Community Edition     with the command `sudo apt-get install docker-ce`.
4) Start the `docker` daemon with the command `sudo service docker start`.
5) Verify that `docker` is installed correctly by running the hello-world image.

        $ sudo docker run hello-world
This command downloads a test image and runs it in a container. When the container runs, it prints an informational message and exits.

**Manage Docker as a non-root user**

If you don’t want to use `sudo` when you use the docker command, create a Unix group called `docker` and add users to it. When the `docker` daemon starts, it makes the ownership of the Unix socket read/writable by the docker group.

To create the `docker` group and add your user:
1) Log into Ubuntu as a user with `sudo` privileges.
2) Create the `docker` group with the command `sudo groupadd docker`.
3) Add your user to the `docker` group.

        $ sudo usermod -aG docker $USER
4) Log out and log back in so that your group membership is re-evaluated.
5) Verify that you can `docker` commands without `sudo` permission.

        $ docker run hello-world
 
If this fails, you will see an error:

         Cannot connect to the Docker daemon. Is 'docker daemon' running on this host?
Check whether the `DOCKER_HOST` environment variable is set for your shell.

        $ env | grep DOCKER_HOST
If it is set, the above command will return a result. If so, unset it.

        $ unset DOCKER_HOST
You may need to edit your environment in files such as `~/.bashrc` or `~/.profile` to prevent the `DOCKER_HOST` variable from being set erroneously.

 [1]: http://www.wikihow.com/Update-Ubuntu-Kernel
  [2]: https://wiki.debian.org/Multiarch/HOWTO#Setting_up_apt_sources
  [3]: https://wiki.ubuntu.com/Kernel/LTSEnablementStack
  [4]: https://docs.docker.com/engine/installation/linux/ubuntulinux/#/install-a-specific-version

## Installing Docker on Ubuntu
**Requirements:** Docker can be installed on any Linux with a kernel of at least version 3.10. Docker is supported on the following 64-bit versions of Ubuntu Linux:

* Ubuntu Xenial 16.04 (LTS)
* Ubuntu Wily 15.10
* Ubuntu Trusty 14.04 (LTS)
* Ubuntu Precise 12.04 (LTS)

**Easy Installation**

**Note: Installing Docker from the default Ubuntu repository will install an old version of Docker.**

To install the latest version of Docker using the Docker repository, use `curl` to grab and run the installation script provided by Docker:

    $ curl -sSL https://get.docker.com/ | sh

Alternatively, `wget` can be used to install Docker:

    $ wget -qO- https://get.docker.com/ | sh

Docker will now be installed.

**Manual Installation**

If, however, running the installation script is not an option, the following instructions can be used to manually install the latest version of Docker from the official repository.

    $ sudo apt-get update
    $ sudo apt-get install apt-transport-https ca-certificates

Add the GPG key:

    $ sudo apt-key adv --keyserver hkp://p80.pool.sks-keyservers.net:80 \
      --recv-keys 58118E89F3A912897C070ADBF76221572C52609D

Next, open the `/etc/apt/sources.list.d/docker.list` file in your favorite editor. If the file doesn’t exist, create it. Remove any existing entries. Then, depending on your version, add the following line:

- Ubuntu Precise 12.04 (LTS):
    
    `deb https://apt.dockerproject.org/repo ubuntu-precise main`

- Ubuntu Trusty 14.04 (LTS)

    `deb https://apt.dockerproject.org/repo ubuntu-trusty main`

- Ubuntu Wily 15.10

    `deb https://apt.dockerproject.org/repo ubuntu-wily main`

- Ubuntu Xenial 16.04 (LTS)

    `deb https://apt.dockerproject.org/repo ubuntu-xenial main`

Save the file and exit, then update your package index, uninstall any installed versions of Docker, and verify `apt` is pulling from the correct repo:

    $ sudo apt-get update
    $ sudo apt-get purge lxc-docker
    $ sudo apt-cache policy docker-engine

Depending on your version of Ubuntu, some prerequisites may be required:

- Ubuntu Xenial 16.04 (LTS), Ubuntu Wily 15.10, Ubuntu Trusty 14.04 (LTS)

    `sudo apt-get update && sudo apt-get install linux-image-extra-$(uname -r)`

- Ubuntu Precise 12.04 (LTS)

    This version of Ubuntu requires kernel version 3.13. You may need to install additional packages depending on your environment:

    `linux-image-generic-lts-trusty`
    
    Generic Linux kernel image. This kernel has AUFS built in. This is required to run Docker.

    `linux-headers-generic-lts-trusty`

    Allows packages such as ZFS and VirtualBox guest additions which depend on them. If you didn’t install the headers for your existing kernel, then you can skip these headers for the `trusty` kernel. If you’re unsure, you should include this package for safety.

    `xserver-xorg-lts-trusty`

    `libgl1-mesa-glx-lts-trusty`

    These two packages are optional in non-graphical environments without Unity/Xorg. Required when running Docker on machine with a graphical environment. 

    To learn more about the reasons for these packages, read the installation instructions for backported kernels, specifically the LTS Enablement Stack — refer to note 5 under each version.

    Install the required packages then reboot the host:

    `$ sudo apt-get install linux-image-generic-lts-trusty`

    `$ sudo reboot`


Finally, update the `apt` package index and install Docker:

    $ sudo apt-get update
    $ sudo apt-get install docker-engine

Start the daemon:

    $ sudo service docker start

Now verify that docker is running properly by starting up a test image:

    $ sudo docker run hello-world

This command should print a welcome message verifying that the installation was successful.

## Create a docker container in Google Cloud
You can use docker, without using docker daemon (engine), by using cloud providers.
In this example, you should have a `gcloud` (Google Cloud util), that connected to your account
```
docker-machine create --driver google --google-project `your-project-name` google-machine-type f1-large fm02
```

This example will create a new instance, in your Google Cloud console. Using machine time `f1-large`

## Install Docker on Ubuntu
Docker is supported on the following *64-bit* versions of Ubuntu Linux:

* Ubuntu Xenial 16.04 (LTS)
* Ubuntu Wily 15.10
* Ubuntu Trusty 14.04 (LTS)
* Ubuntu Precise 12.04 (LTS)

A couple of notes:

> The following instructions involve installation using **Docker** packages only, and this ensures obtaining the latest official release of **Docker**. If you need to  install only using `Ubuntu-managed` packages, consult the Ubuntu documentation (Not recommended otherwise for obvious reasons).

> Ubuntu Utopic 14.10 and 15.04 exist in Docker’s APT repository but are no longer officially supported due to known security issues.

**Prerequisites**
* Docker only works on a 64-bit installation of Linux.
* Docker requires Linux kernel version 3.10 or higher (Except for `Ubuntu Precise 12.04`, which requires version 3.13 or higher). Kernels older than 3.10 lack some of the features required to run Docker containers and contain known bugs which cause data loss and frequently panic under certain conditions. Check current kernel version with the command `uname -r`. Check this post if you need to update your `Ubuntu Precise (12.04 LTS)` kernel by scrolling further down. Refer to this [WikiHow][1]
post to obtain the latest version for other Ubuntu installations.

**Update APT sources**

This needs to be done so as to access packages from Docker repository.

1) Log into your machine as a user with `sudo` or `root` privileges.
2) Open a terminal window.
3) Update package information, ensure that APT works with the https method, and that CA certificates are installed.
 

    $ sudo apt-get update
    $ sudo apt-get install apt-transport-https ca-certificates
4) Add the new `GPG` key. This commands downloads the key with the ID `58118E89F3A912897C070ADBF76221572C52609D` from the keyserver `hkp://ha.pool.sks-keyservers.net:80` and adds it to the `adv keychain`. For more information, see the output of `man apt-key`.

        $ sudo apt-key adv \
               --keyserver hkp://ha.pool.sks-keyservers.net:80 \
               --recv-keys 58118E89F3A912897C070ADBF76221572C52609D

5) Find the entry in the table below which corresponds to your Ubuntu version. This determines where APT will search for Docker packages. When possible, run a long-term support (LTS) edition of Ubuntu.

| Ubuntu Version| Repository|
| ------ | ------ |
| Precise 12.04 (LTS)   | `deb https://apt.dockerproject.org/repo ubuntu-precise main`   |
| Trusty 14.04 (LTS)   | `deb https://apt.dockerproject.org/repo ubuntu-trusty main`   |
|Wily 15.10        |`deb https://apt.dockerproject.org/repo ubuntu-wily main`| 
|Xenial 16.04 (LTS)|`deb https://apt.dockerproject.org/repo ubuntu-xenial main`|

> **Note:** Docker does not provide packages for all architectures. Binary artifacts are built nightly, and you can download them from `https://master.dockerproject.org`. To install docker on a multi-architecture system, add an `[arch=...]` clause to the entry. Refer to [Debian Multiarch wiki][2] for details.

6) Run the following command, substituting the entry for your operating system for the placeholder `<REPO>`.

    $ echo "<REPO>" | sudo tee /etc/apt/sources.list.d/docker.list

7) Update the `APT` package index by executing `sudo apt-get update`.

8) Verify that `APT` is pulling from the right repository.

When you run the following command, an entry is returned for each version of Docker that is available for you to install. Each entry should have the URL `https://apt.dockerproject.org/repo/`. The version currently installed is marked with `***`.See the below example's output.

    $ apt-cache policy docker-engine

      docker-engine:
        Installed: 1.12.2-0~trusty
        Candidate: 1.12.2-0~trusty
        Version table:
       *** 1.12.2-0~trusty 0
              500 https://apt.dockerproject.org/repo/ ubuntu-trusty/main amd64 Packages
              100 /var/lib/dpkg/status
           1.12.1-0~trusty 0
              500 https://apt.dockerproject.org/repo/ ubuntu-trusty/main amd64 Packages
           1.12.0-0~trusty 0
              500 https://apt.dockerproject.org/repo/ ubuntu-trusty/main amd64 Packages
    
From now on when you run `apt-get upgrade`, `APT` pulls from the new repository.

**Prerequisites by Ubuntu Version**

For Ubuntu Trusty (14.04) , Wily (15.10) , and Xenial (16.04) , install the `linux-image-extra-*` kernel packages, which allows you use the `aufs` storage driver.

To install the `linux-image-extra-*` packages:

1) Open a terminal on your Ubuntu host.

2) Update your package manager with the command `sudo apt-get update`.

3) Install the recommended packages.

       $ sudo apt-get install linux-image-extra-$(uname -r) linux-image-extra-virtual
4)   Proceed to Docker installation

For Ubuntu Precise (12.04 LTS), Docker requires the 3.13 kernel version. If your kernel version is older than 3.13, you must upgrade it. Refer to this table to see which packages are required for your environment:

| Package| Description|
| ------ | ------ |
| `linux-image-generic-lts-trusty`| Generic Linux kernel image. This kernel has `AUFS` built in. This is required to run Docker.   |
| `linux-headers-generic-lts-trusty`   | Allows packages such as `ZFS` and `VirtualBox guest additions` which depend on them. If you didn’t install the headers for your existing kernel, then you can skip these headers for the `trusty` kernel. If you’re unsure, you should include this package for safety.   |
| `xserver-xorg-lts-trusty`| Optional in non-graphical environments without Unity/Xorg. **Required** when running Docker on machine with a graphical environment.   |
| `ligbl1-mesa-glx-lts-trusty`   | To learn more about the reasons for these packages, read the installation instructions for backported kernels, specifically the [LTS Enablement Stack][3]. Refer to note 5 under each version.   |

To upgrade your kernel and install the additional packages, do the following:

1. Open a terminal on your Ubuntu host.
2. Update your package manager with the command `sudo apt-get update`.
3. Install both the required and optional packages.

       $ sudo apt-get install linux-image-generic-lts-trusty
4. Repeat this step for other packages you need to install.
5. Reboot your host to use the updated kernel using the command `sudo reboot`.
6. After reboot, go ahead and install Docker.

**Install the latest version**

Make sure you satisfy the prerequisites, only then follow the below steps.

> **Note:** For production systems, it is recommended that you [install a specific version][4] so that you do not accidentally update Docker. You should plan upgrades for production systems carefully.

1) Log into your Ubuntu installation as a user with `sudo` privileges. (Possibly running `sudo -su`).

2) Update your APT package index by running `sudo apt-get update`.

3) Install Docker with the command `sudo apt-get install docker-engine`.
4) Start the `docker` daemon with the command `sudo service docker start`.
5) Verify that `docker` is installed correctly by running the hello-world image.

        $ sudo docker run hello-world
This command downloads a test image and runs it in a container. When the container runs, it prints an informational message and exits.

**Manage Docker as a non-root user**

If you don’t want to use `sudo` when you use the docker command, create a Unix group called `docker` and add users to it. When the `docker` daemon starts, it makes the ownership of the Unix socket read/writable by the docker group.

To create the `docker` group and add your user:
1) Log into Ubuntu as a user with `sudo` privileges.
2) Create the `docker` group with the command `sudo groupadd docker`.
3) Add your user to the `docker` group.

        $ sudo usermod -aG docker $USER
4) Log out and log back in so that your group membership is re-evaluated.
5) Verify that you can `docker` commands without `sudo` permission.

        $ docker run hello-world
 
If this fails, you will see an error:

         Cannot connect to the Docker daemon. Is 'docker daemon' running on this host?
Check whether the `DOCKER_HOST` environment variable is set for your shell.

        $ env | grep DOCKER_HOST
If it is set, the above command will return a result. If so, unset it.

        $ unset DOCKER_HOST
You may need to edit your environment in files such as `~/.bashrc` or `~/.profile` to prevent the `DOCKER_HOST` variable from being set erroneously.

 [1]: http://www.wikihow.com/Update-Ubuntu-Kernel
  [2]: https://wiki.debian.org/Multiarch/HOWTO#Setting_up_apt_sources
  [3]: https://wiki.ubuntu.com/Kernel/LTSEnablementStack
  [4]: https://docs.docker.com/engine/installation/linux/ubuntulinux/#/install-a-specific-version

## Installating Docker-ce OR Docker-ee on CentOS
Docker has announced following editions:

-Docker-ee (Enterprise Edition) along with Docker-ce(Community Edition) and Docker (Commercial Support)

This document will help you with installation steps of Docker-ee and Docker-ce edition in CentOS

<h2> Docker-ce Installation </h2>

Following are steps to install docker-ce edition

1. Install yum-utils, which provides yum-config-manager utility:

    <pre>$ sudo yum install -y yum-utils</pre>

2. Use the following command to set up the stable repository:

   <pre>$ sudo yum-config-manager \
    --add-repo \
    https://download.docker.com/linux/centos/docker-ce.repo</pre>

3. Optional: Enable the edge repository. This repository is included in the docker.repo file above but is disabled by default. You can enable it alongside the stable repository.

    <pre> $ sudo yum-config-manager --enable docker-ce-edge </pre>


- You can disable the edge repository by running the `yum-config-manager` command with the `--disable` flag. To re-enable it, use the `--enable` flag. The following command disables the edge repository.
    <pre> $ sudo yum-config-manager --disable docker-ce-edge </pre>

4. Update the yum package index.

    <pre> $ sudo yum makecache fast </pre>

5. Install the docker-ce using following command:

    <pre> $ sudo yum install docker-ce-17.03.0.ce </pre>

6. Confirm the Docker-ce fingerprint

    `060A 61C5 1B55 8A7F 742B 77AA C52F EB6B 621E 9F35`

    If you want to install some other version of docker-ce you can use following command:
    <pre>$ sudo yum install docker-ce-VERSION</pre>
   Specify the `VERSION` number

7. If everything went well the docker-ce is now installed in your system, use following command to start:

    <pre> $ sudo systemctl start docker </pre>

8. Test your docker installation:

    <pre> $ sudo docker run hello-world </pre>

    you should get following message:

    <pre> Hello from Docker! 
    This message shows that your installation appears to be working correctly. </pre>


----------

<h2>-Docker-ee (Enterprise Edition) Installation</h2>

For Enterprise Edition (EE) it would be required to signup, to get your &lt;DOCKER-EE-URL&gt;.

1. To signup go to https://cloud.docker.com/. Enter your details and confirm your email id. After confirmation you would be given a &lt;DOCKER-EE-URL&gt;, which you can see in your dashboard after clicking on setup.

2. Remove any existing Docker repositories from `/etc/yum.repos.d/`

3. Store your Docker EE repository URL in a yum variable in `/etc/yum/vars/`. Replace &lt;DOCKER-EE-URL&gt; with the URL you noted down in the first step.

    <pre> $ sudo sh -c 'echo "&lt;DOCKER-EE-URL&gt;" > /etc/yum/vars/dockerurl' </pre>

4. Install yum-utils, which provides the yum-config-manager utility:

    <pre> $ sudo yum install -y yum-utils</pre>

5. Use the following command to add the stable repository:

    <pre> $ sudo yum-config-manager \
    --add-repo \
    &lt;DOCKER-EE-URL&gt;/docker-ee.repo</pre>

6. Update the yum package index.

    <pre> $ sudo yum makecache fast </pre>

7. Install docker-ee

    <pre> sudo yum install docker-ee </pre>

8. You can start the docker-ee using following command:

    <pre>$ sudo systemctl start docker </pre>

