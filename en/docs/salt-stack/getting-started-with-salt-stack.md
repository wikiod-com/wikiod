---
title: "Getting started with salt-stack"
slug: "getting-started-with-salt-stack"
draft: false
images: []
weight: 1
type: docs
toc: true
---

## Installation or Setup
Salt can be installed via :

- `salt-bootstrap`: a shell script, that can install salt (client and/or master packages) on standard Unix/Linux platform,
- Platform Specific binaries: available for Windows, Mac OS X and Linux,
- Package Management systems: available for pacman, apt-get, yum and other package management systems. 

# Salt Installation via Salt-Bootstrap

Download Salt:

- via curl: ```curl -o bootstrap_salt.sh -L https://bootstrap.saltstack.com ```
- via wget: ```wget -O bootstrap_salt.sh https://bootstrap.saltstack.com ```

`bootstrap_salt.sh` provides many options. Among them executing the script with:

- `-M` flag will install `salt-master`,
- `-N` flag will not install `salt-minion` package,
- `-A` flag can be used to define the `salt-master` ip. 

It can be invoked with the version of salt to be installed.

Common pattern are:

- `sh bootstrap_salt.sh -M -N stable` to install a "stable" salt master,
- `sh bootstrap_salt.sh -A <ip> stable` to install a "stable" salt minion, with the master IP defined.

Official Documentation can be found [here](https://docs.saltstack.com/en/latest/topics/tutorials/salt_bootstrap.html). 

# Salt Installation via platform specific binaries:

Latest stable installers can be found here: 

- [Windows](https://docs.saltstack.com/en/latest/topics/installation/windows.html)
- [Mac OS](https://docs.saltstack.com/en/latest/topics/installation/osx.html)

# Salt Installation via package management systems

Under Ubuntu 16.04

- add the saltstack repository key to APT via:
```wget -O - https://repo.saltstack.com/apt/ubuntu/16.04/amd64/latest/SALTSTACK-GPG-KEY.pub | sudo apt-key add -```
- add the repository in apt configuration via ```sudo echo "deb http://repo.saltstack.com/apt/ubuntu/16.04/amd64/latest denial main" > /etc/apt/sources.list.d/saltstack.list```
- update your apt cache via ```apt-get update```

If you wish to install a salt-master run `apt-get install salt-master`, otherwise `apt-get install salt-minion`.

Instructions for other OSes can be found [here](https://docs.saltstack.com/en/latest/topics/installation/index.html)



## Command Syntax
Salt commands are executed using a common structure:

    salt '*'      pkg.install       vim
         [target] [module.function] [arguments]

The **target** determines which systems apply the command. In the example above we target all (`'*'`) the Salt minions. See the [targeting minions](https://docs.saltstack.com/en/latest/topics/targeting/index.html) documentation for more information about targeting Salt minions.

The **command (module.function)** is the function to execute. In the above example we use the `pkg.install` function to tell the targets to install a package.

The **arguments** provide any extra data that is needed by the function you are calling. In the example above we tell the `pkg.install` function to install the package named `vim`.

## Version Numbers
As of Salt version `2014.1.0`, Salt uses a date based system for version numbers. Version numbers are in the format `YYYY.MM.R`. The year (`YYYY`) and month (`MM`) indicate when the release was created. The bugfix release number (`R`) increments within that feature release.

In order to distinguish future releases from the current release, code names are used. Salt uses the periodic table to derive the next codename. The first release in the date based system was code name `Hydrogen`, each subsequent release will go to the next atomic release.

