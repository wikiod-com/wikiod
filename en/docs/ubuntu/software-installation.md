---
title: "Software Installation"
slug: "software-installation"
draft: false
images: []
weight: 9990
type: docs
toc: true
---

## Syntax
- sudo apt-get install <package_name>

- sudo apt-get install <package1_name> <package2_name> <package3_name> 

- apt-get [options] [-o config=string] [-c=cfgfile] command [pkg]

## Parameters
| Command | Description |
| ------ | ------ |
| update   | Used to re-synchronize the package index files from their sources. An update should always be performed before an upgrade or dist-upgrade.  |
| upgrade   | Used to install the newest versions of all packages currently installed on the system. |
| install | This option is followed by one or more packages desired for installation. |
| remove | Identical to install except that packages are removed instead of installed. |
| ------ | ------ |
| **Options** | **Description** |
| -y, --yes, --assume-yes | Automatic yes to prompts. Assume "yes" as answer to all prompts and run non-interactively. |
| -h, --help | Show a short usage summary. |

## Install software using APT
Installing software via APT (Advanced Package Tool) also know as 'apt-get'.
To install Mozilla Firefox:

 1. Open a Terminal (<kbd>Ctrl</kbd>+<kbd>Alt</kbd>+<kbd>T</kbd>)
 2. Type `sudo apt-get install firefox`
 3. Hit <kbd>Enter</kbd>
 4. When it asks to install type 'Y' to confirm.

Software will be downloaded and installed.






## List all installed packages
To list all the packages installed in ubuntu, type below command
    
    $ apt list --installed

Output will show all the installed packages.

   

    Listing... Done
       accountsservice/trusty-updates,now 0.6.35-0ubuntu7.3 i386 [installed]
       acl/trusty,now 2.2.52-1 i386 [installed,automatic]
       acpid/trusty,now 1:2.0.21-1ubuntu2 i386 [installed]
       adduser/trusty,now 3.113+nmu3ubuntu3 all [installed]
       apparmor/trusty-updates,trusty-security,now 2.10.95-0ubuntu2.6~14.04.1 i386 [installed]
       apport/trusty-security,now 2.14.1-0ubuntu3.23 all [installed,upgradable to: 2.14.1-0ubuntu3.24]
       apport-symptoms/trusty,now 0.20 all [installed]
       apt/trusty-updates,trusty-security,now 1.0.1ubuntu2.17 i386 [installed]
       apt-transport-https/trusty-updates,trusty-security,now 1.0.1ubuntu2.17 i386 [installed]
       apt-utils/trusty-updates,trusty-security,now 1.0.1ubuntu2.17 i386 [installed]
       apt-xapian-index/trusty,now 0.45ubuntu4 all [installed]
       aptitude/trusty,now 0.6.8.2-1ubuntu4 i386 [installed]

