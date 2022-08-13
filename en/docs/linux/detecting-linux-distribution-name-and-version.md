---
title: "Detecting Linux distribution name and version"
slug: "detecting-linux-distribution-name-and-version"
draft: false
images: []
weight: 9731
type: docs
toc: true
---

## Syntax
- **uname** - to print information about your operating system.

    uname [OPTION]

## Detect what debian-based distribution you are working in
Just execute `lsb_release -a`.

On Debian:

    $ lsb_release -a
    No LSB modules are available.
    Distributor ID: Debian
    Description:    Debian GNU/Linux testing (stretch)
    Release:        testing
    Codename:       stretch

On Ubuntu:

    $ lsb_release -a
    No LSB modules are available.
    Distributor ID: Ubuntu
    Description:    Ubuntu 14.04.4 LTS
    Release:        14.04
    Codename:       trusty

In case when you don't have `lsb_release` installed you may want to try some guessing, for example, there is a file `/etc/issue` that often contains distribution name. For example, on ubuntu:

    $ cat /etc/issue
    Ubuntu 12.04.5 LTS \n \l

Don't use file `/etc/debian_version` because its contents do not match distribution name!

_Note that this will also work on non-Debian-family distributions like Fedora, RHEL, or openSUSE — but that `lsb_release` may not be installed._

## Detect what systemd-based distribution you are using
This method will work on modern versions of Arch, CentOS, CoreOS, Debian, Fedora, Mageia, openSUSE, Red Hat Enterprise Linux, SUSE Linux Enterprise Server, Ubuntu, and others. This wide applicability makes it an ideal as a first approach, with fallback to other methods if you need to also identify older systems.

Look at `/etc/os-release`. In specific, look at variables `NAME`, `VERSION`, `ID`, `VERSION_ID`, and `PRETTY_NAME`.

On Fedora, this file might look like:

        NAME=Fedora
        VERSION="24 (Workstation Edition)"
        ID=fedora
        VERSION_ID=24
        PRETTY_NAME="Fedora 24 (Workstation Edition)"
        ANSI_COLOR="0;34"
        CPE_NAME="cpe:/o:fedoraproject:fedora:24"
        HOME_URL="https://fedoraproject.org/"
        BUG_REPORT_URL="https://bugzilla.redhat.com/"
        REDHAT_BUGZILLA_PRODUCT="Fedora"
        REDHAT_BUGZILLA_PRODUCT_VERSION=24
        REDHAT_SUPPORT_PRODUCT="Fedora"
        REDHAT_SUPPORT_PRODUCT_VERSION=24
        PRIVACY_POLICY_URL=https://fedoraproject.org/wiki/Legal:PrivacyPolicy
        VARIANT="Workstation Edition"
        VARIANT_ID=workstation
    
On CentOS, this file might look like this:
    
        NAME="CentOS Linux"
        VERSION="7 (Core)"
        ID="centos"
        ID_LIKE="rhel fedora"
        VERSION_ID="7"
        PRETTY_NAME="CentOS Linux 7 (Core)"
        ANSI_COLOR="0;31"
        CPE_NAME="cpe:/o:centos:centos:7"
        HOME_URL="https://www.centos.org/"
        BUG_REPORT_URL="https://bugs.centos.org/"
        
        CENTOS_MANTISBT_PROJECT="CentOS-7"
        CENTOS_MANTISBT_PROJECT_VERSION="7"
        REDHAT_SUPPORT_PRODUCT="centos"
        REDHAT_SUPPORT_PRODUCT_VERSION="7"

This file is [documented on the freedesktop web site](https://www.freedesktop.org/software/systemd/man/os-release.html); in principle, it is not systemd specific — but it will exist on all systemd-based distributions.

From the bash shell, one can source the `/etc/os-release` file and then use the various variables directly, like this:

    $ ( source /etc/os-release && echo "$PRETTY_NAME" )
    Fedora 24 (Workstation Edition)



## Detect what RHEL / CentOS / Fedora distribution you are working in
Look at the contents of `/etc/redhat-release`

    cat /etc/redhat-release

Here is the output from a Fedora 24 machine:  `Fedora release 24 (Twenty Four)`

----

As mentioned in the debian-based response, you can also use the `lsb_release -a` command, which outputs this from a Fedora 24 machine:

    LSB Version:    :core-4.1-amd64:core-4.1-noarch:cxx-4.1-amd64:cxx-4.1-noarch:desktop-4.1-amd64:desktop-4.1-noarch:languages-4.1-amd64:languages-4.1-noarch:printing-4.1-amd64:printing-4.1-noarch
    Distributor ID:    Fedora
    Description:    Fedora release 24 (Twenty Four)
    Release:    24
    Codename:    TwentyFour



## Uname - Print information about the current system

**Uname** is the short name for **u**nix **name**. Just type `uname` in console to get information about your operating system.


    uname [OPTION]

If no *OPTION* is specified, `uname` assumes the `-s` option.

`-a` or `--all` -   Prints all information, omitting `-p` and `-i` if the information is unknown.

## Example:

    > uname -a

    SunOS hope 5.7 Generic_106541-08 sun4m sparc SUNW,SPARCstation-10

All the options:

<table>
        <tr>
          <td><b>-s</b>, <b>--kernel-name</b></td>
          <td>Print the kernel name.</td>
        </tr>
        <tr>
          <td><b>-n</b>, <b>--nodename</b></td>
          <td>Print the <a href="../jargon/n/network.htm">network</a> <a href="../jargon/h/hostcomp.htm">node</a> <a href="../jargon/h/hostname.htm">hostname</a>.</td>
        </tr>
        <tr>
          <td><b>-r</b>, <b>--kernel-release</b></td>
          <td>Print the <a href="../jargon/k/kernel.htm">kernel</a> <a href="../jargon/r/release.htm">release</a>.</td>
        </tr>
        <tr>
          <td><b>-v</b>, <b>--kernel-version</b></td>
          <td>Print the kernel version.</td>
        </tr>
        <tr>
          <td><b>-m</b>, <b>--machine</b></td>
          <td>Print the machine hardware name.</td>
        </tr>
        <tr>
          <td><b>-p</b>, <b>--processor</b></td>
          <td>Print the processor type, or "<b>unknown</b>".</td>
        </tr>
        <tr>
          <td><b>-i</b>, <b>--hardware-platform</b></td>
          <td>Print the hardware platform, or "<b>unknown</b>".</td>
        </tr>
        <tr>
          <td><b>-o</b>, <b>--operating-system</b></td>
          <td>Print the operating system.</td>
        </tr>
        <tr>
          <td><b>--help</b></td>
          <td>Display a help message, and exit.</td>
        </tr>
        <tr>
          <td><b>--version</b></td>
          <td>Display version information, and exit.</td>
        </tr>
      </table>


## Detect basic informations about your distro
just execute `uname -a`.

On Arch:

    $ uname -a
    Linux nokia 4.6.4-1-ARCH #1 SMP PREEMPT Mon Jul 11 19:12:32 CEST 2016 x86_64        GNU/Linuxenter code here

## find your linux os (both debian & rpm) name and release number
Most of linux distros stores its version info in the /etc/lsb-release (debian) or /etc/redhat-release (RPM based) file. Using below generic command should get you past most of the Debian and RPM derivatives as Linux Mint and Cent-Os. 

Example on Ubuntu Machine:

# cat /etc/*release
DISTRIB_ID=Ubuntu
DISTRIB_RELEASE=14.04
DISTRIB_CODENAME=trusty
DISTRIB_DESCRIPTION="Ubuntu 14.04 LTS"

## using GNU coreutils
So the GNU coreutils should be avaialable on all linux based systems (please correct me if I am wrong here).

If you do not know what system you are using you may not be able to directly jump to one of the examples above, hence this may be your first port of call.

`$ uname -a

On my system this gives me the following...

`Linux Scibearspace 3.16.0-4-amd64 #1 SMP Debian 3.16.7-ckt25-2+deb8u3 (2016-07-02) x86_64 GNU/Linux

Here you can see the following :

Scibearspace : the name of my pc

 - Scibearspace : the name of my pc
 - 3.16.0-4-amd64 : the kernel and architecture
 - SMP Debian 3.16.7-CKT25-2+deb8u3 : tells me I am running debian with the 3.16  kernel
 - Finaly the last part I am running debian 8 (update 3).

I would welcome any others to add in results for RHEL, and SuSe systems.

