---
title: "Build from source"
slug: "build-from-source"
draft: false
images: []
weight: 9959
type: docs
toc: true
---

Examples below are not necessarily in the correct order. See the Remarks section below for more information on the whole process.

# Overview of the whole process

1. Download the latest source code.
2. Configure the kernel.
3. Build the world and the kernel.
4. Configure the root filesystem of your new FreeBSD.
5. Install the world and the kernel.

# Get the number of processors

An easy way to speed up the process of building and installing the new system is to use more processors to increase the computational power.

To find out what's the number of the processors you have to speed up the process:

 ```sh
 sysctl hw.ncpu
 ```

 For example:

 >  ```text
 >  hw.ncpu: 1
 >  ```

 Let's set the `$NUMBER_OF_PROCESSORS` environmental variable then:

 ```sh
 export $NUMBER_OF_PROCESSORS=$(sysctl hw.ncpu | tr -d 'a-z.: ')
 ```


## Download the latest source code
# SVN

FreeBSD project use [SVN][1] as default SCM. Source could be download with [`svnlite`][2] software.

## Get Current

```sh
cd /usr/src
svnlite checkout https://svn.freebsd.org/base/head .
```

## Get Releases

```sh
cd /usr/src
svnlite checkout https://web.freebsd.org/base/release/11.0.0 .
```
# Tarball (http & ftp)

You can also get source from frozen tarball with [`fetch`][3] command

## http

```sh
cd /tmp
fetch http://ftp.freebsd.org/pub/FreeBSD/releases/amd64/11.0-RELEASE/src.txz
cd /usr/src
tar xJvf /tmp/src.txz
```

## ftp

```sh
cd /tmp
fetch ftp://ftp.freebsd.org/pub/FreeBSD/releases/amd64/11.0-RELEASE/src.txz
cd /usr/src
tar xJvf /tmp/src.txz
```


# Git

## GitHub

```sh
git clone https://github.com/freebsd/freebsd freebsdsrc
```


  [1]: https://svn.freebsd.org/
  [2]: https://www.freebsd.org/cgi/man.cgi?query=svnlite&apropos=0&sektion=0&manpath=FreeBSD%2011.0-stable&arch=default&format=html
  [3]: https://www.freebsd.org/cgi/man.cgi?query=fetch

## Configure the kernel
1. Go to the directory with the source code:

   ```sh
   cd freebsdsrc
   ```

2. Go to the directory with the kernel's configuration code:

   ```sh
   # If your system is 32-bit.
   cd sys/i386/conf/
   # If your system is 64-bit.
   cd sys/amd64/conf/
   ```

3. Get a copy of the **GENERIC** kernel (let's call it _MODEDKERNEL_). It will be
the base of your customisations.

   ```sh
   cp GENERIC MODEDKERNEL
   ```

4. Modify the `MODEDKERNEL` file at your will.

## Build the world and the kernel
# Build the world

Go to the `freebsdsrc/` (the root directory of the FreeBSD source tree you've already downloaded) and build the world:

```sh
sudo make -j${NUMBER_OF_PROCESSORS} buildworld KERNCONF=MODEDKERNEL -DNO_CLEAN
```

## Estimated time

- _Estimated time on Hasee Q540S running on a one processor: 8 hours._
- _Estimated time on Dell L702X running on 8 processors: 98 minutes._

# Build the kernel

To build the kernel run:

```sh
sudo make -j${NUMBER_OF_PROCCESORS} buildkernel KERNCONF=UFFIE -DNO_CLEAN
```

## Estimated time

- _Estimated time on Hasee Q540S running on a one processor: 2 hours._
- _Estimated time on Dell L702X running on 8 processors: 19 minutes._




## Configure the root filesystem of your new FreeBSD
Let's configure the destination directory for the root filesystem of
your new FreeBSD (for example `/usr/home/beastie/MODEDKERNEL`). 

1. Add the following lines to `/etc/src.conf` to set it up:

   ```text
   .if ${KERNCONF} == "MODEDKERNEL"
       DESTDIR?=/usr/home/beastie/MODEDKERNEL
       MODULES_OVERRIDE=md ufs
   .endif
   ```

   _Remember to use spaces not tabs if you wish to indent the code._

2. Create the root file system now:

   - Make distribution directories:

     ```sh
     sudo make distrib-dirs KERNCONF=MODEDKERNEL
     ```

     _Estimated time on Hasee Q540S: a few seconds._
 
   - Make the distribution:

     ```sh
     sudo make distribution KERNCONF=UFFIE
     ```

     _Estimated time on Hasee Q540S: 3 minutes._

## Install the world and the kernel
# Install the world

```sh
sudo make installworld KERNCONF=MODEDKERNEL
```

_Estimated time on Hasee Q540S: 5 minutes._

# Install the kernel

```sh
sudo make installkernel KERNCONF=MODEDKERNEL
```

_Estimated time on Hasee Q540S: a few seconds._


