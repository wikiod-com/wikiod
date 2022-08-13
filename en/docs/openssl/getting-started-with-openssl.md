---
title: "Getting started with openssl"
slug: "getting-started-with-openssl"
draft: false
images: []
weight: 1
type: docs
toc: true
---

## Installation or Setup
 OpenSSL is an open source project that provides a robust, commercial-grade, and full-featured toolkit for the Transport Layer Security (TLS) and Secure Sockets Layer (SSL) protocols. It is also a general-purpose cryptography library. 

The OpenSSL toolkit is licensed under an Apache-style license, which basically means that you are free to get and use it for commercial and non-commercial purposes subject to some simple license conditions. 

## Build and Install openssl on Linux/Unix Systems
## Overview ##

These instructions are for acquiring, building, and installing openssl from source.  Openssl is usually included in package managers as well.

## Resources ##

https://github.com/openssl/openssl 

## Dependencies ##

* make
* perl 5
* gcc/clang
* git

Dependencies can be installed through a package manager such as apt, dnf, or brew.

## Steps ##

```
$ cd ~/path/to/projects
$ git clone https://github.com/openssl/openssl.git
$ cd openssl
$ ./config
$ make
$ make test
$ sudo make install
```

By default, openssl will be installed to /usr/local.  

## Verify ##
```
$ openssl --version
```

You now have a default build of openssl installed to your machine.

## (De-)Initialization of openssl library
## Overview ##
Openssl consists of 2 libraries: `libcrypto` and `libssl`. Before openssl API can be used in an application, mandatory initialization procedures are expected to be performed. Once application is done with openssl related work, it is expected to cleanup allocated resources.

Code below does complete initialization, however, developer is free to initialize only openssl stuff he is interested in.

## Initialize libcrypto ##

    ERR_load_crypto_strings();
    OpenSSL_add_all_algorithms();
    OPENSSL_config(NULL); // Load default configuration (e.g. openssl.conf)

## Initialize libssl ##

    OPENSSL_init_ssl(0, NULL);

## Deinitialize ##

    CONF_modules_unload(1);
    EVP_cleanup();
    CRYPTO_cleanup_all_ex_data();
    ERR_remove_state();
    ERR_free_strings();

## Run OpenSSL on Windows without Installing
This workaround helped us so much at my job (Tech Support), we made a simple batch file we could run from anywhere (We didnt have the permissions to install the actual exe). This workaround will run OpenSSL and open up the bin folder for you (cause this is where any files you create or modify will be saved).

## How to Set Up:
1. Download the OpenSSL binaries [here][1]. (Note that this is confirmed to work with version 0.9.8h.)
2. Copy this code to a file named StartOpenSSL.bat. Save this to a location of your choice. It can be run from anywhere.

        @echo off
        title OpenSSL

        cd\openssl\bin

        if exist "C:\openssl\share\openssl.cnf" (

        set OPENSSL_CONF=c:/openssl/share/openssl.cnf
        start explorer.exe c:\openssl\bin

        echo Welcome to OpenSSL

        openssl

        ) else (
    
        echo Error: openssl.cnf was not found
        echo File openssl.cnf needs to be present in c:\openssl\share
        pause

        )

        exit
3. Once you have downloaded the OpenSSL binaries, extract them to your C drive in a folder titled OpenSSL. (The path needs to be C:\OpenSSL). Do not move any of the folders contents around, just extract them to the folder.
4. You are ready to use OpenSSL. This is a great workaround for Windows users who dont have the privileges to install it as it requires no permissions. Just run the bat file from earlier by double clicking it.
  [1]: http://gnuwin32.sourceforge.net/packages/openssl.htm

## OpenSSL commands examples
Inspect ssl certificate

    openssl x509 -in server.crt -noout -text

Generate server key

    openssl genrsa -out server.key 2048

Generate csr

    openssl req -out server.csr -key server.key -new

