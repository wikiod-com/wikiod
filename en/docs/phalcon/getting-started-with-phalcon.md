---
title: "Getting started with phalcon"
slug: "getting-started-with-phalcon"
draft: false
images: []
weight: 1
type: docs
toc: true
---

## Installation
Download installation files from Phalcon [dedicated download page](https://phalconphp.com/en/download), as well as finding manuals on making Phalcon work with popular platforms.

# Windows
Put the actual [DLL files](https://phalconphp.com/en/download/windows) in a directory proper to extend PHP functionality. For XAMPP use `xampp\php\ext\` - and for WAMP  use `wamp\bin\php\php*\ext\` derectory. Then enable Phalcon by adding `extension=php_phalcon.dll` to the appropriate `php.ini` file. Restart the web server and Phalcon should become available.

# Linux platforms
To compile the desired version of Phalcon, first install PHP sources along with some other necessary tools:

    #Ubuntu
        sudo apt-get install php5-dev php5-mysql gcc libpcre3-dev
    
    #Fedora
        sudo yum install php-devel php-mysqlnd gcc libtool
    
    #RHEL
        sudo yum install php-devel php-mysql gcc libtool
    
    #Suse
        yast2 -i php5-pear php5-devel php5-mysql gcc
    
    #OS X (Using Homebrew)
        brew tap homebrew/dupes
        brew tap homebrew/versions
        brew tap homebrew/php
        brew install php5x php5x-phalcon # Where "x" - minor number of PHP

After they are all properly installed, Phalcon can be compiled:

    git clone --depth=1 git://github.com/phalcon/cphalcon.git
    cd cphalcon/build
    sudo ./install

(Pick the desired version instead of using just `git://github.com/phalcon/cphalcon.git`) Afterwards the Phalcon extension should be available in the PHP directories. All that's left is to include `extension=phalcon.so` in the desired `php.ini` file. Restart the web server and it should be available.

## Ubuntu users
It is possible to install Phalcon directly from repositories using following commands:

    sudo apt-add-repository ppa:phalcon/stable
    sudo apt-get update
    sudo apt-get install php5-phalcon

Mac OS X
========

Homebrew
--------
If you have brew installed you first need to tap homebrew-php:

    brew tap homebrew/homebrew-php

After that you need to determine your PHP version. This can be done via the command:

    php -v

The command will output something similar to `PHP 5.6.22` you want the first and second numbers, which are `5` and `6` in this case. Then you run the following command to install the proper version (replacing `5` and `6` with the version you have):

    brew install php56-phalcon

Sources:
 - https://docs.phalconphp.com/en/latest/reference/install.html#mac-os-x

