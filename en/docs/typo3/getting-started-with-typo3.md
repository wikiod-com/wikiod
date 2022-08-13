---
title: "Getting started with typo3"
slug: "getting-started-with-typo3"
draft: false
images: []
weight: 1
type: docs
toc: true
---

## Classic installation on a Unix server
Get the Source Package from http://typo3.org/download/ and upload this package to your web server. Put it one level above the document root.

For this manual, we will use the .tar.gz file. Use the shell to execute the according commands:

    /var/www/site/htdocs/$ cd ..
    /var/www/site/$ wget get.typo3.org/7.6 -O typo3_src-7.6.x.tar.gz

Unpack the typo3_src-7.6.x.tar.gz file on your web server:

    /var/www/site/$ tar -xzf typo3_src-7.6.x.tar.gz

Create these symlinks in your document root:

    cd htdocs
    ln -s ../typo3_src-7.6.x typo3_src
    ln -s typo3_src/typo3 typo3
    ln -s typo3_src/index.php index.php

## TYPO3 CMS
TYPO3 is the leading enterprise content management system. It focuses on providing the features that professionals need to build and maintain large and/or complicated websites across multiple devices (mobile and desktop).


**Features**
- Open Source
- Multilingual
- Frequent security updates - safety first
- Manage as many websites as you want from a single installation
- Highly extendable through a large library of free third-party extensions.
- Expandable through its API based framework
- Professional support by numerous hosting companies, web agencies and freelancers

