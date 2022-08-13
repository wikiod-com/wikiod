---
title: "Incubator"
slug: "incubator"
draft: false
images: []
weight: 9990
type: docs
toc: true
---

## Introduction
The [Phalcon Incubator][1] can be used by the community to experiment with new features or expand onto the existing Phalcon adapters, prototypes or functionalities.

Anything in the Incubator can be potentially corporated into the framework.

Github repository: https://github.com/phalcon/incubator

  [1]: https://github.com/phalcon/incubator

## Installation
# Installation via Composer #
The easiest way to install the Incubator is by using [Composer][1].

Install Composer and create a new `composer.json` file in the root of your project.

    |-- app
    |-- public
    |   `-- index.php
    |-- vendor
    |-- composer.json

Add the following content to the `composer.json` file. If you are still using Phalcon 2.0.x

    {
        "require": {
            "phalcon/incubator": "^2.0"
        }
    }

If you are using Phalcon 3.0.0

    {
        "require": {
            "phalcon/incubator": "~3.0"
        }
    }


After altering the `composer.json` file, you need to run the following command, from the root of your project.

    $ php composer.phar install

If you already installed your files and you would like to update them instead. Then use `update` instead of `install`.  
By default Composer will create a new folder named `vendor` in your project root and download all the requested files into this directory.

After composer has been installed, your document structure should look something like this:

    |-- app
    |-- public
    |   `-- index.php
    |-- vendor
    |   `-- phalcon
    |       `-- incubator
    |           `-- docs
    |           `-- Library
    |           `-- tests
    |-- composer.json

# Installation via Github #

Create a folder named `vendor` in your project root directory. And also create the folder `phalcon` inside this folder. 

    |-- app
    |-- public
    |   `-- index.php
    |-- vendor
    |   `-- phalcon


Now navigate inside the `phalcon` folder and clone the Incubator from the Github repository.

    git clone https://github.com/phalcon/incubator.git

By default, the above command will download the latest version of Phalcon. If you'd like to download an earlier version you can simply add the `--branch` parameter to the command, followed by the required branch version.

    git clone https://github.com/phalcon/incubator.git --branch 2.0.9

# Installation via the manual way #

If the above methods are confusing for you and you like to do stuff manually, you can easily [download the repository from Github][2] and place the files inside the `vendor/phalcon/`, in your project root.

    |-- app
    |-- public
    |   `-- index.php
    |-- vendor
    |   `-- phalcon


  [1]: https://www.wikiod.com/composer-php/getting-started-with-composer-php
  [2]: https://github.com/phalcon/incubator/archive/master.zip

## Usage
# Loading the Incubator into your project

Add the following lines of code to your loader file

    $loader = new Phalcon\Loader();
    
    $loader->registerNamespaces([
        'Phalcon' => '/path/to/your/vendor/phalcon/incubator/Library/Phalcon/',
        // any other namespaces you have loaded
        // ...
    ]);
    
    $loader->register();

Now you can access all the Incubator functionalities by using the normal Phalcon namespaces:

    \Phalcon\Acl\Adapter\Database;

