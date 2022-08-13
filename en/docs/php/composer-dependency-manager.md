---
title: "Composer Dependency Manager"
slug: "composer-dependency-manager"
draft: false
images: []
weight: 9604
type: docs
toc: true
---

[Composer](https://getcomposer.org) is PHP's most commonly used dependency manager. It's analogous to `npm` in Node, `pip` for Python, or `NuGet` for .NET.

## Syntax
 - php path/to/composer.phar [command] [options] [arguments]

## Parameters
| Parameter | Details |
| ------ | ------ |
| license | Defines the type of license you want to use in the Project. |
| authors | Defines the authors of the project, as well as the author details. |
| support | Defines the support emails, irc channel, and various links. |
| require | Defines the actual dependencies as well as the package versions. |
| require-dev | Defines the packages necessary for developing the project. |
| suggest | Defines the package suggestions, i.e. packages which can help if installed. |
| autoload | Defines the autoloading policies of the project. |
| autoload-dev | Defines the autoloading policies for developing the project. |

Autoloading will only work for libraries that specify autoload information. Most libraries do and will adhere to a standard such as [PSR-0](http://www.php-fig.org/psr/psr-0/) or [PSR-4](http://www.php-fig.org/psr/psr-4/).

## Helpful Links

 - [Packagist](https://packagist.org) – Browse available packages (which you can install with Composer).
 - [Official Documentation](https://getcomposer.org/doc/04-schema.md)
 - [Official Getting Started guide](https://getcomposer.org/doc/00-intro.md)

## Few Suggestions

1. Disable xdebug when running Composer.
2. Do not run Composer as `root`. Packages are not to be trusted.

## Autoloading with Composer
While composer provides a system to manage dependencies for PHP projects (e.g. from [Packagist](https://packagist.org/)), it can also notably serve as an autoloader, specifying where to look for specific namespaces or include generic function files.

It starts with the `composer.json` file:

    {
        // ...
        "autoload": {
            "psr-4": {
                "MyVendorName\\MyProject": "src/"
            },
            "files": [
                "src/functions.php"
            ]
        },
        "autoload-dev": {
            "psr-4": {
                "MyVendorName\\MyProject\\Tests": "tests/"
            }
        }
    }

This configuration code ensures that all classes in the namespace `MyVendorName\MyProject` are mapped to the `src` directory and all classes in `MyVendorName\MyProject\Tests` to the `tests` directory (relative to your root directory). It will also automatically include the file `functions.php`.

After putting this in your `composer.json` file, run `composer update` in a terminal to have composer update the dependencies, the lock file and generate the `autoload.php` file. When deploying to a production environment you would use `composer install --no-dev`. The `autoload.php` file can be found in the `vendor` directory which should be generated in the directory where `composer.json` resides.

You should `require` this file early at a setup point in the lifecycle of your application using a line similar to that below.

    require_once __DIR__ . '/vendor/autoload.php';

Once included, the `autoload.php` file takes care of loading all the dependencies that you provided in your `composer.json` file.


Some examples of the class path to directory mapping:

 - `MyVendorName\MyProject\Shapes\Square` ➔ `src/Shapes/Square.php`.
 - `MyVendorName\MyProject\Tests\Shapes\Square` ➔ `tests/Shapes/Square.php`.


## What is Composer?
[Composer](https://getcomposer.org/) is a dependency/package manager for PHP. It can be used to install, keep track of, and update your project dependencies. Composer also takes care of autoloading the dependencies that your application relies on, letting you easily use the dependency inside your project without worrying about including them at the top of any given file.

Dependencies for your project are listed within a `composer.json` file which is typically located in your project root. This file holds information about the required versions of packages for production and also development.

A full outline of the `composer.json` schema can be found on the [Composer Website](https://getcomposer.org/doc/04-schema.md).

This file can be edited manually using any text-editor or automatically through the command line via commands such as `composer require <package>` or `composer require-dev <package>`.

To start using composer in your project, you will need to create the `composer.json` file. You can either create it manually or simply run `composer init`. After you run `composer init` in your terminal, it will ask you for some basic information about your project: **Package name** (*vendor/package* - e.g. `laravel/laravel`), **Description** - *optional*, **Author** and some other information like Minimum Stability, License and Required Packages.

The `require` key in your `composer.json` file specifies Composer which packages your project depends on. `require` takes an object that maps package names (e.g. *monolog/monolog*) to version constraints (e.g. *1.0.**).

    {
        "require": {
            "composer/composer": "1.2.*"
        }
    }

To install the defined dependencies, you will need to run the `composer install` command and it will then find the defined packages that matches the supplied `version` constraint and download it into the `vendor` directory. It's a convention to put third party code into a directory named `vendor`.

You will notice the `install` command also created a `composer.lock` file.

A `composer.lock` file is automatically generated by Composer. This file is used to track the currently installed versions and state of your dependencies.  Running `composer install` will install packages to exactly the state stored in the lock file.



## Difference between 'composer install' and 'composer update'
## `composer update`

`composer update` will update our dependencies as they are specified in `composer.json`.

For example, if our project uses this configuration:

    "require": {
        "laravelcollective/html": "2.0.*"
    }

Supposing we have actually installed the `2.0.1` version of the package, running `composer update` will cause an upgrade of this package (for example to `2.0.2`, if it has already been released).

In detail `composer update` will:

 - Read `composer.json`    
 - Remove installed packages that are no more required in
   `composer.json`
 - Check the availability of the latest versions of our required
   packages
 - Install the latest versions of our packages
 - Update `composer.lock` to store the installed packages version

## `composer install`

`composer install` will install all of the dependencies as specified in the `composer.lock` file at the version specified (locked), without updating anything.

In detail:

 - Read `composer.lock` file
 - Install the packages specified in the `composer.lock` file

## When to install and when to update

* `composer update` is mostly used in the 'development' phase, to upgrade our project packages.

* `composer install` is primarily used in the 'deploying phase' to install our application on a production server or on a testing environment, using the same dependencies stored in the `composer.lock` file created by `composer update`.


## Composer Available Commands
| Command | Usage |
| ------ | ------ |
| about   | Short information about Composer   |
| archive |        Create an archive of this composer package
|  browse |         Opens the package's repository URL or homepage in your browser.
|  clear-cache |     Clears composer's internal package cache.
|  clearcache   |   Clears composer's internal package cache.
|  config  |        Set config options
| create-project|  Create new project from a package into given directory.
|  depends  |       Shows which packages cause the given package to be installed
|  diagnose  |      Diagnoses the system to identify common errors.
|  dump-autoload |   Dumps the autoloader
|  dumpautoload  |  Dumps the autoloader
|  exec  |          Execute a vendored binary/script
|  global |         Allows running commands in the global composer dir ($COMPOSER_HOME).
|  help  |          Displays help for a command
|  home |          Opens the package's repository URL or homepage in your browser.
|  info  |          Show information about packages
|  init  |          Creates a basic composer.json file in current directory.
|  install |         Installs the project dependencies from the composer.lock file if present, or falls back on the composer.json.
|  licenses |       Show information about licenses of dependencies
|  list      |      Lists commands
|  outdated   |     Shows a list of installed packages that have updates available, including their latest version.
|  prohibits   |    Shows which packages prevent the given package from being installed
|  remove       |   Removes a package from the require or require-dev
|  require      |   Adds required packages to your composer.json and installs them
|  run-script   |   Run the scripts defined in composer.json.
|  search       |   Search for packages
|  self-update  |   Updates composer.phar to the latest version.
|  selfupdate   |   Updates composer.phar to the latest version.
|  show         |   Show information about packages
|  status       |   Show a list of locally modified packages
|  suggests     |   Show package suggestions
|  update       |   Updates your dependencies to the latest version according to composer.json, and updates the composer.lock file.
|  validate     |    Validates a composer.json and composer.lock
|  why          |    Shows which packages cause the given package to be installed
|  why-not      |   Shows which packages prevent the given package from being installed|

## Benefits of Using Composer
Composer tracks which versions of packages you have installed in a file called `composer.lock`, which is intended to be committed to version control, so that when the project is cloned in the future, simply running `composer install` will download and install all the project's dependencies.

Composer deals with PHP dependencies on a per-project basis. This makes it easy to have several projects on one machine that depend on separate versions of one PHP package.

Composer tracks which dependencies are only intended for dev environments only

    composer require --dev phpunit/phpunit

Composer provides an autoloader, making it extremely easy to get started with any package. For instance, after installing [Goutte][1] with `composer require fabpot/goutte`, you can immediately start to use Goutte in a new project:

    <?php

    require __DIR__ . '/vendor/autoload.php';

    $client = new Goutte\Client();

    // Start using Goutte

Composer allows you to easily update a project to the latest version that is allowed by your composer.json. EG. `composer update fabpot/goutte`, or to update each of your project's dependencies: `composer update`.


  [1]: https://github.com/FriendsOfPHP/Goutte

## Installation
You may install Composer locally, as part of your project, or globally as a system wide executable.

# Locally

To install, run these commands in your terminal.

    php -r "copy('https://getcomposer.org/installer', 'composer-setup.php');"
    # to check the validity of the downloaded installer, check here against the SHA-384:
    # https://composer.github.io/pubkeys.html
    php composer-setup.php
    php -r "unlink('composer-setup.php');"

This will download `composer.phar` (a PHP Archive file) to the current directory. Now you can run `php composer.phar` to use Composer, e.g.

    php composer.phar install

# Globally

To use Composer globally, place the composer.phar file to a directory that is part of your `PATH`

    mv composer.phar /usr/local/bin/composer

Now you can use `composer` anywhere instead of `php composer.phar`, e.g.

    composer install

