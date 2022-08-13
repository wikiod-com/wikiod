---
title: "Getting started with symfony"
slug: "getting-started-with-symfony"
draft: false
images: []
weight: 1
type: docs
toc: true
---

## Creating a new Symfony project using the Symfony Installer
The [Symfony Installer][1] is a command line tool that helps you to create new Symfony applications. It requires PHP 5.4 or higher.

## Downloading and installing the Symfony Installer on Linux / MacOS ##

Open a terminal and execute the following commands:

    sudo mkdir -p /usr/local/bin
    sudo curl -LsS https://symfony.com/installer -o /usr/local/bin/symfony
    sudo chmod a+x /usr/local/bin/symfony

This creates a global `symfony` executable that can be called from anywhere. You have to do this only once: now you can create as many Symfony projects with it as you want.

## Creating a new project with the latest Symfony version ##

Once the installer is installed, you can use it to create a new Symfony project. Run the following command:

    symfony new my_project_name

This command will create a new directory (called `my_project_name`) containing the most recent version of the [Symfony Standard Edition][2]. It will also install all of its dependencies (including the actual Symfony components) using Composer.

## Creating a new project using a specific Symfony version ##

If you want to select a specific Symfony version instead of the latest one, you can use the optional second argument of the `new` command.

To select a minor version:

    symfony new my_project_name 3.2

To select a patch version:

    symfony new my_project_name 3.2.9

To select a beta version or release candidate:

    symfony new my_project 2.7.0-BETA1
    symfony new my_project 2.7.0-RC1

To select the most recent Long Term Support (LTS) version:

    symfony new my_project_name lts

  [1]: https://symfony.com/doc/current/setup.html#creating-symfony-applications
  [2]: https://github.com/symfony/symfony-standard

## Creating a new Symfony project using Composer
If for some reason using the [Symfony Installer][1] is not an option, you can also create a new project using Composer. First of all, make sure you have [installed Composer][2].

Next, you can use the `create-project` command to create a new project:

    composer create-project symfony/framework-standard-edition my_project_name

Similar to the Symfony Installer, this will install the latest version of the [Symfony Standard Edition][3] in a directory called `my_project_name` and will then install its dependencies (including the Symfony components).

## Installing a specific Symfony version ##

As with the Symfony Installer, you can select a specific version of Symfony by supplying an optional third argument:

    composer create-project symfony/framework-standard-edition my_project_name "2.8.*"

Note however that not all version aliases (such as `lts` for example) are available here.


  [1]: https://www.wikiod.com/symfony/getting-started-with-symfony#Creating a new Symfony project using the Symfony Installer
  [2]: https://www.wikiod.com/composer-php/getting-started-with-composer-php
  [3]: https://github.com/symfony/symfony-standard

## Running the Symfony application using PHP's built-in web server
After [creating a new Symfony application][1], you can use the `server:run` command to start a simple PHP web server, so you can access your new application from your web browser:

    cd my_project_name/
    php bin/console server:run

You can now visit http://localhost:8000/ to see the Symfony welcome page.

**Important**: while using the built-in web server is great for development, you should **not** use it in production. Use a full-featured web server such as Apache or Nginx instead.


  [1]: https://www.wikiod.com/symfony/getting-started-with-symfony#Creating a new Symfony project using the Symfony Installer

