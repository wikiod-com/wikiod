---
title: "Install Symfony2 on localhost"
slug: "install-symfony2-on-localhost"
draft: false
images: []
weight: 9994
type: docs
toc: true
---

## Using the command prompt
The best way to install and configure a Symfony2 project is described in the [official documentation](http://symfony.com/download) as follows:
    
**Mac OS X / Linux**

    $ sudo curl -LsS http://symfony.com/installer -o /usr/local/bin/symfony
    $ sudo chmod a+x /usr/local/bin/symfony 

**Windows**

    c:\> php -r "file_put_contents('symfony', file_get_contents('https://symfony.com/installer'));" 

Then you could use the Symfony binary to build the right scaffolding:

    $ symfony new my_project 

## Using composer over console
Given you've already installed [composer][1] up and running and it's accessible globally, you can simply create new Symfony projects as stated in the [official documentation][2].

Now you can create a new Symfony project with composer:

    composer create-project symfony/framework-standard-edition my_project_name

This will create the project in the current directory you're in.

If you want to create the project with a specific version of Symfony, you can add a parameter with the version number:

    composer create-project symfony/framework-standard-edition my_project_name "2.8.*"

Hint: If you're thinking that composer won't do anything, add the `-vvv` flag to the command. That way, you'll receive detailed information about what composer is doing right now.


  [1]: https://getcomposer.org/
  [2]: http://symfony.com/doc/2.8/setup.html#creating-symfony-applications-with-composer

