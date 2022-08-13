---
title: "Getting started with symfony2"
slug: "getting-started-with-symfony2"
draft: false
images: []
weight: 1
type: docs
toc: true
---

## Installation or Setup
Symfony Framework - built with symfony components, is one of the leading PHP framework used to create robust websites and web applications.

Symfony can be installed quickly through two recommended ways.

 1. The official documentaion recommends to install the framework through the
    ***Symfony Installer*** which is a tiny php application that is installed once on the local system that helps in downloading the framework and setting up the configuration of the framework. Symfony Installer requires PHP 5.4 or higher. To install on legacy php version use Composer.
 2. Through the PHP dependency manager ***[Composer][1]***

**Installing through Symfony Installer**
========================================
On Linux/Mac OS X run the following commands:

    $ sudo curl -LsS https://symfony.com/installer -o /usr/local/bin/symfony
    $ sudo chmod a+x /usr/local/bin/symfony

On Windows move to the project directory and run the following command:

    php -r "file_put_contents('symfony', file_get_contents('https://symfony.com/installer'));"

symfony project can be created by running `symfony new my_project [2.8]` on Linux/Mac OS X

On Windows `php symfony new my_project [2.8]`

or alternatively `symfony new my_project lts` will use the latest long-term support version of Symfony.

**Installation through Composer**
=============================

 - [Download Composer][2]
 - Use Composer's `create-project` command to download Symfony
   
       composer create-project symfony/framework-standard-edition my_project_name ["2.8.*"]

Excellent detailed official documentation [here][3]

**Running the Symfony application**  
========================================
For starting symfony internal web server (available since PHP 5.4), go to the project directory and execute this command:

for symfony<=2.8

    php app/console server:start

and for symfony >=3.0

    php bin/console server:start

This starts the web server at `localhost:8000` in the background that serves your Symfony application. Then, open your browser and access the `http://localhost:8000/` URL to see the Symfony welcome page:

[![enter image description here][4]][4]


  [1]: https://getcomposer.org/
  [2]: https://getcomposer.org/download/
  [3]: https://symfony.com/doc/master/book/installation.html
  [4]: http://i.stack.imgur.com/S3WRW.png

## Simplest example in Symfony
1) Install symfony correctly as guided above.
2) Start symfony server if you are not installed in www directory.
3) Ensure http://localhost:8000 is working if symfony server is used.
4) Now it is ready to play with simplest example.
5) Add following code in a new file **/src/AppBundle/Controller/MyController.php** in symfony installation dir.
6) Test the example by visiting http://localhost:8000/hello <br>(If you are not using Symfony's built-in http server, visit http://localhost/(symfony-dir)/web/app_dev.php/hello)
7) That's all. Next: use twig to render the response.


    <?php
    // src/AppBundle/Controller/MyController.php

    namespace AppBundle\Controller;
    
    use Sensio\Bundle\FrameworkExtraBundle\Configuration\Route;
    use Symfony\Component\HttpFoundation\Response;
    
    class MyController
    {
        /**
         * @Route("/hello")
         */
        public function myHelloAction()
        {
            return new Response(
                '<html><body>
                       I\'m the response for request <b>/hello</b>
                 </body></html>'
            );
        }
    }
**NOTE:** All controller classes should have ends with word '**Controller**' and methods related to routs should ends with word '**Action**'. Further, In which controller your Actions are placed is not relevant until you define a rout prefix for the controller.

## Installing and Configuring Symfony
**Checking Requirements**

Run `bin/symfony_requirements` for checking symfony requirements and php cli setting. Install all packages that needed to run a symfony project. Setting your php.ini for example setting timezone and short_open_tag. Setting both php.ini for your php webserver (eg: /etc/php/apache2/php.ini) and php cli (eg: /etc/php/cli/php.ini). Open http://localhost/config.php for checking php webserver setting. If everything has passed, you are ready to run your symfony project.

**Running Project**

Run `composer install` to install all depedencies. Then setting up permission for `var/cache`, `var/logs` and `var/sessions`.

Detailed official documentation [here][1]


  [1]: http://symfony.com/doc/current/book/installation.html

