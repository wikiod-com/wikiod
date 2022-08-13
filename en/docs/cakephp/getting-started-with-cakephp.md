---
title: "Getting started with cakephp"
slug: "getting-started-with-cakephp"
draft: false
images: []
weight: 1
type: docs
toc: true
---

## Basic first empty project
## [Initial Creation and Download](http://book.cakephp.org/3.0/en/installation.html) (CakePHP 3.x)
The easiest way to create a new CakePHP project is via Composer (if you don't know about composer look [here](https://getcomposer.org/) for more info)

### Install Composer
If you need to install it and are on a windows machine follow [this guide](https://getcomposer.org/doc/00-intro.md#installation-windows)

If you are on Linux/Unix/OSX follow [this guide](https://getcomposer.org/doc/00-intro.md#installation-linux-unix-osx)

### Create the first CakePHP Project
Open a console window and navigate to your installation of php (on Windows with the default xampp installation this is  `C:\xampp\php`)

To create an empty project then run the following command:

    php composer.phar create-project --prefer-dist cakephp/app name_of_your_project

### [Baking/Model/View/Controllers](http://book.cakephp.org/3.0/en/bake/usage.html)
The magic of CakePHP is baking - an automated generation of controller, model and view code with basic CRUD options. 

Before baking you need to have your database connection configured. To do this you need to edit the file `config/app.php` in your project.

    'Datasources' => [
    'default' => [
        'className' => 'Cake\Database\Connection',
        'driver' => 'Cake\Database\Driver\Mysql',
        'persistent' => false,
        'host' => 'localhost',
        'username' => 'my_app', //in basic xampp: root
        'password' => 'sekret', //in basic xampp: ''
        'database' => 'my_app', //name of the database you want to connect to your project
        'encoding' => 'utf8',
        'timezone' => 'UTC',
        'cacheMetadata' => true,
    ]
],

If your database is connected correctly you then enter `bin/cake bake`in the root folder of your project in a console window.

This should output something like this:

    Welcome to CakePHP v3.1.6 Console
    ---------------------------------------------------------------
    App : src
    Path: /var/www/cakephp.dev/src/
    PHP: 5.5.8
    ---------------------------------------------------------------
    The following commands can be used to generate skeleton code for your application.

    Available bake commands:

    - all
    - behavior
    - cell
    - component
    - controller
    - fixture
    - form
    - helper
    - mailer
    - migration
    - migration_snapshot
    - model
    - plugin
    - shell
    - shell-helper
    - template
    - test

    By using `cake bake [name]` you can invoke a specific bake task.

For simplicity purposes we are going to bake everything with the default settings. To do this you enter

    cake bake all
This will output something along those lines:

    Welcome to CakePHP v3.2.11 Console
    ---------------------------------------------------------------
    App : src
    Path: C:\xampp\htdocs\tipping\src\
    PHP : 5.6.15
    ---------------------------------------------------------------
    Bake All
    ---------------------------------------------------------------
    Possible model names based on your database:
    - users
    - blogs
    Run `cake bake all [name]` to generate skeleton files.
By running `cake bake all <modelNameYouWantToBake>` the model, table, controller, fixture and view files are created. 
Run this for every possible model name and you have a functioning Project with basic CRUD options.

Now you can open your browser and see how it looks and start extending the project by your own logic

## CakePHP 2.x Basic Introduction
Will talk about directory structure of CakePHP, what each folder means.
## CakePHP has some main folders ##

 1. app - It Contains our application source code, all our code lies under this directory.
 2. lib - This is the cakephp core liberary, it contains all the base cakephp library code. Editing code inside this directory is not suggested as they can cause error while upgrading the cakephp library.
 3. plugins - This contains the cakephp plugins code which will be used for our applicatin.
 4. vendors - This contains external code, This code will not use cakephp library.
 5. index.php - This is the index file.

> We can have multiple applications hosted inside a single project. i.e they can use same lib folders, plugin and vendors.

> To Modify the lib code, best practice is to extend them in our app folder and perform the modifications.

> Plugins and vendors folder are shared by all the applications hosted in the same directory.

> index.php is the file which is called first.

[![enter image description here][1]][1]

## Now we should jump to our app folder ##



  [1]: http://i.stack.imgur.com/OXjZb.png

## Installation or Setup
## Requirements 
The following setup guide is for cakephp 2.8 and above. All cakephp versions lower than 2.8 are not compatible with php 7

HTTP Server. For example: Apache. Having mod_rewrite is preferred, but by no means required.

* PHP 5.5.9 or greater (including PHP 7). 
* mbstring PHP extension
* intl PHP extension

> **Attention!** In both XAMPP and WAMP, the mbstring extension is working by default.
In XAMPP, intl extension is included but you have to uncomment extension=php_intl.dll in php.ini and restart the server through the XAMPP Control Panel.
In WAMP, the intl extension is “activated” by default but not working. To make it work you have to go to php folder (by default) C:\wamp\bin\php\php{version}, copy all the files that looks like icu*.dll and paste them into the apache bin directory C:\wamp\bin\apache\apache{version}\bin. Then restart all services and it should be OK.

While a database engine isn’t required, we imagine that most applications will utilize one. CakePHP supports a variety of database storage engines:

* MySQL (5.1.10 or greater)
* PostgreSQL
* Microsoft SQL Server (2008 or higher)
* SQLite 3


## CakePHP3 Folder Structure
After you’ve downloaded, these are the files and folders you should see:
 - The **bin folder** holds the Cake console executables.
 - The **config folder** holds the Configuration files CakePHP uses. Database connection details, bootstrapping, core configuration files and more should be stored here.
 - The **plugins folder** is where the Plugins your application uses are stored.
 - The **logs folder** normally contains your log files, depending on your log configuration.
 - The **src folder** will be where your application’s files will be placed.
 - The **tests folder** will be where you put the test cases for your
   application.
 - The **tmp folder** is where CakePHP stores temporary data. The actual data it stores depends on how you have CakePHP configured, but this folder is usually used to store model descriptions and sometimes session information.
 - The **vendor folder** is where CakePHP and other application dependencies will be installed. Make a personal commitment not to edit files in this folder.
 - The **webroot directory** is the public document root of your application. It contains all the files you want to be publically reachable.

> Make sure that the **tmp** and **logs** folders exist and are
> writable, otherwise the performance of your application will be
> severely impacted. In debug mode, CakePHP will warn you, if it is not
> the case.

Inside src Folder
--------------

CakePHP’s src folder is where you will do most of your application development.

**Console** folder contains the console commands and console tasks for your application. For more information see Shells, Tasks & Console Tools.

**Controller** folder contains your application’s controllers and their components.

**Locale** folder stores string files for internationalization.

**Model** folder contains your application’s tables, entities and behaviors.

**View** - presentational classes are placed here: cells, helpers, and template files.

**Template** - presentational files are placed here: elements, error pages, layouts, and view template files.

## Requirements
    1-HTTP Server. For example: Apache. Having mod_rewrite is preferred, but by no means required.
    2-PHP 5.5.9 or greater (including PHP 7)
    3-mbstring PHP extens ion
    4-intl PHP extension


I usually make an apache and mysql installation on a linuxbox. I can use windows too, however I do not recommend it ;) So, I usually make a new entry into the /etc/hosts file to make a sitename available to cakephp.

    127.0.0.1   localhost caketest.local
next step to copy all cakephp files into a subdirectory inside /home/myusername/public_html/caketest

    app
    cake
    index.php
    plugins
    README
    vendors
    .htaccess

then I set up the site to apache (not neccessary),

<VirtualHost *:80>
    DocumentRoot "/home/myusername/public_html/caketest"
    ServerName caketest.local  
    # This should be omitted in the production environment
    SetEnv APPLICATION_ENV development

    <Directory "/home/myusername/public_html/caketest">
    Options Indexes MultiViews FollowSymLinks
    AllowOverride All
    Order allow,deny
    Allow from all
    </Directory>
</VirtualHost>

restart apache. you also need to edit the .htaccess files and place a RewriteBase directive with hte path to the actual directory, e.g.

     RewriteBase /~myusername/caketest

create a database, set the db connection in cake config files and that's all. you can point your browser to http://caketest.local if you do not want a test site url you can skip hosts, and apache vhost creation, but the url to use should be http:/localhost/~myusername/caketest

another important thing is to enable userdir modul in apache, and also check if using php is enabled in userdirs too.

