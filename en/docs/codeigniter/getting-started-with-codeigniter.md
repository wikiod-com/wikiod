---
title: "Getting started with codeigniter"
slug: "getting-started-with-codeigniter"
draft: false
images: []
weight: 1
type: docs
toc: true
---

## Installation and Setup
Windows Environment
===================

 1. Install [XAMPP][1] or [WAMP][2]
 2. Download and Unzip the package from [Codeigniter.com][3]
 3. Extract all the document in the server space (htdocs or www directory)
 
Mac Environment
===============

 1. Install [MAMP][5]
 2. Download and Unzip the package from [Codeigniter.com][3]
 3. Extract all the document in the server space (htdocs)

Linux Environment
=================

 1. Download and Unzip the package from [Codeigniter.com][3]
 2. Place the extracted folder in /var/www
 (in WAMP) or xampp/htdocs (XAMPP)

GitHub
============

`git clone https://github.com/bcit-ci/CodeIgniter.git`

----------

If you follow the system correctly, you will get the below screen. 

[![enter image description here][4]][4] 


----------

Base URL
========

 1. Go to `application/config/config.php`
 2. Define base URL as `$config['base_url'] = 'http://localhost/path/to/folder';`
 


----------


Remove `index.php` from URL
===========

Apache Configuration |
:----- 

 1. go to root
 2. create htaccess file
 3. Add below code inside it

        RewriteEngine on
        RewriteCond $1 !^(index\.php|assets|image|resources|robots\.txt)
        RewriteCond %{REQUEST_FILENAME} !-f
        RewriteCond %{REQUEST_FILENAME} !-d
        RewriteRule ^(.*)$ index.php/$1 [L,QSA]

Note: .htaccess code vary depending on hosting server. In some hosting server (e.g.: Godaddy) need to use an extra `?` in the last line of above code. The following line will be replaced with last line in applicable case:

    RewriteRule ^(.*)$ index.php?/$1 [L,QSA]

Nginx Configuration |
:----- 

 1. Open nginx config file (by default: `/etc/nginx/sites-available/default`)
 2. Add below code inside it

        server {
           server_name domain.tld;

           root /path-to-codeigniter-folder; //you codeigniter path
           index index.html index.php;

           # set expiration of assets to MAX for caching
           location ~* \.(ico|css|js|gif|jpe?g|png)(\?[0-9]+)?$ {
                expires max;
                log_not_found off;
           }

           location / {
                # Check if a file or directory index file exists, else route it to index.php.
                try_files $uri $uri/ /index.php;
           }

           location ~* \.php$ {
                fastcgi_pass 127.0.0.1:9000;
                include fastcgi.conf;
           }
        }





Database Configuration
===========

 1. Go to `application/config/database.php`
 2. Set the following configuration variables.
       - Host
       - Username
       - Password
       - Database Name
       - Port


## **Set Default Controller** ##

1. Go to `application/config/routes.php`
2. set the following configuration variable value with your controller name.
    - default_controller

## **AutoLoad Library And Helper** ##

1. Go to `application/config/autoload.php`
2) set Auto load value like `$autoload['libraries'] = array('database', 'session');` 
3) set Helper value like `$autoload['helper'] = array('url', 'file', 'form', 'html', 'text');`

  [1]: https://www.apachefriends.org/download.html
  [2]: http://www.wampserver.com/en/
  [3]: https://www.codeigniter.com/download
  [4]: http://i.stack.imgur.com/k04Jh.png
  [5]: https://www.mamp.info/en/downloads/

## Run multiple applications on one CI system
Codeigniter may be configured to run more than one project without duplicating CI core files.

It's possible by splitting CI Application side. For example let's take project of website, which contains `front-end` and `back-end` Content Management System (CMS) applications.
In this case CI folder structure will be like:

---

Folder structure:
---

    ├── Codeigniter
    │   ├── applications
    │   │   ├─ front-end
    │   │   │   ├── views
    │   │   │   ├── models
    │   │   │   ├── controllers
    │   │   │   ├── config
    │   │   │   └── ...
    │   │   ├─ back-end
    │   │   │   ├── views
    │   │   │   ├── models
    │   │   │   ├── controllers
    │   │   │   ├── config
    │   │   │   └── ...
    │   │   │
    │   ├── system
    │   │   ├── core
    │   │   ├── database
    │   │   ├── helpers
    │   │   └── ...
    │   │   
    │   ├── index.php
    └   └── backend.php
In `applications` folder we created two folders: `front-end` and `back-end` and copied all  default content of `applications` under these two folders. 

Also we duplicated `index.php` file under root folder as `backend.php`

Next is to configure `CI` to work with this two instances of application.

---

**Codeigniter Configuration**:
---

Open **index.php** and **backend.php** files and update `application_folder` confg:


    //index.php
    $application_folder = 'applications/front-end';

    //backend.php
    $application_folder = 'applications/back-end';


---

After configuration above, CI is ready to run two applications under one CI system:

>Request on `example.com/Codeigniter/index.php` will open `front-end` app

>Request on `example.com/Codeigniter/backend.php` will open `back-end` app



## Increase security by hiding the location of your CodeIgniter files
Within the CodeIgniter, there are two main directories to worry about: **system** and **application**.  The system folder contains the core guts of CodeIgniter. The application folder will contain all of the code specific to your application, including models, controllers, views and other relevant libraries.  

Per the CodeIgniter [installation instructions][1], in the best interest of securing your application, both the system and application folder should be placed above web root so that they are not directly accessible via a browser. By default, .htaccess files are included in each folder to help prevent direct access, but it is best to remove them from public access entirely in case the web server configuration changes or doesn't abide by the .htaccess.

    ├── CodeIgniter 
    │   ├── application
    │   ├── system
    │   ├── wwwroot
    │   │   ├── index.php

After moving the system and application folders, open the main `index.php` file and set the `$system_path`, `$application_folder` variables, preferably with a full path, e.g. ‘`/www/MyUser/system‘`.  However, relative paths should work.

**For Linux/Apache:**

    $application_folder = './application';
    $system_path = './system';

**For Windows/IIS:**

    $application_folder = '../application/';
    $system_path = '../system/';

  [1]: https://www.codeigniter.com/user_guide/installation/index.html

