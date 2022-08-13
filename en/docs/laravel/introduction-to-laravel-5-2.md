---
title: "Introduction to laravel-5.2"
slug: "introduction-to-laravel-52"
draft: false
images: []
weight: 9978
type: docs
toc: true
---

Laravel is a MVC framework with bundles, migrations, and Artisan CLI. Laravel offers a robust set of tools and an application architecture that incorporates many of the best features of frameworks like CodeIgniter, Yii, ASP.NET MVC, Ruby on Rails, Sinatra, and others.
Laravel is an Open Source framework. It has a very rich set of features which will boost the speed of Web Development. If you familiar with Core PHP and Advanced PHP, Laravel will make your task easier. It will save a lot time.

This section provides an overview of what laravel-5.1 is, and why a developer might want to use it.

It should also mention any large subjects within laravel-5.1, and link out to the related topics.  Since the Documentation for laravel-5.1 is new, you may need to create initial versions of those related topics.

## Installation or Setup
Instructions on installing Laravel 5.1 on a Linux/Mac/Unix Machine.

Before initiating the installation, check if the following requirements are met:
- PHP >= 5.5.9
- OpenSSL PHP Extension
- PDO PHP Extension
- Mbstring PHP Extension
- Tokenizer PHP Extension


Let's begin the installation:
1. Install composer. [Composer Documentation][1]
2. Run `composer create-project laravel/laravel <folder-name> "5.1.*"`
3. Ensure that the `storage` folder and the `bootstrap/cache` folder are writable.
4. Open the `.env` file and set the configuration information like database credentials, debug status, application environment, etc.
5. Run `php artisan serve` and point your browser to `http://localhost:8000`. If everything is fine then you should get the page


  [1]: https://getcomposer.org/doc/00-intro.md

## Install Laravel 5.1 Framework on Ubuntu 16.04, 14.04 & LinuxMint
**Step 1 – Install LAMP**

To start with Laravel, we first need to set up a running LAMP server. If you have already running LAMP stack skip this step else use followings commands to set up lamp on Ubuntu system.

Install PHP 5.6

    $ sudo apt-get install python-software-properties
    $ sudo add-apt-repository ppa:ondrej/php
    $ sudo apt-get update
    $ sudo apt-get install -y php5.6 php5.6-mcrypt php5.6-gd

Install Apache2

    $ apt-get install apache2 libapache2-mod-php5

Install MySQL

    $ apt-get install mysql-server php5.6-mysql

**Step 2 – Install Composer**

Composer is required for installing Laravel dependencies. So use below commands to download and use as a command in our system.

    $ curl -sS https://getcomposer.org/installer | php
    $ sudo mv composer.phar /usr/local/bin/composer
    $ sudo chmod +x /usr/local/bin/composer

**Step 3 – Install Laravel**

To download latest version of Laravel, Use below command to clone master repo of laravel from github.

    $ cd /var/www
    $ git clone https://github.com/laravel/laravel.git

Navigate to Laravel code directory and use composer to install all dependencies required for Laravel framework.

    $ cd /var/www/laravel
    $ sudo composer install

Dependencies installation will take some time. After than set proper permissions on files.

    $ chown -R www-data.www-data /var/www/laravel
    $ chmod -R 755 /var/www/laravel
    $ chmod -R 777 /var/www/laravel/app/storage

**Step 4 – Set Encryption Key**

Now set the 32 bit long random number encryption key, which used by the Illuminate encrypter service.

    $ php artisan key:generate
    
    Application key [uOHTNu3Au1Kt7Uloyr2Py9blU0J5XQ75] set successfully.

Now edit `config/app.php` configuration file and update above generated application key as followings. Also make sure cipher is set properly.

    'key' => env('APP_KEY', 'uOHTNu3Au1Kt7Uloyr2Py9blU0J5XQ75'),
    
    'cipher' => 'AES-256-CBC',

**Step 5 – Create Apache VirtualHost**

Now add a Virtual Host in your Apache configuration file to access Laravel framework from web browser. Create Apache configuration file under `/etc/apache2/sites-available/` directory and add below content.

    $ vim /etc/apache2/sites-available/laravel.example.com.conf

This is the Virtual Host file structure.

    <VirtualHost *:80>
    
            ServerName laravel.example.com
            DocumentRoot /var/www/laravel/public
    
            <Directory />
                    Options FollowSymLinks
                    AllowOverride None
            </Directory>
            <Directory /var/www/laravel>
                    AllowOverride All
            </Directory>
    
            ErrorLog ${APACHE_LOG_DIR}/error.log
            LogLevel warn
            CustomLog ${APACHE_LOG_DIR}/access.log combined
    </VirtualHost>

Finally lets enable website and reload Apache service using below command.

    $ a2ensite laravel.example.com
    $ sudo service apache2 reload

**Step 6 – Access Laravel**

At this point you have successfully completed Laravel 5 PHP framework on your system. Now make host file entry to access your Laravel application in web browser. Change `127.0.0.1` with your server ip and `laravel.example.com` with your domain name configured in Apache.

    $ sudo echo "127.0.0.1  laravel.example.com" >> /etc/hosts

And access http://laravel.example.com in your favorite web browser as below.

