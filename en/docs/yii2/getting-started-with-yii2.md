---
title: "Getting started with yii2"
slug: "getting-started-with-yii2"
draft: false
images: []
weight: 1
type: docs
toc: true
---

## Installation or Setup
Yii2 can be installed in two ways. They are 

 1. Installing via Composer
 2. Installing from an Archive File

# Installing via Composer

## Installing Composer

If you do not already have Composer installed, you may do so by following the instructions at [getcomposer.org](http://getcomposer.org/). On Linux and Mac OS X, you'll run the following commands:

    curl -sS https://getcomposer.org/installer | php
    mv composer.phar /usr/local/bin/composer

For windows just download and install [composer-setup.exe][1]
You may have to configure the github API access token to overide Github API rate limit.

## Installing Yii

With Composer installed, you can install Yii by running the following commands under a Web-accessible folder:

    composer global require "fxp/composer-asset-plugin:^1.2.0"
    composer create-project --prefer-dist yiisoft/yii2-app-basic basic

then run the following command to install Yii2 with basic template.

    composer create-project --prefer-dist --stability=dev yiisoft/yii2-app-basic basic

To install Yii2 with advanced template run

    composer create-project --prefer-dist --stability=dev yiisoft/yii2-app-advanced advanced
    cd advanced
    php init
After that create a new database and adjust the components['db'] configuration in common/config/main-local.php accordingly. then run the following command to 
    
    php yii migrate

# Installing from an Archive File

 1. Download the archive file from [Yii-download][2]
 2. Unpack the downloaded file to a Web-accessible folder.
 3. Modify the config/web.php file by entering a secret key for the cookieValidationKey configuration item
         
    You can add any type of key you want:

        'cookieValidationKey' => '',
    
        For example : xyctuyvibonp
        
        'cookieValidationKey' => 'xyctuyvibonp',
        
        

     //insert a secret key in the following (if it is empty) - this is required by cookie validation
    'cookieValidationKey' => 'enter your secret key here',

  [1]: https://getcomposer.org/Composer-Setup.exe
  [2]: http://www.yiiframework.com/download/

## Install Yii2 advanced in ubuntu
First we need to install composer. Steps to install composer
Install Composer.

    curl -sS https://getcomposer.org/installer | php

Now change directory:

    sudo mv composer.phar /usr/local/bin/composer

Check composer working

    composer

Now Composer installed.

**There two ways to install Yii2 advance.**

**1.Installing from an Archive File**

Get zip file from below link.

Unzip it into destination directory, e.g. `/var/www/html`.

https://github.com/yiisoft/yii2/releases/download/2.0.8/yii-advanced-app-2.0.8.tgz

Move inside the "advanced" folder. Move manually or type below command.

    cd advanced

Run below command.

    php init

**2.Installing via Composer**

Installing via composer require github authentication token.
For token you need to sign up on GitHub.

After signup you can generate your token :

**Steps to generate a token**
1. In the top right corner of any page, click your profile photo, then click Settings.
2. In the user settings sidebar, click Personal access tokens.
3. Click Generate new token.
4. Give your token a descriptive name.
5. Select the scopes you wish to grant to this token.
6. Click Generate token.
7. Copy the token to your clipboard. For security reasons, after you navigate
off this page, no one will be able to see the token again.

Reference : https://help.github.com/articles/creating-an-access-token-for-command-line-use/


----------


After Generating token copy it

Change directory

    cd /var/www/html/

Run below command

    composer config -g github-oauth.github.com <AuthToken>

example:

    composer config -g github-oauth.github.com f1eefb8f188c22dd6467f1883cb2615c194d1ce1

Install yii2

    composer create-project --prefer-dist yiisoft/yii2-app-advanced advanced

Move inside the "advanced" folder. Move manually or type below
command.

    cd advanced

Run below command.

    php init

Its done!

Now you can check it.

http://localhost/advanced/frontend/web

and

http://localhost/advanced/backend/web

