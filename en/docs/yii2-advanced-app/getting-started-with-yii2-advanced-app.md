---
title: "Getting started with yii2-advanced-app"
slug: "getting-started-with-yii2-advanced-app"
draft: false
images: []
weight: 1
type: docs
toc: true
---

## Installation or Setup
**Installation Requirements**

The minimum requirement by this project template is that your Web server supports PHP 5.4.0.

Yii2-advanced-app can be installed in two ways. They are

   1) Installing via Composer
   2) Installing from an Archive File

**1) Installing via Composer**

   If you do not already have Composer installed, you may do so by following the instructions at [getcomposer.org][1]. On Linux and Mac OS X, you'll run the following commands:

   First we need to install composer.

   **Step-1** 
   Install Composer. Run below command in your terminal

    curl -sS https://getcomposer.org/installer | php

   **Step-2** Now change directory

    sudo mv composer.phar /usr/local/bin/composer

   **Step-3** check composer working

    composer

   If Composer installed successfully.

   Then install the application using the following commands:

    composer global require "fxp/composer-asset-plugin:~1.1.1"
    composer create-project --prefer-dist yiisoft/yii2-app-advanced yii-application

  The first command installs the [composer asset plugin][2] which allows managing bower and npm package dependencies through Composer. You only need to run this command once for all. The second command installs the advanced application in a directory named `yii-application.` You can choose a different directory name if you want.

**2) Installing from an Archive File**

Extract the archive file downloaded from [yiiframework.com][3] to a directory named advanced that is directly under the Web root.

Modify the config/web.php file by entering a secret key for the `cookieValidationKey` configuration item (this is done automatically if you are installing Yii using Composer):

    // !!! insert a secret key in the following (if it is empty) - this is required by cookie validation
    'cookieValidationKey' => 'enter your secret key here',

Then follow the instructions given in the next subsection.

**Preparing application**

After you install the application, you have to conduct the following steps to initialize the installed application. You only need to do these once for all.

  1. Open a console terminal, execute the init command and select dev as environment.

    /path/to/php-bin/php /path/to/yii-application/init

   If you automate it with a script you can execute init in non-interactive mode.

    /path/to/php-bin/php /path/to/yii-application/init --env=Production --overwrite=All

   2. Create a new database and adjust the `components['db']` configuration in `common/config/main-local.php` accordingly.

   3. Open a console terminal, apply migrations with command `/path/to/php-bin/php /path/to/yii-application/yii migrate`.


  [1]: http://getcomposer.org
  [2]: https://github.com/fxpio/composer-asset-plugin
  [3]: http://www.yiiframework.com/download/

