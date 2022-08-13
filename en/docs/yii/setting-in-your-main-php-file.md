---
title: "Setting in your main.php file"
slug: "setting-in-your-mainphp-file"
draft: false
images: []
weight: 9990
type: docs
toc: true
---

## How to set main.php file in YII Framework V1.
In the versions of YII Framework Version 1.

You will set your main.php File.

File Path : **application_name/protected/config/main.php**

    <?php
    return array(
        // Set Application Name
        'name'              => "Applicaiton Name",
        
        // Set Default Controller
        'defaultController' => 'site/login',
        
        // Set Language
        'language'          => 'in',
        
        // Set Language for messages and views
        'sourceLanguage'    => 'en',
        
        // Set Time Zone
        'timeZone' => 'Asia/Calcutta',
        
        //Charset to use
        'charset'=>'utf-8',
    
        // preloading 'log' component
        'preload'=>array('log'),
        
        //application-level parameters that can be accessed
        'params'=> array(
            $documentUrl = $baseUrl,    // Document URL
            $documentPath = $_SERVER['DOCUMENT_ROOT'] . '/', // Document Path
        ),
    );
    ?>

[List of Supported Time Zones - PHP][1]


  [1]: http://php.net/manual/en/timezones.php

## How to remove index.php in url.
Removed the commented lines for rewrite in httpd.conf file.

    LoadModule rewrite_module modules/mod_rewrite.so

You can modify **.htaccess** file your application folder.

    RewriteEngine on
    
    # if a directory or a file exists, use it directly
    RewriteCond %{REQUEST_FILENAME} !-f
    RewriteCond %{REQUEST_FILENAME} !-d
    
    # otherwise forward it to index.php
    RewriteRule . index.php

after you can change your **main.php** file code.

    <?php
    return array(
        // application components
        'components'=>array(
                // uncomment the following to enable URLs in path-format
                'urlManager'=>array(
                    'urlFormat'=>'path',
                    'showScriptName'=>false,
                    'rules'=>array(
                        '<controller:\w+>/<id:\d+>'=>'<controller>/view',
                        '<controller:\w+>/<action:\w+>/<id:\d+>'=>'<controller>/<action>',
                        '<controller:\w+>/<action:\w+>'=>'<controller>/<action>',
                    ),
                    'urlSuffix'=>'.html',
                    'caseSensitive'=>false
                ),
        ),
    );
    ?>

