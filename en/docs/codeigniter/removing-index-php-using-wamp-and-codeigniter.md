---
title: "Removing index.php using WAMP and CodeIgniter"
slug: "removing-indexphp-using-wamp-and-codeigniter"
draft: false
images: []
weight: 9974
type: docs
toc: true
---

## How to remove the index.php from url's with using wamp and codeigniter
First thing to do is enable the mod rewrite on wamp go to Apache modules and scroll down the list

If not showing tick enable it and then restart all servers.

[![enter image description here][1]][1]

**Linux users can also use below terminal command to enable rewrite module**

    sudo a2enmod rewrite
**Then restart apache using:**

    sudo service apache2 restart

Then out side of your application folder create a file called .htaccess

    project > application

    project > system

    project > .htaccess

    project > index.php

Try this code below 

    Options +FollowSymLinks
    RewriteEngine on
    RewriteCond $1 !^(index\.php|images|robots\.txt)
    RewriteCond %{REQUEST_FILENAME} !-f
    RewriteCond %{REQUEST_FILENAME} !-d
    RewriteRule ^(.*)$ ./index.php/$1 [L]

If not here is some more htaccess [examples][2]

Then go to the config.php file. Set your base_url and make the index_page blank

        $config['base_url'] = ((isset($_SERVER['HTTPS']) && $_SERVER['HTTPS'] == "on") ? "https" : "http");
        $config['base_url'] .= "://".$_SERVER['HTTP_HOST'];
        $config['base_url'] .= str_replace(basename($_SERVER['SCRIPT_NAME']),"",$_SERVER['SCRIPT_NAME']);
        $config['index_page'] = '';


Hope this helps you can use the htaccess files from examples for others.

  [1]: http://i.stack.imgur.com/go2OZ.jpg
  [2]: https://github.com/riwakawebsitedesigns/htaccess_for_codeigniter

