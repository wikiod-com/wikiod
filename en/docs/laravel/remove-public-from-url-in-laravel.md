---
title: "Remove public from URL in laravel"
slug: "remove-public-from-url-in-laravel"
draft: false
images: []
weight: 9978
type: docs
toc: true
---

How to remove `public` from URL in Laravel, there are many answers on internet but the easiest way is described below

## How to do that?
Follow these steps to remove `public` from the url

 1. Copy `.htaccess` file from `/public` directory to `Laravel/project` root folder.
 2. Rename the `server.php` in the `Laravel/project` root folder to `index.php`.

Cheers you will be good now.

Please Note: It is tested on *Laravel 4.2*, *Laravel 5.1*, *Laravel 5.2*, *Laravel 5.3*.

I think this is the easiest way to remove `public` from the url.

## Remove the public from url
 1. Renaming the `server.php` to `index.php`
 2. Copy the `.htaccess` from `public` folder to `root` folder
 3. Changing `.htaccess` a bit as follows for statics:


    RewriteEngine On
    
    RewriteCond %{REQUEST_FILENAME} !-d
    RewriteRule ^(.*)/$ /$1 [L,R=301]
    
    RewriteCond %{REQUEST_URI} !(\.css|\.js|\.png|\.jpg|\.gif|robots\.txt)$ [NC]
    RewriteCond %{REQUEST_FILENAME} !-d
    RewriteCond %{REQUEST_FILENAME} !-f
    RewriteRule ^ index.php [L]
    
    RewriteCond %{REQUEST_FILENAME} !-d
    RewriteCond %{REQUEST_FILENAME} !-f
    RewriteCond %{REQUEST_URI} !^/public/
    RewriteRule ^(css|js|images)/(.*)$ public/$1/$2 [L,NC]


Sometimes I've use this method for removing `public` form url.



