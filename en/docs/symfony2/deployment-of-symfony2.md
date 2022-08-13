---
title: "Deployment of Symfony2"
slug: "deployment-of-symfony2"
draft: false
images: []
weight: 9989
type: docs
toc: true
---

## Steps to move Symfony 2 project to hosting manually
It depends on kind of hosting that you have: 

 1. If you have SSH console, then you can do it on hosting after step 2,
    if you haven't then do it locally: run command 
    
        php app/console cache:clear --env=prod'. 
 
 2. Suppose you have on you hosting folders
    `youdomain/public_html`, so in `public_html` must be located all web
    files. So you must upload all from Symfony project (folders:
    `app`, `src`, `vendors`, `bin`; files: `deps`, `deps.lock`), except for folder `web`
    in folder `youdomain`. Everything from folder `web` upload to folder
    `public_html`. 
 3. Check CHMOD for folders `app/cache` and `app/logs`, there
    should be write access. 
 4. If there is no file .htaccess in
    public_html, then create it and add such code in it:
    https://raw.github.com/symfony/symfony-standard/master/web/.htaccess
 5. Now you should use `youdomain.com/index` instead of
    `youdomain.com/app_dev.php/index`, that you use locally. If a site still
    did not works, you can open file `web/config.php` and find a code where
    a check for IP performs, you find there only IP `127.0.0.1`. Add your
    current IP to this list and upload new config on the server. Then you
    can open path `yourdomain/config.php` and check what's wrong. If
    `config.php` shows that everything ok, but it still didn't work, you can
    enable `app_dev.php` to debug: open `app/app_dev.php` and your IP the same way as in
    `config.php`. Now you can run scripts as locally using `app_dev.php`.



