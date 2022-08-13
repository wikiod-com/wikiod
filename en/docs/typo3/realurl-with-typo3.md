---
title: "Realurl with TYPO3"
slug: "realurl-with-typo3"
draft: false
images: []
weight: 9955
type: docs
toc: true
---

## Introduction & Setup
The extension 'realurl' provides complete transformation of URLs with GET parameter in the browser, like “index.php?id=123&type=0&L=1” into a virtual path, a so called “Speaking URL” like “home/about-us/index.html” and back again. The objective is that URLs shall be as human readable as possible.

The extension is very flexible and can provide from simple translation of page IDs to encoding of almost any possible combination of GET parameters.

***To install this extension, four steps must be taken:***

 1. Install it in the Extension Manager  
 2. Configure Apache / .htaccess
 3. Modify your TypoScript template records with configuration for RealURL
 4. Configure the extension in typo3conf/localconf.php

**Configure Apache**

RealURLs work by providing 'virtual paths' to 'virtual files'. These don't actually exist on the file-system, so you must tell Apache to let a PHP-script handle the request if it can't find the file. This way, all URLs to pages (like www.server.com/products/product1/left.html ) will be 'redirected' to /index.php, which will handle the translation of the URL into GET parameters. Real files (like images, the TYPO3 backend, static html-files, etc.) will still be handled by Apache itself though.

You should put the supplied sample .htaccess file (called _.htaccess) in the root of your TYPO3-installation.

Alternatively, you could include the following lines in your httpd.conf , probably in the VirtualHost -section. Here is an example:

    <VirtualHost 127.0.0.1>
        DocumentRoot /var/www/typo3/dev/testsite-3/
        ServerName www.test1.intra
    
        RewriteEngine On
        RewriteRule ^/typo3$ - [L]
        RewriteRule ^/typo3/.*$ - [L]
    
        RewriteCond %{REQUEST_FILENAME} !-f
        RewriteCond %{REQUEST_FILENAME} !-d
        RewriteCond %{REQUEST_FILENAME} !-l
        RewriteRule .* /index.php
    </VirtualHost>


If you put it into a .htaccess file it has to look slightly different, basically stripping the leading slashes (“/”):

    RewriteEngine On
    RewriteRule ^typo3$ - [L]
    RewriteRule ^typo3/.*$ - [L]
    
    RewriteCond %{REQUEST_FILENAME} !-f
    RewriteCond %{REQUEST_FILENAME} !-d
    RewriteCond %{REQUEST_FILENAME} !-l
    RewriteRule .* index.php


*This will tell Apache that it should rewrite every URL that's not a filename, directory or symlink. It leaves everything starting with /typo3/ alone too.*

**NGINX Configuration** (optional)

In case NGINX is used as http server instead of Apache, rewrite rule also needs to be applied for it and placed into *server* section of website configuration from *sites-available*:

    location / {
      try_files $uri $uri/ /index.php$is_args$args;
    }

**TypoScript configuration**

Place these three lines in the main TypoScript template record of your website:

    config.simulateStaticDocuments = 0
    config.baseURL = http://mydomain.com/
    config.tx_realurl_enable = 1

Line 1 simply disables “simulateStaticDocuments” - “RealURL” is incompatible with simulateStaticDocuments and will simply not work if it has been enabled. This line should remind you of this fact.

Line 2 makes the frontend output a `<base>` tag in the header of the pages. This is required because relative references to images, stylesheets etc. will break when the virtual paths are used unless this has been set. Please see below for a detail discussion of why this is needed. Do not forget to write real name of your domain! And note slash in the end – it is required!

Line 3 enables the encoding of URLs as the virtual paths, the “Speaking URLs”.

If you use config.typolinkEnableLinksAcrossDomains , make sure you check “Is site root?” on all pages that are site roots.

And finally you have to generate a realurl.conf.php file. Either you can create it automatically by permitting the extension to do it or you can do it manually.

Will be covered in next topic

Courtesy : 

Dmitry Dulepov 

> https://docs.typo3.org/typo3cms/extensions/realurl/Realurl/Index.html



