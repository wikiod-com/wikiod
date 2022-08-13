---
title: "Getting started with apache"
slug: "getting-started-with-apache"
draft: false
images: []
weight: 1
type: docs
toc: true
---

## Installation or Setup
Detailed instructions on getting apache set up or installed.

## Ubuntu Installation ##
-----------------------
    sudo apt-get install apache2

## Windows Installation ##
-----------------------
Check out the [WAMP](http://www.wampserver.com/en/) stack. WAMP stands for Windows, Apache, MySQL, PhpMyAdmin.

## CentOS Installation ##
-----------------------
Apache 2.2 comes with CentOS6, whereas 2.4 comes with CentOS7, to install on either OS, run

    yum -y install httpd

## macOS Installation ##
-----------------------
macOS comes with Apache pre-installed,however,can install Apache via Homebrew

If you already have the built-in Apache running, it will need to be shutdown first, and any auto-loading scripts removed.

    $ sudo apachectl stop
    $ sudo launchctl unload -w /System/Library/LaunchDaemons/org.apache.httpd.plist 2>/dev/null
    $ brew install httpd24 --with-privileged-ports --with-http2

## [Ubuntu] Simple Hello World Example
This example will guide you through setting up a back end serving an a Hello World HTML page.

## Installing Requirements ##
----------
Order matters for this step!

- `sudo apt-get install apache2`  

## Setting up the HTML ##
----------
Apache files live in `/var/www/html/`. Lets quickly get there. Make sure you're in your root directory first, `cd`, then `cd /var/www/html/`.

This `html` directory is where all your website files will live. Lets quickly make a simple Hello World file.

Using your favorite text editor, type the following in

    <!DOCTYPE html>
    <html>
    <head>
        <title>Hello World!</title>
    </head>
    <body>
        <h1>Hello World!</h1>
    </body>
    </html>
 
Save this file as `index.html` in the current directory and you're set to go!

## Visiting Your Webpage ##
----------
To visit the page you just created, in your browser of choice, go to `localhost`. If that doesn't work, try `127.0.0.1`. You should see "Hello World!" as a `h1`. You're done!

    
    


## To ensure the server is up.
If you get a message that the browser can't connect to the server, first check to ensure the server is up.

    $ ps -aef | grep httpd
You should see a few httpd processes if Apache is up and running.


