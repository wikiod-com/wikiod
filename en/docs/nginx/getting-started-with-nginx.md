---
title: "Getting started with nginx"
slug: "getting-started-with-nginx"
draft: false
images: []
weight: 1
type: docs
toc: true
---

## Reload NGINX configuration file
As a root user:

    sudo nginx -s reload

## Ubuntu 14.04 example

    sudo service nginx reload

## Ubuntu 16.04 example

    sudo systemctl reload nginx

Before reloading, it is a good idea to check config for syntax errors:

    sudo nginx -t

  Or

    sudo service nginx configtest

 

## Installation and setup
Nginx is a Web server used to serve HTTP requests over the Internet.

Nginx is available on Linux, Windows and other OSes as direct download, and can also be built from source. For detailed instructions see [Nginx official reference.][1]

**ubuntu/debian**

  nginx stable version is available in official repo, it can be installed using
  

    sudo apt-get install nginx

  It will install and configure system startup files, but if you need latest version, you may need to add official ppa.  

    sudo add-apt-repository ppa:nginx/stable
    sudo apt-get update
    sudo apt-get install nginx

above instructions will install latest stable edition.
      
  [1]: https://www.nginx.com/resources/wiki/start/topics/tutorials/install/



## Nginx inside
One of the biggest appeals of Nginx is the difference in how it works internally as compared to the other popular servers, specially Apache.

Servers are busy programs as they have to serve requests from multiple clients. The more requests a server can successfully serve per second, the better.

Nginx works on a concurrency paradigm known as Asynchronous IO.

In a conventional server, one thread is dedicated to one request. This means, once a thread takes up a request, it is effectively unavailable for other requests. But in reality, a thread could do a lot better by accepting a bunch of requests and serving them simultaneously. Asynchronous IO is what enables this.

Nginx, therefore with its Asynchronous IO architecture, can serve many requests within one thread.

Another good thing about Nginx is its relatively leaner resource footprint. Compared to Apache, Nginx is less resource heavy, and this makes it suitable to cloud servers what tend not to be very powerful.

There are certainly other Async IO server out there, but Nginx is the most well supported among all in terms of pluginx (aka Nginx Modules).

## Nginx installation on Debian and Debian-based distros like Ubuntu
Run command below to install nginx.

    sudo apt-get install nginx

By default, Nginx automatically starts when it is installed. You can access the default Nginx landing page to confirm that the software is running properly by visiting your server's domain name or public IP address in your web browser.

but if you need latest version, you may need to add official ppa.  

    sudo add-apt-repository ppa:nginx/stable
    sudo apt-get update
    sudo apt-get install nginx


## Restart NGINX
As a root user:

    nginx -s restart

## Ubuntu example

    sudo service nginx restart

## Shutdown NGINX
Run as a root user.

Fast shutdown:

    nginx -s stop

Graceful shutdown:

    nginx -s quit

## Test if your changes in  nginx.config are valid
Ubuntu 14.04 example 
    
    sudo nginx -t 




