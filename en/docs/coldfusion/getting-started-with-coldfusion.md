---
title: "Getting started with coldfusion"
slug: "getting-started-with-coldfusion"
draft: false
images: []
weight: 1
type: docs
toc: true
---

## Hello World
File: test.cfm

Tag Implementation

<pre><code>&lt;cfoutput>Hello World!&lt;/cfoutput></code></pre>


CFScript Implementation

<pre><code>&lt;cfscript>
writeOutput("Hello World!");
&lt;/cfscript></code></pre>


## Installation or Setup
# Linux (Ubuntu) Installation

## Lucee (Open Source)

### ColdFusion / CFML Interpretor
Download the appropriate file from 
their site (http://lucee.org/downloads.html) and execute their installer

    wget http://cdn.lucee.org/downloader.cfm/id/155/file/lucee-5.0.0.252-pl0-linux-x64-installer.run
    sudo chmod +x lucee-5.0.0.252-pl0-linux-x64-installer.run
    sudo ./lucee-5.0.0.252-pl0-linux-x64-installer.run

Step through installer.

### Nginx

Install Nginx on your server

    sudo apt-get install nginx

Edit your /etc/nginx/sites-available/default

    server {
        listen 80;
        server_name _;
    
        root /opt/lucee/tomcat/webapps/ROOT;
        index index.cfm index.html index.htm;
    
        #Lucee Admin should always proxy to Lucee
        location /lucee {
            include lucee.conf;
        }
    
        #Pretty URLs
        location / {
            try_files $uri /index.cfm$uri?$is_args$args;
            include lucee.conf;
        }
    
        location ~ \.cfm {
            include lucee.conf;
        }
    
        location ~ \.cfc {
            include lucee.conf;
        }
    }

Edit /etc/nginx/lucee.conf

    proxy_pass http://127.0.0.1:8888;
    proxy_set_header Host $http_host;
    proxy_set_header X-Real-IP $remote_addr;
    proxy_set_header X-Forwarded-For $proxy_add_x_forwarded_for;
    proxy_set_header X-Forwarded-Proto $scheme;

Reload nginx

    sudo service nginx reload

Access the Lucee Server admin here:

    127.0.0.1/lucee/admin/server.cfm
or

    127.0.0.1:8888/lucee/admin/server.cfm

Your root web directory lives here:

    /opt/lucee/tomcat/webapps/ROOT

-----

## Adobe (Closed Source)

### ColdFusion / CFML Interpretor
Download the appropriate file from 
their site (https://www.adobe.com/products/coldfusion/download-trial/try.html) and execute their installer

    wget <URL>/ColdFusion_2016_WWEJ_linux64.bin
    sudo chmod +x ColdFusion_2016_WWEJ_linux64.bin
    sudo ./ColdFusion_2016_WWEJ_linux64.bin

Step through installer.  Make sure you select the internal web server (port 8500)

### Nginx

Install Nginx on your server

    sudo apt-get install nginx

Edit your /etc/nginx/sites-available/default

    server {
        listen 80;
        server_name _;
    
        root /opt/coldfusion2016/cfusion/wwwroot;
        index index.cfm index.html index.htm;
    
        location / {
            try_files $uri $uri/ =404;
        }
    
        location ^~ /CFIDE/administrator {
            deny all;
        }
    
        location ~* \.(cfm|cfml|cfc|html)$ {
            include /etc/nginx/conf/dc_tomcat_connector.conf;
        }
    
        location ^~ /rest {
            include tomcatconf;
        }
    }

Edit /etc/nginx/tomcat.conf

    proxy_pass http://127.0.0.1:8500;
    proxy_set_header Host $host;
    proxy_set_header X-Forwarded-Host $host;
    proxy_set_header X-Forwarded-Server $host;
    proxy_set_header X-Forwarded-For $http_x_forwarded_for;
    proxy_set_header X-Real-IP $remote_addr;

Reload nginx

    sudo service nginx reload

Access the Adobe ColdFusion Server admin here:

    127.0.0.1:8500/CFIDE/administrator/index.cfm

Your root web directory lives here:

    /opt/coldfusion2016/cfusion/wwwroot

