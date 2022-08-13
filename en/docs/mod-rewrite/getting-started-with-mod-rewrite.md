---
title: "Getting started with mod-rewrite"
slug: "getting-started-with-mod-rewrite"
draft: false
images: []
weight: 1
type: docs
toc: true
---

## Installation
mod_rewrite must be enabled before being used on an Apache server.

### Debian/Ubuntu
Run `a2enmod rewrite`

Then restart Apache with `service apache2 restart`

### General case
Add or uncomment the following line in the static configuration file (such as `httpd.conf`):

    LoadModule rewrite_module modules/mod_rewrite.so

Then restart Apache.

## Using mod_rewrite in the static configuration file
Add the following directive *before* using any other mod_rewrite directive (RewriteRule, RewriteCond, RewriteBase or RewriteMap).

    RewriteEngine on

By default the engine is turned off. mod_rewrite directives found while the engine is turned off are ignored. Enable it from within the virtual host context when using virtual hosts, or from specific directory contexts when applicable.

## Using mod_rewrite from the dynamic configuration files
> **Important:** Using the dynamic configuration files (.htaccess) is a big performance hit. When you have access to the static configuration file (httpd.conf or something similar) you should use that instead.

In the static configuration file, allow dynamic configuration files to override "Fileinfo" using [`AllowOverride`](https://httpd.apache.org/docs/current/mod/core.html#allowoverride). This directive must be placed in directory context:

    AllowOverride FileInfo

The filename used for dynamic configuration files is governed by the [`AccessFileName`](https://httpd.apache.org/docs/current/mod/core.html#accessfilename) directive. By default, the dynamic configuration files are hidden files called `.htaccess`.

At the top of each dynamic configuration file containing mod_rewrite directives, add the following directive:

    RewriteEngine on

