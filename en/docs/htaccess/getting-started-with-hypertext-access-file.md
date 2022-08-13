---
title: "Getting started with Hypertext Access file"
slug: "getting-started-with-hypertext-access-file"
draft: false
images: []
weight: 1
type: docs
toc: true
---

## Setting up .htaccess
<!-- language-all: lang-bash -->

> `.htaccess` files (or "distributed configuration files") provide a way
> to make configuration changes on a per-directory basis. A file,
> containing one or more configuration directives, is placed in a
> particular document directory, and the directives apply to that
> directory, and all subdirectories thereof.

An .htaccess file controls how Apache interacts with your site. It is used to alter the requests and modify default behavior without needing to alter the core server configuration files.

Setting up `.htaccess` is as simple as opening a notepad and saving it as `.htaccess`. Generally, this file will placed on the `root` directory of your website files, but you can use it under multiple different directories. This is especially useful if you're looking to password protect specific directories.

# Enabling .htaccess #

> Sometimes even a single error in your `httpd.conf` or `.htacces`s file
> will result in a temporary meltdown of the server, and users will see
> *500 - Internal Server Error* page.
> So, make sure to always make a backup of
> your `httpd.conf` and `.htaccess` files before you make a change.

    <Directory "/var/www">
        AllowOverride All
    </Directory>

`.htaccess` files are normally enabled by default. This is controlled by `AllowOverride` directive in the `httpd.conf` file. This directive can only be placed inside of a `<Directory>` section. 

Beside `All` there are numerous other values that limit configuration of only certain contexts. Some of them are:

 - **None** - Completely disable `.htaccess`.
 - **AuthConfig** - Authorization directives such as those dealing with Basic Authentication.
 - **FileInfo** - Directives that deal with setting Headers, Error Documents, Cookies, URL Rewriting, and more.
 - **Indexes** - Default directory listing customizations.
 - **Limit** - Control access to pages in a number of different ways.
 - **Options** - Similar access to Indexes but includes even more values such as ExecCGI, FollowSymLinks, Includes and more.


    # Only allow .htaccess files to override Authorization and Indexes
    AllowOverride AuthConfig Indexes



## Custom Error Pages
<!-- language-all: lang-bash -->

`.htaccess` can be used to set a custom error pages that matches the theme of your website instead of seeing a white error page with black techno-babble when users end up on at a page with an error server response code. The error page can be any browser parseable file, including (But not limited to) .html, .php, .asp, .txt, .xml.

Examples for almost all common error response codes:

    #Client Errors

    ErrorDocument 400 /mycool400page.html    # Bad Request
    ErrorDocument 401 /mycool401page.html    # Unauthorized
    ErrorDocument 402 /mycool402page.html    # Payment Required
    ErrorDocument 403 /mycool403page.html    # Forbidden
    ErrorDocument 404 /mycool404page.html    # Page Not Found

    #Server Errors

    ErrorDocument 500 /mycool500page.html    # Internal Server Error
    ErrorDocument 501 /mycool501page.html    # Not Implemented
    ErrorDocument 502 /mycool502page.html    # Bad Gateway
    ErrorDocument 503 /mycool503page.html    # Service Unavailable
    ErrorDocument 504 /mycool504page.html    # Gateway Timeout
    ErrorDocument 505 /mycool505page.html    # Internal Server Error

It is always good practice to include Error Documents for the most common error responses, 400, 403,  404, and 500, as these errors are able to occur on all browsers.

the 500 error is one of the most notorious errors as it occurs if anything fails while loading the page to send, most commonly server html preprocessing failures from things like PHP, ASP, and other html preprocessors. It is good practice while testing to set the 500 page to display the error that occurred, rather then an unspecific 500 error page.

To enable the 500 error page to write a specific error see one of the following based on what html preprocessor you are using:
[php][1]
[asp][2]

  [1]: http://stackoverflow.com/questions/2687730/how-can-i-make-php-display-the-error-instead-of-giving-me-500-internal-server-er
  [2]: http://stackoverflow.com/questions/2640526/detailed-500-error-message-asp-iis-7-5

## Setting Server Timezone
There are many time zones around the world, it is important to make sure your server is set to the right one. This is done in `.htaccess` by using:

    SetEnv TZ America/Indianapolis

A few example of possible other time zones:

    America/Los_Angeles
    America/Los_Angeles - Pacific Time 
    Pacific/Honolulu - Hawaii

Just make sure you use `SetEnv` in front of your selected time zone.

