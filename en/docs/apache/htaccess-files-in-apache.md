---
title: ".htaccess files in Apache"
slug: "htaccess-files-in-apache"
draft: false
images: []
weight: 9964
type: docs
toc: true
---

## Force HTTPS
.htaccess can be used to force your HTTP site to redirect to HTTPS.

Here's a quick way that doesn't require editing the code for your domain:

    RewriteEngine On
    RewriteCond %{HTTPS} =off
    RewriteRule ^ https://%{HTTP_HOST}%{REQUEST_URI} [L,R=301]

> **Warning:** The code above assumes that you can trust `%{HTTP_HOST}` to point to your domain.
> 
> If you need to be sure that the redirect location is your domain, replace `%{HTTP_HOST}` with your domain.

The code above does this:

 1. Enable [RewriteEngine][1].
 2. Continue if the current request is not using HTTPS.
 3. Do a HTTP 301 redirect to `https://%{HTTP_HOST}%{REQUEST_URI}`, where
    - `%{HTTP_HOST}` is the host requested by the browser and
    - `%{REQUEST_URI}` is the URI requested by the browser (everything after the domain).

> **Warning:** Your web application must be able to handle HTTPS requests, and Apache for your host should be configured with a valid site certificate.

Note that it is significantly more efficient to simply do a `Redirect` in the http vhost than to do these multiple per-request comparisons in a .htaccess file. See http://wiki.apache.org/httpd/RedirectSSL for further discussion of this technique.

  [1]: https://www.wikiod.com/apache/htaccess-files-in-apache#Rewrite Engine

## Rewrite Engine
The RewriteEngine module within Apache is used to dynamically rewrite URLs and paths depending on various expressions provided:
```
<IfModule mod_rewrite.c>
RewriteEngine On
RewriteBase /
RewriteRule ^index\.php$ - [L]
RewriteCond %{REQUEST_FILENAME} !-f
RewriteCond %{REQUEST_FILENAME} !-d
RewriteRule . /index.php [END]
</IfModule>
```
The above rules will rewrite PHP files to no longer show their extension, and so that index.php will just show as a naked domain (similar to the behavior normally seen in index.html). The above rule ships with WordPress.

Note that in Apache httpd 2.2.16 and later, this entire block can be replaced with a single line using the FallbackResource directive:

```
FallbackResource /index.php
```


## Enable CORS
To enable ***Cross-Origin Resource Sharing*** (**CORS**) in Apache you'll need to set at least one HTTP header which changes it (the default behaviour is to block CORS). In the following example, we're going to be setting this HTTP header inside `.htaccess`, but it can also be set in your site `your-site.conf` file or the Apache config file. Regardless of how your configuration looks like, you can set the relevant HTTP headers in any Apache config block, i.e. `<VirtualHost>`, `<Directory>`, `<Location>`, and `<Files>`.

There are a few CORS related HTTP headers which you can return in the response:

    Access-Control-Allow-Origin

    Access-Control-Allow-Credentials
    Access-Control-Allow-Methods
    Access-Control-Max-Age
    Access-Control-Allow-Headers
    Access-Control-Expose-Headers

Some of the above are required for "preflight" requests. Some HTTP clients (namely, modern browsers) perform a request **before** your desired request just to see if they have authorisation to make the actual request on the server. See https://en.wikipedia.org/wiki/Cross-origin_resource_sharing for more on the preflight request.

The main HTTP header we need is `Access-Control-Allow-Origin` and that's we're going to set. However, the same principle applies pretty much to all of them (you just need to know what to return).

The following example sets the required HTTP header within a `<Directory>` config block to enable an SSL protected client Full Qualified Domain Name (FQDN):

    <Directory /path/to/your/site/>
            Header set Access-Control-Allow-Origin "https://my.CLIENT.domain"
    </Directory>

After we've set this on the **server**, we  can now perform a request from https://my.client.domain to our server and it should respond.

Note: A lot of people use `Access-Control-Allow-Origin: "*"` which is a wildcard, to mean requests from **ALL** domains should be accepted. This is usually ill-advised unless you're running some sort of a public API or repository of files. Also, please note the context of you HTTP header setting. You might want to allow HTTP requests for an API, but not for "hotlinking" images etc. You can set this header anywhere you want within your Apache config flow to **only** set it in specific situations. For example, the following would **only** set the CORS HTTP header when the requested path is **not** a file or directory (suits a public API which disallows image hotlinking):

    <Directory /path/to/your/site/>
        Options +FollowSymlinks
        Options +Indexes
        RewriteEngine On

        #Make sure it's not a specific file or directory that they're trying to reach
        RewriteCond %{SCRIPT_FILENAME} !-f
        RewriteCond %{SCRIPT_FILENAME} !-d
        Header set Access-Control-Allow-Origin "*"
        RewriteRule ^(.*)$ index.php/$1 [L]
    </Directory>

> ## Prerequisites ##
> You've got to have [mod_headers](http://httpd.apache.org/docs/2.0/mod/mod_headers.html) installed and enabled: `a2enmod headers`

## 301 Redirection by Htaccess
The HTTP response status code 301 Moved Permanently is used for permanent URL redirection, meaning current links or records using the URL that the response is received for should be updated. The new URL should be provided in the Location field included with the response. The 301 redirect is considered a best practice for upgrading users from HTTP to HTTPS.
write this code in htaccess file for PHP-APACHE

    Redirect 301 /oldpage/ /newpage/
    
Here is an example using an htaccess file to redirect to a non www with an SSL attached to the domain.

    RewriteEngine On
    RewriteCond %{HTTPS} off
    RewriteCond %{HTTP_HOST} ^www\.(.*)$ [NC]
    RewriteRule ^(.*)$ http://%1/$1 [R=301,L]
    
    RewriteCond %{HTTPS} on
    RewriteCond %{HTTP_HOST} ^www\.(.*)$ [NC]
    RewriteRule ^(.*)$ https://%1/$1 [R=301,L]
    
    RewriteEngine On
    RewriteCond %{SERVER_PORT} 80
    RewriteRule ^(.*)$ https://example.com/$1 [R,L] 

