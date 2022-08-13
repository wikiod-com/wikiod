---
title: "Denying Access"
slug: "denying-access"
draft: false
images: []
weight: 9992
type: docs
toc: true
---

## Denying IPs
<!-- language-all: lang-bash -->

    order allow,deny
    deny from 255.0.0.0
    allow from all

This denies access to the IP `255.0.0.0`.

    order allow,deny
    deny from 123.45.6.
    allow from all

This denies access to all IPs in the range `123.45.6.0` to `123.45.6.255`.


## Hot Link Prevention
<!-- language-all: lang-bash -->

    RewriteEngine on
    RewriteCond %{HTTP_REFERER} !^$
    RewriteCond %{HTTP_REFERER} !^http://(www\.)?yourdomain.com/.*$ [NC]
    RewriteRule \.(gif|jpg|css)$ - [F]

This blocks all the links to '.gif', '.jpg' and '.css' files which are not from the domain name `http://www.yourdomain.com`.

**Display alternate content:**

    RewriteEngine on
    RewriteCond %{HTTP_REFERER} !^$
    RewriteCond %{HTTP_REFERER} !^http://(www\.)?yourdomain.com/.*$ [NC]
    RewriteRule \.(gif|jpg)$ http://www.yourdomain.com/angryman.jpg [R,L]

This blocks all links to '.gif' and '.jpg' files which are not from the domain name 'http://www.yourdomain.com/' and displays the file 'http://www.yourdomain.com/angryman.jpg' instead.

## Denying access from IPs to files/directories
<!-- language-all: lang-bash -->

    # Deny access to a directory from the IP 255.0.0.0
    <Directory /path/to/directory>
        order allow,deny
        deny from 255.0.0.0
        allow from all
    </Directory>

    # Deny access to a file from the IP 255.0.0.0
    <FilesMatch "^\.ht">
        order allow,deny
        deny from 255.0.0.0
        allow from all
    </FilesMatch>

