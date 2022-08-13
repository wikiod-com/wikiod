---
title: "Contexts of rewrite rules"
slug: "contexts-of-rewrite-rules"
draft: false
images: []
weight: 9997
type: docs
toc: true
---

This topic describes the two contexts in which `RewriteRule` can be used. In examples omitting `RewriteEngine on`, it is assumed this directive has occured before that example.

## Rewrite rules in per-directory context
The per-directory context is a part of the static configuration file between `<Directory>` and `</Directory>` tags. The entire content of dynamic configuration files is within the per-directory context of the folder in which the .htaccess resides.

`RewriteRule`'s in per-directory context match against the part of an url after the protocol, hostname, port and prefix of the directory in which they reside, and before the query string.

**In the static configuration file**

When the following rule is used on the url `http://example.com/foo?id=1`, the regex in the first argument of `RewriteRule` is matched against `foo`. The protocol (`http`), hostname (`example.com`) and prefix for this directory (`/`) are removed. At the other end, the query string (`?id=1`) is also removed.

    <Directory "/">
      RewriteRule ^foo$ bar [L]
    </Directory>

In the following example, using the url `http://example.com/topic/15-my-topic-name`, the first argument of `RewriteRule` would be matched against `topic/15-my-topic-name`:

    <Directory "/topic/">
      RewriteRule ^topic/([0-9]+)-[^/]*/?$ topics.php?id=$1 [L]
    </Directory>

**In the dynamic configuration file**

When the following rule is placed in a `.htaccess` file that is in the www-root folder and then used on the url `http://example.com/foo?id=1`, the first regex is matched against `foo`.

    RewriteRule ^foo$ bar [L]

> In per-directory context, the matched url **never** starts with a `/`. In such a context, a directive starting with `RewriteRule ^/` will never match anything.

## Rewrite rules in virtual host context
The virtual host context is a part of the static configuration file between `<VirtualHost>` and `</VirtualHost>` tags.

`RewriteRule`'s in virtual host context match against the part of an url after the protocol, hostname and port, and before the query string.

When the following rule is used for the url `http://example.com/foo?id=1`, the regex in the first argument of `RewriteRule` is matched against `/foo`.

    <VirtualHost 1.2.3.4:80>
      ServerName example.com

      RewriteEngine on
      RewriteRule ^/foo$ /bar [L]
    </VirtualHost>

