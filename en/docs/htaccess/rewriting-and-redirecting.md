---
title: "Rewriting and Redirecting"
slug: "rewriting-and-redirecting"
draft: false
images: []
weight: 9948
type: docs
toc: true
---

Before URLs can be rewritten, a module called `mod_rewrite.c` needs to be enabled. Usually, it is disabled in the configuration by default.

`mod_rewrite` can be enabled by executing the command

    $ sudo a2enmod mod_rewrite
    $ sudo service apache2 restart

or by commenting out the lines

    #LoadModule rewrite_module modules/mod_rewrite.so
    #AddModule mod_rewrite.c

in `httpd.conf` file.


## Popular Rewrite Flags
# F|forbidden

Similar to `Deny`, this flag forces the server to immediately return a *403 Forbidden* status code to the requesting browser or client for the request.

Example: Deny access to requests that end with `exe`:

<!-- language: lang-bash -->

    RewriteRule .exe$ - [F]

# G|gone

If a requested resource was available in the past, but is no longer available, you can use this flag to force the server to immediately return a *410 Gone* status code to the requesting browser or client for the request.

Example: Tell a visitor that an old product no longer exists:

<!-- language: lang-bash -->

    RewriteRule ^old-product.html$ - [G]

# L|last

In most contexts, other than `.htaccess`, this flag instructs [`mod_rewrite`](https://httpd.apache.org/docs/current/mod/mod_rewrite.html) to stop processing the current condition/rule set, much the same way `last` and `break` (Perl and C, respectively) do.

However, in the `.htaccess` or `<Directory>` context, a request that has been rewritten using a `RewriteRule` with this flag will be passed back to the URL parsing engine for further processing. As such, it is possible, for the rewritten URI to be handled by the same context, and perhaps altered further.

A general recommendation is to use the `END` flag to not only stop processing the current condition/rule set, but also to prevent any further rewriting in these contexts.

Note: The `F` and `G` flags, discussed above, both use `L` implicitly, so you do not need to specify them separately.

# N|next

This flag will re-run the rewriting process from the beginning, starting again with the first condition/rule set. This time, the URL to match is no longer the original URI, but rather the rewritten URI returned by the last rule set. Use this flag to restart the rewriting process.

**A word of warning:** Use this flag with caution, as it may result in an infinite-loop!

# NC|nocase

This instructs `mod_rewrite` to match the `Pattern` of a `RewriteRule` without being case-sensitive. To clarify, `MyIndex.html` and `myindex.html` would be regarded by the module as the same thing. Further, this flag allows you to use `a-z` instead of `A-Za-z` in a regular expression.

# R|redirect

This flag is used to send an HTTP redirect response to the requesting browser/client.

By default, if no code is given, a redirect response with the *302 Found* (similar to a temporary redirect) status code will be returned. If you wish to use a more permanent redirect, then you should use the `302` (*301 Moved Permanently*) status code.

Generally, only status codes in the range 300-399 should be used with this flag. If status codes outside of this range are used (which is perfectly acceptable), then the substitution string is discarded and rewriting is stopped as if the `L` flag were used. In some cases, this is a handy way to force *404 Not Found* responses, even if the request points to an existing resource.

Example: Issue a *302 Found* redirect response:

<!-- language: lang-bash -->

    RewriteRule ^bus$ /train [R,L]

Example: Issue a *301 Moved Permanently* redirect response:

<!-- language: lang-bash -->

    RewriteRule ^speed-train$ /hyperloop [R=301,L]

Example: Force a *404 Not Found*:

<!-- language: lang-bash -->

    RewriteRule ^blip$ - [R=404,L]

## Redirect with/without query params
Redirect without query params:
    
    RewriteRule ^route$ /new_route_without_query [L,R=301,QSD]

Redirect with query params:

    RewriteCond %{QUERY_STRING} ^$
    RewriteRule ^/?route$ %{REQUEST_URI}?query=param1&query2=param2 [NC,L,R=301]

## www and non-www redirects
<!-- language-all: lang-bash -->

Redirect any naked domain to `www.[your_domain].tld`:

    # Start Apache Rewriting engine
    RewriteEngine On
    # Make sure you're not already using www subdomain
    # and that the host string is not empty
    RewriteCond %{HTTP_HOST} !^$
    RewriteCond %{HTTP_HOST} !^www\.
    # We check for http/https connection protocol
    RewriteCond %{HTTPS}s ^on(s)|
    # In case the previous conditions matches, redirect to www
    RewriteRule ^(.*)$ http%1://www.%{HTTP_HOST}/$1 [R=301,L]

---

Redirect `www.[your_domain].tld` to `[your_domain].tld`

    # Start Apache Rewriting engine
    RewriteEngine On
    # We check if we're on the www subdomain
    RewriteCond %{HTTP_HOST} ^www\.([^\.]+\.[^\.]+)$
    # In case the previous condition matches, redirect to non-www
    RewriteRule ^(.*)$ http://%1/$1 [R=301,L]

---

Redirect any level of nested subdomains to your main domain:

    # Start Apache Rewriting engine
    RewriteEngine On
    # We check if there's a subdomain
    RewriteCond %{HTTP_HOST} \.([^.]+\.[^.]+)$
    # redirect to the main domain name
    RewriteRule ^ http://%1%{REQUEST_URI} [R=301,L]

## SEO Friendly URLs
<!-- language-all: lang-bash -->

Search engines won't index your products if you have a URL like the following:

    http://www.yourdomain.com/product.php?id=123

SEO friendly URL would look like `http://www.yourdomain.com/123/product-name/`. The following code helps achieve this without having to change `product.php` code.

    RewriteEngine On
    RewriteRule ^product/([0-9]+)/product-name-slug/?$ product.php?id=$1

## Adding a trailing slash at the end
<!-- language-all: lang-bash -->

    RewriteCond %{REQUEST_FILENAME} !-f
    RewriteCond %{REQUEST_URI} !(.*)/$
    RewriteRule ^(.*)$ /$1/ [L,R=301]

The first `RewriteCond` helps exclude the files. The second `RewriteCond` checks if there is already a trailing slash. If the case is so `RewriteRule` is not applied.

If you have any URL that shouldn't be rewritten, you can add one more `RewriteCond`.

    RewriteCond %{REQUEST_FILENAME} !-f
    RewriteCond %{REQUEST_URI} !(.*)/$
    RewriteCond %{REQUEST_URI} !url/to/not/rewrite
    RewriteRule ^(.*)$ /$1/ [L,R=301]


## http and https redirects and HSTS configuration
<!-- language-all: lang-bash -->

## Generic redirect __*to*__ *https*:

    # Enable Rewrite engine
    RewriteEngine on
    
    # Check if URL does not contain https
    RewriteCond %{HTTPS} off [NC]
    # If condition is true, redirect to https
    RewriteRule (.*) https://%{SERVER_NAME}/$1 [R=301,L]

## Generic redirect __*to*__ *http*:

    # Enable Rewrite engine
    RewriteEngine on
    
    # Check if URL does contain https
    RewriteCond %{HTTPS} on [NC]
    # If condition is true, redirect to http
    RewriteRule (.*) http://%{SERVER_NAME}/$1 [R=301,L]

## Forcing HTTPS connection (HSTS):

    <IfModule mod_headers.c>
        Header always set Strict-Transport-Security "max-age=31536000; includeSubDomains"
    </IfModule>
where, the `includeSubDomains` option can be removed, if HSTS should be applied only to the base domain, or the domain with the above configuration.

