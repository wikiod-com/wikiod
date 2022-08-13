---
title: "General Security and Hack Prevention"
slug: "general-security-and-hack-prevention"
draft: false
images: []
weight: 9977
type: docs
toc: true
---

.htaccess redirection is a common vector for malicious hackers to exploit and infect websites. We have seen what .htaccess files are, how they are used by malicious hackers, and how to protect your website.

## Hack Prevention
<!-- language-all: lang-bash -->

## Prevent access to your `.htaccess` file

    <Files .htaccess>
    order allow,deny
    deny from all
    </Files>
    
    # Rename the file
    AccessFileName thehtfile.ess

## Prevent URL attacks

    # Enable rewrites
    RewriteEngine On
    
    # Block <script> tags from executing in the URL
    RewriteCond %{QUERY_STRING} (<|%3C).*script.*(>|%3E) [NC,OR]

    # Block scripts from setting a PHP Globals variable
    RewriteCond %{QUERY_STRING} GLOBALS(=|[|\%[0-9A-Z]{0,2}) [OR]
    
    # Block scripts from using base64_encode
    RewriteCond %{QUERY_STRING} base64_encode.*(.*) [OR]
    
    # Block scripts from using the a_REQUEST variable
    RewriteCond %{QUERY_STRING} _REQUEST(=|[|\%[0-9A-Z]{0,2})

## Disable use of scripts on your directories..

    AddHandler cgi-script .php .pl .py .jsp .asp .htm .shtml .sh .cgi
    Options -ExecCGI

## Disable directory index
Enabled directory index means that if someone access to any folder which don't contains index.php , index.html, index.htm or any other default file defined in DirectoryIndex in apache configuration then all files in that folder will be listed in browser if you try to visit that page.  

Often directory index is enabled by default on your apache server, in these cases good security practice is to disable directory index with following line:

    Options -Indexes



