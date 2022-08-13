---
title: "Getting started with https"
slug: "getting-started-with-https"
draft: false
images: []
weight: 1
type: docs
toc: true
---

## Getting started with HTTPS
HTTPS *(Hypertext Transfer Protocol Secure)* is an encrypted version of HTTP protocol, most often used in connection with services where only the sender and receiver must know the message. It's required if you handle credit card information, and will improve your rank on Google.

To enable [HTTPS][1] you need to check if your web host supports it—if you don't know you can ask their support for help and information about it. Some web hosts may take some money for it.

>**Important!**: Your HTTPS needs to use [sha2][2] or [sha3][3] *([sha1][4] is blocked by **Chrome**, **Firefox**, **Edge** and **IE**)*

---
 
When you have enabled HTTPS on your web host, you can use HTTPS. But the browser does not use HTTPS as default; the best way to make sure that all traffic runs on HTTPS is by using a [.htaccess][5] file and adding it to the root of your website.

**The .htaccess file**

<!-- language: lang-bsh -->

    RewriteEngine On

    # If we receive a forwarded http request from a proxy...
    RewriteCond %{HTTP:X-Forwarded-Proto} =http [OR]

    # ...or just a plain old http request directly from the client
    RewriteCond %{HTTP:X-Forwarded-Proto} =""
    RewriteCond %{HTTPS} !=on

    # Redirect to https version
    RewriteRule ^ https://%{HTTP_HOST}%{REQUEST_URI} [L,R=301]


This will change the http:// to https://.


> **NOTE**: .htaccess is a system file, and can't be seen by default. [How to show .htaccess][6]





  [1]: https://da.wikipedia.org/wiki/HTTPS
  [2]: https://en.wikipedia.org/wiki/SHA-2
  [3]: https://en.wikipedia.org/wiki/SHA-3
  [4]: https://en.wikipedia.org/wiki/SHA-1
  [5]: http://stackoverflow.com/questions/13170819/what-is-htaccess-file
  [6]: http://www.temi.co.uk/how-to-view-htaccess-file-and-other-hidden-files-on-windows-pc/

