---
title: "How to create virtual host in Apache"
slug: "how-to-create-virtual-host-in-apache"
draft: false
images: []
weight: 9967
type: docs
toc: true
---

The main entry point for Apache's `VirtualHost` is at [Apache Virtual Host documentation](https://httpd.apache.org/docs/current/en/vhosts/). From there, you have general documentation about virtual host configuration, and reference documentation about `VirtualHost` and related directives as well.

## 1) IP based vhosts  2) Multiple vhosts with the same Port  3) Defining vhosts using Macro (Apache2.4)
**1) IP based vhosts**

 
     <VirtualHost 192.168.13.37>
         ServerName example.com
         DocumentRoot /var/www/domains/example.com/html
         ErrorLog /var/log/example.com/error.log
         CustomLog /var/log/example.com/access.log common
     </VirtualHost>

     <VirtualHost 192.168.47.11>
         ServerName otherurl.com
         DocumentRoot /srv/www/htdocs/otherurl.com/html
         ErrorLog /var/log/otherurl.com/error.log
         CustomLog /var/log/otherurl.com/access.log common
     </VirtualHost>

Just change the port to your given IP(s). The port is irrelevant for the decision which vhost is chosen.

**2) Multiple vhosts with the same Port**

Since NameVirtualHost isn't needed anymore you can just write multiple vhosts with the same port.

    <VirtualHost *:80>
         DocumentRoot /srv/www/htdocs/otherurl.com/html
         ErrorLog /var/log/otherurl.com/error.log
         CustomLog /var/log/otherurl.com/access.log common
    </VirtualHost>

    <VirtualHost *:80>
         ServerName example.com
         ServerAlias ex1.com ex2.com
         DocumentRoot /var/www/domains/example.com/html
         ErrorLog /var/log/example.com/error.log
         CustomLog /var/log/example.com/access.log common
     </VirtualHost>

Here the opposite applies: the IP is irrelevant, but if the request is received on port 80 the name you entered is evaluated. Did you call ex1.com the 2nd vhost gets picked. And if you called any other url (like otherurl.com, but also example3.com) the first one will be picked. You can use this vhost as a 'fallback' if you will.

**3) Defining vhosts using Macro (Apache2.4)**

    <Macro VHost $port $host>
        <VirtualHost *:$port>
            Servername $host
            DocumentRoot /srv/www/htdocs/$host
            ErrorLog /var/log/$host/error.log
        </VirtualHost>
    </Macro>

    Use VHost 80 example.com
    Use VHost 443 secure_example.com

Creates two vhosts, one for port 80, one for 443, and sets the used variables accordingly.

## Name-based virtual host configuration
Name-based virtual hosting on Apache is described on the [Apache website][1] as such:

> With name-based virtual hosting, the server relies on the client to
> report the hostname as part of the HTTP headers. Using this technique,
> many different hosts can share the same IP address.

Therefore, more than one website can be hosted on one server through this method.  On ubuntu, the configuration files are in /etc/apache2/sites-available.  In that directory, you will find 000-default.conf.  That is the default configuration, all requests will be sent to this configuration file until others have been set up. 

To set up a virtual host, here **example.com** will be used, but you should replace it with your **domain.com**. Copy the default file:

     cp 000-default.conf example.com.conf

The configuration file can have the following directives:

     <VirtualHost *:80>
            ServerAdmin admin@example.com
            ServerName example.com
            ServerAlias www.example.com
    
            DocumentRoot /var/www/example.com/html
            
            ErrorLog /var/log/apache/logs/error.log
            # Possible values include: debug, info, notice, warn, error, crit,
            # alert, emerg.
            LogLevel warn
    
            CustomLog /var/log/apache/logs/access.log combined    
    </VirtualHost>


* The first line, indicates that all requests on port 80 (default http port) should be matched. You can also have a IP address instead of * which is the IP of the server.
* `ServerAdmin` is the contact details of website admin used for displaying with http error messages. 
* `ServerName` is the domain name of website. 
* `ServerAlias` is a secondary name of the website, usually will be www.domain.com
* `DocumentRoot` is the root folder loaded when we browse a website. 
* `ErrorLog` is the file in where errors are directed
* `LogLevel`. is the level of errors to be sent to the log
* `CustomLog` is the file where access information is directed

Edit the file replacing example.com with your website domain name and appropriate directory for the website files.

Save the file and enable the site with the following Apache command:

    sudo a2ensite example.com.conf

Reload apache

    sudo service apache2 reload

A few more things that must be checked:

* Ensure your DNS for your domain is set up for the correct IP (this may take time to propogate)
* Ensure your port 80 is open on the firewall
* Ensure your file permissions are setup correctly on the server files - ownership should be www-data:www-data and directory permissions should be 750 and file permissions should be 640. 

Your virtual host should be up and running!  You can repeat this for other websites on the same server, with a different configuration file (using the same naming convention) and different directories under /var/www.

  [1]: https://httpd.apache.org/docs/2.4/vhosts/name-based.html

## Virtual Host In WAMP


## Force HTTPS using virtual host
Use **Redirect** to force users to connect to the secure URL.

    <VirtualHost *:80>
        ServerName example.com
        SSLProxyEngine on
        Redirect permanent / https://secure_example.com/
    </VirtualHost>

The rest of the configuration can be put in the ssl virtual host (port 443) since everything is redirected.

    <VirtualHost _default_:443>
        ServerName secure_example.com
        ServerAdmin webmaster@example.com 
        DocumentRoot /var/www/domains/secure_example.com/html
        ErrorLog /var/log/secure_example.com/error.log
        CustomLog /var/log/secure_example.com/access.log common
        SSLEngine On
        ...
    </VirtualHost>
       

## PHP Development Virtual Host
This is an example on how to control PHP error logging in a virtual host site for development and debugging.
Assumptions

 - The PHP module has been installed.
 - Development environment is not for production.

  
    <VirtualHost *:80>
        ServerName example.com
        DocumentRoot /var/www/domains/example.com/html
        ErrorLog /var/www/domains/example.com/apache.error.log
        CustomLog /var/www/domains/example.com/apache.access.log common
        php_flag log_errors on
        php_flag display_errors on
        php_value error_reporting 2147483647
        php_value error_log /var/www/domains/example.com/php.error.log
    </VirtualHost>

**Note:** The Virtual Host configuration is for development only because the display_errors is enabled and you do not want that in production.

