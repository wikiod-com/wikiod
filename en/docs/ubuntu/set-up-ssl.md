---
title: "Set up SSL"
slug: "set-up-ssl"
draft: false
images: []
weight: 9992
type: docs
toc: true
---

----------

   **[a2ensite][1]** - a script that enables the specified site (which contains a
       <VirtualHost> block) within the apache2 configuration by
       creating symlinks within /etc/apache2/sites-enabled 


----------
   **[apache2][2]** - popular web server.  Alternative web servers are tomcat, nginx, etc.

----------
   **[openssl][3]** - SSL works by using a private key to encrypt data transferred over an SSL-enabled connection, thus thwarting eavesdropping of information. Use openssl to generate keys and certificates.

----------
   **[vim][4]** - a popular text editor


  [1]: http://man.he.net/man8/a2ensite
  [2]: https://help.ubuntu.com/lts/serverguide/httpd.html
  [3]: https://help.ubuntu.com/community/OpenSSL
  [4]: https://help.ubuntu.com/community/VimHowto

## Set up ssl for local testing in apache
Enable the module by typing:

    sudo a2enmod ssl

Restart the web server (apache2) so the change is recognized:

    sudo service apache2 restart

Optional (but a good idea): Create a directory that will contain our new certificate files:

    sudo mkdir /etc/apache2/ssl

Create the key and self-signed certificate in that directory:

    sudo openssl req -x509 -nodes -days 365 -newkey rsa:2048 -keyout /etc/apache2/ssl/your_domain.key -out /etc/apache2/ssl/your_domain.crt

You will be asked a series of questions for your security certificate. Answer each one as you are prompted, but with your own company's information.  Here is an example:

    Country Name (2 letter code) [AU]:US 
    State or Province Name (full name) [Some-State]:Pennsylvania 
    Locality Name (eg, city) []:Philadelphia 
    Organization Name (eg, company) [Internet Widgits Pty Ltd]:Cool Company
    Organizational Unit Name (eg, section) []:IT
    Common Name (e.g. server FQDN or YOUR name) []:test_domain.com
    Email Address []:my_email@test_domain.com

Open the file with root privileges:

    sudo vim /etc/apache2/sites-available/default-ssl.conf

Configure your virtual host by adding your server details and certificate locations to the default-ssl.conf file:

    <IfModule mod_ssl.c>
        <VirtualHost _default_:443>
            ServerAdmin admin@example.com
            ServerName your_domain.com
            ServerAlias www.your_domain.com
            DocumentRoot /var/www/html
            ErrorLog ${APACHE_LOG_DIR}/error.log
            CustomLog ${APACHE_LOG_DIR}/access.log combined
            SSLEngine on
            SSLCertificateFile /etc/apache2/ssl/your_domain.crt
            SSLCertificateKeyFile /etc/apache2/ssl/your_domain.key
            <FilesMatch "\.(cgi|shtml|phtml|php)$">
                SSLOptions +StdEnvVars
            </FilesMatch>
            <Directory /var/www/html>
                SSLOptions +StdEnvVars
                DirectoryIndex index.php
                AllowOverride All
                Order allow,deny
                Allow from all
            </Directory>
            BrowserMatch "MSIE [2-6]" \
                            nokeepalive ssl-unclean-shutdown \
                            downgrade-1.0 force-response-1.0
            BrowserMatch "MSIE [17-9]" ssl-unclean-shutdown
        </VirtualHost>
    </IfModule>

Save your changes and enter the following to enable your new ssl configuration:

    sudo a2ensite default-ssl.conf

To activate the new configuration, run:
  

    service apache2 reload

Now, attempt to access your site locally using https!

