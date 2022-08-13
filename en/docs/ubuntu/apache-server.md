---
title: "Apache Server"
slug: "apache-server"
draft: false
images: []
weight: 9983
type: docs
toc: true
---

## Php 5.6 with Apache 2 Server on Ubuntu 16.04
This tutorial will guide you through the process from scratch.
Please note some preliminary notes about this particular setup, useful in case that you already have some requested package:

 - Is needed a version of php >=5.0 (I had troubles with php 7.0)
 - Is requested any version of perl
 - Is needed a version of Apache that supports php scripting


    sudo apt-get update
    sudo apt-get install perl apache2 zip gzip tar
    sudo add-apt-repository ppa:ondrej/php
    sudo apt update
    sudo apt install php5.6 libapache2-mod-php5.6 php5.6-curl php5.6-gd php5.6-mbstring php5.6-mcrypt php5.6-mysql php5.6-xml php5.6-xmlrpc

If you need to disable php 7.0 (or any different version) before proceeding, do the following:

    a2dismod php7.0

Then proceed with:

    a2enmod php5.6


That last command should give 'enabled' or 'already enabled' as output!

If you are installing it under a virtual machine on VMware, please do the following:

    sudo apt-get install open-vm-tools

Now restart Apache:

    service apache2 restart

Now Apache is installed and configured for php.
Try to open a Browser and type `localhost`, it should display the test page for Apache.


To change the timezone for php, open the following file

    sudo vim /etc/php/5.6/apache2/php.ini

And edit as following, uncommenting and editing with the timezone that you prefer:

    date.timezone = Europe/Rome

**Extra step 1**

If you are installing a tarball (tar.gz) on Apache, unzip it into `/var/www/html` (it's the default root location)

    sudo cp tarball.tar.gz /var/www/html/
    cd /var/www/html/
    sudo tar -zxf tarball.tar.gz

**Extra step 2**

Please note that you may also need to change permissions to www-data, because default permissions are to root and this can give some writing issues.


    sudo chown -R www-data:www-data /var/www/

