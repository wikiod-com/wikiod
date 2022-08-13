---
title: "Getting started with magento"
slug: "getting-started-with-magento"
draft: false
images: []
weight: 1
type: docs
toc: true
---

## Installation and Setup
Prerequisites and Requirements for Magento Community Edition 1.9
================================================================

**Hosting**


- Apache 2.x ( with mod_rewrite ) or Nginx 1.7.x

- Due to the demands of processing Magento operations, it is recommended that you install Magento on a server with at least 2 GB of RAM. This will ensure that all of the software involved in managing the store will have enough memory to work.

- Ability to run scheduled jobs (crontab) with PHP 5.

- Ability to override options in .htaccess files.

**PHP**

- PHP 5.4, PHP 5.5
- Required extensions: PDO_MySQL, simplexml, mcrypt, hash, GD, DOM, iconv, curl, 
SOAP (for Webservices API)

- memory_limit no less than 256 MB (512 MB recommended)

**Database**

- MySQL 5.6 (Oracle, Percona, MariaDB)

**SSL**
- A valid security certificate is required for HTTPS.
- Self-signed SSL certificates are not supported


Installation:
=============

**Download and Set Up Magento Files**

We are using openMage mirror as direct download for 1.9.2.4 branch is disabled and magento website require account. But you are encouraged to download copy from https://www.magentocommerce.com/download

    cd /var/www/html
    wget https://github.com/OpenMage/magento-mirror/archive/magento-1.9.zip
    unzip magento-1.9.zip
    rm magento-1.9.zip
    rsync -avP magento-mirror-magento-1.9/. .
    rm magento-mirror-magento-1.9 -r
    sudo chown -R www-data:www-data /var/www/html/
    chmod -R 0777 media var

**Create a MySQL Database and User**
  
   access mysql console

    mysql -u root -p
    
in mysql console

    CREATE DATABASE magento;
    CREATE USER magento_db_user@localhost IDENTIFIED BY 'password';
    GRANT ALL PRIVILEGES ON magento.* TO magento_db_user@localhost IDENTIFIED BY 'password';
    FLUSH PRIVILEGES;
    exit

**Complete the installation through the web interface**

To access the web interface with your browser, navigate to your server's domain name or public IP address:

    http://domain_name/

Then follow on screen instructions






    

## Troubleshooting Common Problems
**Only the homepage works, all other pages return 404**

Make sure `mod_rewrite` module has been installed in Apache and been enabled to load. 
See **step 2** for info on how to do this here: https://www.digitalocean.com/community/tutorials/how-to-set-up-mod_rewrite-for-apache-on-ubuntu-14-04

Make sure the you allow changes in the .htaccess by enabling it in your site conf. See **step 3**: https://www.digitalocean.com/community/tutorials/how-to-set-up-mod_rewrite-for-apache-on-ubuntu-14-04

Your `.htaccess` file may be mis-configured or missing: head over to Magento's download page: https://www.magentocommerce.com/download - download the relevant version and extract the .htaccess file to be placed in your Magento installation root.

**Site works but not styles or scripts are loading**

Make sure you have set the relevant permissions and ownership: **See here** for more info - http://devdocs.magento.com/guides/m1x/install/installer-privileges_before.html 

Common solution: Try reindexing and flushing cache manually (in case the admin is too hard to navigate). 
Reindex via the command line: https://www.atwix.com/magento/process-magento-indexes-from-command-line/ 
Flush cache (via admin or command line): https://www.properhost.com/support/kb/23/How-To-Clear-The-Magento-Cache  

**Followed**

