---
title: "Installation and Configuration"
slug: "installation-and-configuration"
draft: false
images: []
weight: 9992
type: docs
toc: true
---

## Wordpress on LAMP
I have tested the following steps on Amazon EC2 with Ubuntu 14.04

Here is a summary of command lines to install LAMP technology stack (before installing wordpress):

**1: Install LAMP technology stack**

**Update linux**

`#> sudo apt-get update`    ->  update package repositories information

`#> sudo apt-get upgrade`   ->  install package upgrades

**Install apache2**

`#> sudo apt-get install apache2`

**Install php**

`#> sudo apt-get install php5 libapache2-mod-php5 php5-mcrypt`

**Install mysql**

`#> sudo apt-get install mysql-server php5-mysql`

**Install phpmyadmin**

`#> sudo apt-get install phpmyadmin`

*In case thing went wrong, remove installed package and its configuration file(s)*

`#> sudo apt-get purge package_name`

Once the stack is installed here is the steps to complete wordpress installation:

**2: Install WordPress**

1. `wget https://wordpress.org/latest.zip`

2. `sudo mkdir /var/www/wordpress`

3. Move zip file into /var/www/wordpress and extract.

4. `cd /etc/apache2/sites-available/` 

5. `sudo cp 000-default.conf wordpress.conf`

6. Modify wordpress.conf so apache2 knows to forward any request or your domain to the wordpress app folder.

7. `sudo a2ensite wordpress.conf`

8. `sudo service apache2 restart`

9. Go to PhpMyAdmin via your browser by youdomain.com/phpmyadmin .
Create a new user “wordpress” and check to create the corresponding database.
`cd /var/www/wordpress`

10. `sudo cp wp-config-example.php wp-config.php`

11. Modify config file to add your MySQL wordpress database information.

12. `sudo chown -R www-data:www-data /var/www/wordpress`

13. `sudo chmod -R 755 /var/www/wordpress/`

14. Open browser and type in your domain. You should see the WordPress installation page. Follow the instruction and you are done!

## Installation WP in MAMP
It's quite simple to install wordpress in MAMP.

You have to follow a few simple steps:

1 - Download MAMP from [here][1], it's free and you probably don't need the pro version.

2 - Install on your PC or Mac.

3 - Run the program -> you will see a little window open, from there you can set MAMP. At the moment leave all the pre-set values, for the first installation you don't need to complicate your life! In the future remember it's good practice to change your password and user name for the MySQL database. By default it's root.

4 - Click on "Open webStart page" - here you can see your data info like password, admin name and also info about MAMP.

5 - Click on tools -> phpMyAdmin and you will be redirected to the database page.

6 - Make a new database, click on new and give it the name you want, you will need this name later.

7 - Now look for a folder call "htdocs", it's inside the MAMP folder on your PC or Mac. If you use a Mac you need to one the application in Finder and open the app using "show package contents". Inside you will find the `htdocs` folder.

8 - Take the Wordpress folder and copy it inside the htdocs, you need put inside all folder, no put the zip file. Rename the folder, you can call with your project name.

9 - Comeback to the window of MAMP in your browser, and click on "My Site" - this will open a URL call `http://localhost:8888` (8888 is the default port, you can change it if you want). Here you can see the folder you have put inside the `htdocs` folder, click on the name you have given to the folder.

10 - Now start a normal installation of WordPress, you need to use the Admin and Password. By default it's `root` and `root`, and use the name of the database you have created before.

11 - Run the installation and finish!


You will see your website on `http://localhost:8888/namefolder`
Of course you need keep MAMP running.  

  [1]: https://www.mamp.info/en/

